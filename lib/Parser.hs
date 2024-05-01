module Parser where

import Control.Monad.Except
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.Either.Extra
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Maybe
import Data.Maybe.HT
import qualified Data.Text as Text
import qualified Data.Tuple.Ops as TupleOps
import Foreign.C (throwErrno)
import Term
import qualified Text.Regex as Regex

-- tokenizer
data Token
  = Lparen {lpDbg :: Dbg}
  | Rparen {rpDbg :: Dbg}
  | ToLambda {laDbg :: Dbg}
  | ToLet {toLetDbg :: Dbg}
  | ToBlock {toBlkDbg :: Dbg}
  | ToIf {toIfDbg :: Dbg}
  | ToTrue {toTDbg :: Dbg}
  | ToFalse {toFDbg :: Dbg}
  | ToInt {toInt :: Integer, toNDbg :: Dbg}
  | ToVar {toVName :: Text.Text, toVDbg :: Dbg}
  deriving (Eq)

instance Show Token where
  show (Lparen _) = "'('"
  show (Rparen _) = "')'"
  show (ToLambda _) = "\"\"lambda\""
  show (ToLet _) = "\"let\""
  show (ToBlock _) = "\"block\""
  show (ToIf _) = "\"if\""
  show (ToTrue _) = "#\"true\""
  show (ToFalse _) = "#\"false\""
  show (ToInt value _) = show value
  show (ToVar name _) = Text.unpack name

getTokenDbg :: Token -> Dbg
getTokenDbg (Lparen dbg) = dbg
getTokenDbg (Rparen dbg) = dbg
getTokenDbg (ToLambda dbg) = dbg
getTokenDbg (ToIf dbg) = dbg
getTokenDbg (ToTrue dbg) = dbg
getTokenDbg (ToFalse dbg) = dbg
getTokenDbg (ToInt _ dbg) = dbg
getTokenDbg (ToVar _ dbg) = dbg

isSpecialChar :: Char -> Bool
isSpecialChar char = char == '(' || char == ')'

-- TODO: add quit as a keyword
keywords :: Map.HashMap Text.Text (Dbg -> Token)
keywords =
  (Map.fromList . map (TupleOps.app1 Text.pack))
    [ ("lambda", ToLambda),
      ("let", ToLet),
      ("block", ToBlock),
      ("if", ToIf),
      ("#true", ToTrue),
      ("#false", ToFalse)
    ]

getTokenOfLexeme :: Text.Text -> Int -> Token
getTokenOfLexeme lexeme index =
  let dbg = Dbg index (index + Text.length lexeme)
   in case Regex.matchRegex (Regex.mkRegex "^-?[0-9]+$") (Text.unpack lexeme) of
        Just _ -> ToInt (read (Text.unpack lexeme)) dbg
        _ -> fromMaybe (ToVar lexeme) (Map.lookup lexeme keywords) dbg

type TokenizerType a = State (Text.Text, Int) a

skipSpaces :: TokenizerType ()
skipSpaces = do
  (input, index) <- get
  case Text.uncons input of
    Just (head, tail) | isSpace head -> put (tail, index + 1)
    _ -> return ()

getToken :: TokenizerType (Maybe Token)
getToken = do
  (input, index) <- get
  case Text.uncons input of
    Just ('(', rest) -> do put (rest, index + 1); return $ Just (Lparen (Dbg index (index + 1)))
    Just (')', rest) -> do put (rest, index + 1); return $ Just (Rparen (Dbg index (index + 1)))
    Just (char, _) | isSpace char -> do skipSpaces; getToken
    _ -> do
      let (lexeme, rest) = Text.break (\char -> char == '(' || char == ')' || isSpace char) input
      let length = Text.length lexeme
      put (rest, index + length)
      return $ toMaybe (length /= 0) (getTokenOfLexeme lexeme index)

tokenizeLoop :: TokenizerType [Token]
tokenizeLoop = do
  maybeToken <- getToken
  case maybeToken of
    Just token -> do
      rest <- tokenizeLoop
      return $ token : rest
    Nothing -> do return []

tokenize :: Text.Text -> [Token]
tokenize source = evalState tokenizeLoop (source, 0)

-- parser
data ParserState = ParserState {psTokens :: [Token], psContext :: [Text.Text], psSource :: Text.Text}

type ParserType = EitherT String (State ParserState)

getVarName :: Term -> Text.Text
getVarName (TFVar name _) = name
getVarName (TBVar _ name _) = name

parseError :: String -> ParserType a
parseError expected = do
  ParserState tokens _ input <- lift get
  let dbg = let len = Text.length input in Dbg len len
  let message = "Parsing error: expected " ++ expected
  case tokens of
    token : _ -> throwError (makeErrorString input dbg (message ++ " but got " ++ show token))
    [] -> throwError (makeErrorString input dbg (message ++ " but reached end of file"))

-- does not consume token
peek :: ParserType Token
peek = do
  ParserState tokens _ _ <- lift get
  case tokens of
    head : _ -> return head
    _ -> parseError "token"

setTokens :: [Token] -> ParserType ()
setTokens tokens = do lift $ modify (\s -> s {psTokens = tokens})

setContext :: [Text.Text] -> ParserType ()
setContext context = do lift $ modify (\s -> s {psContext = context})

parseRparen :: ParserType Int
parseRparen = do
  ParserState tokens _ _ <- lift get
  case tokens of
    Rparen (Dbg _ end) : rest -> do setTokens rest; return end
    _ -> parseError "')'"

-- <term> ::=
--     | <var>
--     | (lambda (<var>*) <term>)
--     | (<term> <term>*)
--     | (block (let <var> <term>)* <term>)
--     | #true
--     | #false
--     | (if <term> <term> <term>)
--     | <int>
--     | ()
parseTerm :: ParserType Term
parseTerm = do
  ParserState tokens _ input <- lift get
  case tokens of
    ToVar _ _ : _ -> parseVar
    ToTrue dbg : rest -> do setTokens rest; return $ TBool True dbg
    ToFalse dbg : rest -> do setTokens rest; return $ TBool False dbg
    ToInt value dbg : rest -> do setTokens rest; return $ TInt value dbg
    Lparen (Dbg start _) : Rparen (Dbg _ end) : rest -> do setTokens rest; return $ TUnit (Dbg start end)
    Lparen _ : ToLambda _ : _ -> parseLambda
    Lparen _ : ToBlock _ : _ -> parseBlock
    Lparen _ : ToIf _ : _ -> parseIf
    Lparen _ : _ -> parseApp
    token : _ ->
      let message = "Internal parsing error, could not match against any rules" ++ show tokens
       in throwError $ makeErrorString input (getTokenDbg token) message
    [] -> throwError "Internal parsing error, token list was empty"

-- <term>*)  does not consume the closing right parentheses
parseTerms :: ParserType [Term]
parseTerms = do
  ParserState tokens _ input <- lift get
  case tokens of
    Rparen _ : _ -> return []
    _ -> do result <- parseTerm; tail <- parseTerms; return $ result : tail

-- <var>
parseVar :: ParserType Term
parseVar = do
  ParserState tokens context input <- lift get
  case tokens of
    ToVar name dbg : rest -> do
      setTokens rest
      return $ case elemIndex name context of
        Just index -> TBVar index name dbg
        _ -> TFVar name dbg
    _ -> parseError "<var>"

-- <var>*)  does not consume the closing right parentheses
parseVars :: ParserType [Term]
parseVars = do
  ParserState tokens _ _ <- lift get
  case tokens of
    Rparen _ : _ -> return []
    _ -> do var <- parseVar; vars <- parseVars; return $ var : vars

-- (lambda (<var>*) <term>)
parseLambda :: ParserType Term
parseLambda = do
  ParserState tokens context input <- lift get
  case tokens of
    Lparen (Dbg start _) : ToLambda _ : Lparen _ : rest -> do
      setTokens rest
      vars <- parseVars
      let varNames = map getVarName vars
      parseRparen
      setContext (reverse varNames ++ context)
      body <- parseTerm
      setContext context
      TAbs (length vars) body [] varNames . Dbg start <$> parseRparen
    _ -> parseError "(lambda (<var>*) <term>)"

-- (<term> <term>*)
parseApp :: ParserType Term
parseApp = do
  state@(ParserState tokens _ input) <- lift get
  case tokens of
    Lparen (Dbg start _) : rest -> do
      setTokens rest
      fun <- parseTerm
      args <- parseTerms
      TApp fun args . Dbg start <$> parseRparen
    _ -> parseError "(<term> <term>*)"

-- (let <var> <term>)
parseLet :: ParserType Term
parseLet = do
  ParserState tokens context input <- lift get
  case tokens of
    Lparen (Dbg start _) : ToLet _ : rest -> do
      setTokens rest
      var <- parseVar
      value <- parseTerm
      -- the body is a dummy value intended to be replaced
      TLet value (TUnit emptyDbg) (getVarName var) . Dbg start <$> parseRparen
    _ -> parseError "(let <var> <term>)"

-- (let <var> <term>)* <term>)  does not consume the closing right parentheses
parseLets :: ParserType Term
parseLets = do
  ParserState tokens context _ <- lift get
  case tokens of
    Lparen _ : ToLet _ : _ -> do
      term <- parseLet
      setContext $ tLetName term : context
      result <- parseLets
      setContext context
      return $ term {tLetBody = result}
    _ -> do parseTerm

-- (block (let <var> <term>)* <term>)
parseBlock :: ParserType Term
parseBlock = do
  ParserState tokens _ input <- lift get
  case tokens of
    Lparen (Dbg start _) : ToBlock _ : rest -> do
      setTokens rest
      result <- parseLets
      parseRparen
      return result
    _ -> parseError "(block (let <var> <term>)* <term>)"

-- (if <term> <term> <term>)
parseIf :: ParserType Term
parseIf = do
  ParserState tokens context input <- lift get
  case tokens of
    Lparen (Dbg start _) : ToIf _ : rest -> do
      setTokens rest
      cond <- parseTerm
      cnsq <- parseTerm
      alt <- parseTerm
      TIf cond cnsq alt . Dbg start <$> parseRparen
    _ -> parseError "(if <term> <term> <term>)"