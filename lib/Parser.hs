{-# LANGUAGE Strict #-}

module Parser where

import Control.Monad.Except
import Control.Monad.Reader
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
  = TokLparen {tokLpDbg :: Dbg}
  | TokRparen {tokRpDbg :: Dbg}
  | TokLambda {tokLambdaDbg :: Dbg}
  | TokLet {tokLetDbg :: Dbg}
  | TokIf {tokIfDbg :: Dbg}
  | TokBool {tokBoolValue :: Bool, tokBoolDbg :: Dbg}
  | TokInt {tokIntValue :: Integer, tokIntDbg :: Dbg}
  | TokVar {tokVarName :: Text.Text, tokVarDbg :: Dbg}
  deriving (Eq)

instance Show Token where
  show (TokLparen _) = "'('"
  show (TokRparen _) = "')'"
  show (TokLambda _) = "\"\"lambda\""
  show (TokLet _) = "\"let\""
  show (TokIf _) = "\"if\""
  show (TokBool True _) = "#\"true\""
  show (TokBool False _) = "#\"false\""
  show (TokInt value _) = show value
  show (TokVar name _) = Text.unpack name

getTokenDbg :: Token -> Dbg
getTokenDbg (TokLparen dbg) = dbg
getTokenDbg (TokRparen dbg) = dbg
getTokenDbg (TokLambda dbg) = dbg
getTokenDbg (TokLet dbg) = dbg
getTokenDbg (TokIf dbg) = dbg
getTokenDbg (TokBool _ dbg) = dbg
getTokenDbg (TokInt _ dbg) = dbg
getTokenDbg (TokVar _ dbg) = dbg

isSpecialChar :: Char -> Bool
isSpecialChar char = char == '(' || char == ')'

-- TODO: add quit as a keyword
keywords :: Map.HashMap Text.Text (Dbg -> Token)
keywords =
  (Map.fromList . map (TupleOps.app1 Text.pack))
    [ ("lambda", TokLambda),
      ("let", TokLet),
      ("if", TokIf),
      ("#true", TokBool True),
      ("#false", TokBool False)
    ]

getTokenOfLexeme :: Text.Text -> Int -> Token
getTokenOfLexeme lexeme index =
  let dbg = Dbg index (index + Text.length lexeme)
   in case Regex.matchRegex (Regex.mkRegex "^-?[0-9]+$") (Text.unpack lexeme) of
        Just _ -> TokInt (read (Text.unpack lexeme)) dbg
        _ -> fromMaybe (TokVar lexeme) (Map.lookup lexeme keywords) dbg

type TokenizerType a = State (Text.Text, Int) a

skipSpaces :: TokenizerType ()
skipSpaces = do
  (input, index) <- get
  case Text.uncons input of
    Just (head, tail) | isSpace head -> do put (tail, index + 1); skipSpaces
    _ -> return ()

getToken :: TokenizerType (Maybe Token)
getToken = do
  (input, index) <- get
  case Text.uncons input of
    Just ('(', rest) -> do put (rest, index + 1); return $ Just (TokLparen (Dbg index (index + 1)))
    Just (')', rest) -> do put (rest, index + 1); return $ Just (TokRparen (Dbg index (index + 1)))
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
data ParserState = ParserState {psTokens :: [Token], psSource :: Text.Text}

type ParserType = EitherT String (State ParserState)

getVarName :: Term -> Text.Text
getVarName (TermFvar name _) = name
getVarName (TermBvar _ name _) = name

parseError :: String -> ParserType a
parseError expected = do
  ParserState tokens input <- lift get
  let dbg = let len = Text.length input in Dbg len len
  let message = "Parsing error: expected " ++ expected
  case tokens of
    token : _ -> throwError (makeErrorString input (getTokenDbg token) (message ++ " but got " ++ show token))
    [] -> throwError (makeErrorString input dbg (message ++ " but reached end of file"))

getTokens :: ParserType [Token]
getTokens = do ParserState tokens _ <- lift get; return tokens

setTokens :: [Token] -> ParserType ()
setTokens tokens = do lift $ modify (\s -> s {psTokens = tokens})

parseTokRparen :: ParserType Int
parseTokRparen = do
  ParserState tokens _ <- lift get
  case tokens of
    TokRparen (Dbg _ end) : rest -> do setTokens rest; return end
    _ -> parseError "')'"

-- <term> ::=
--     | <var>
--     | (lambda (<var>*) <term>)
--     | (<term> <term>*)
--     | (let ((<var> <term>)*) <term>)
--     | #true
--     | #false
--     | (if <term> <term> <term>)
--     | <int>
--     | ()
parseTerm :: ParserType Term
parseTerm = do
  ParserState tokens input <- lift get
  case tokens of
    TokVar _ _ : _ -> parseVar
    TokBool value dbg : rest -> do setTokens rest; return $ TermBool True dbg
    TokInt value dbg : rest -> do setTokens rest; return $ TermInt value dbg
    TokLparen (Dbg start _) : TokRparen (Dbg _ end) : rest -> do setTokens rest; return $ TermUnit (Dbg start end)
    TokLparen _ : TokLambda _ : _ -> parseLambda
    TokLparen _ : TokLet _ : _ -> parseLet
    TokLparen _ : TokIf _ : _ -> parseIf
    TokLparen _ : _ -> parseApp
    token : _ ->
      let message = "Internal parsing error, could not match against any rules" ++ show tokens
       in throwError $ makeErrorString input (getTokenDbg token) message
    [] -> throwError "Internal parsing error, token list was empty"

-- <term>*)  does not consume the closing right parentheses
parseTerms :: ParserType [Term]
parseTerms = do
  tokens <- getTokens
  case tokens of
    TokRparen _ : _ -> return []
    _ -> do result <- parseTerm; tail <- parseTerms; return $ result : tail

-- <var>
parseVar :: ParserType Term
parseVar = do
  tokens <- getTokens
  case tokens of
    TokVar name dbg : rest -> do
      setTokens rest
      return $ TermFvar name dbg
    _ -> parseError "<var>"

-- <var>*)  does not consume the closing right parentheses
parseVars :: ParserType [Term]
parseVars = do
  tokens <- getTokens
  case tokens of
    TokRparen _ : _ -> return []
    _ -> do var <- parseVar; vars <- parseVars; return $ var : vars

-- (lambda (<var>*) <term>)
parseLambda :: ParserType Term
parseLambda = do
  tokens <- getTokens
  case tokens of
    TokLparen (Dbg start _) : TokLambda _ : TokLparen _ : rest -> do
      setTokens rest
      vars <- parseVars
      let varNames = map getVarName vars
      parseTokRparen
      body <- parseTerm
      TermAbs body [] varNames . Dbg start <$> parseTokRparen
    _ -> parseError "(lambda (<var>*) <term>)"

-- (<term> <term>*)
parseApp :: ParserType Term
parseApp = do
  tokens <- getTokens
  case tokens of
    TokLparen (Dbg start _) : rest -> do
      setTokens rest
      fun <- parseTerm
      args <- parseTerms
      TermApp fun args . Dbg start <$> parseTokRparen
    _ -> parseError "(<term> <term>*)"

-- (let (<binding>*) <term>)
parseLet :: ParserType Term
parseLet = do
  tokens <- getTokens
  case tokens of
    TokLparen (Dbg start _) : TokLet _ : TokLparen _ : rest -> do
      setTokens rest
      letBindings <- parseLetBindings
      body <- parseTerm
      TermLet letBindings body . Dbg start <$> parseTokRparen
    _ -> parseError "(let <var> <term>)"

-- (<var> <term>)
parseLetBinding :: ParserType LetBinding
parseLetBinding = do
  tokens <- getTokens
  case tokens of
    TokLparen (Dbg start _) : rest -> do
      setTokens rest
      var <- parseVar
      value <- parseTerm
      end <- parseTokRparen
      let dbg = Dbg start end
      return $ LetBinding value (tFvarName var) dbg
    _ -> parseError "(<var> <term>)"

-- <binding>*)
parseLetBindings :: ParserType [LetBinding]
parseLetBindings = do
  tokens <- getTokens
  case tokens of
    TokLparen _ : _ -> do
      head <- parseLetBinding
      tail <- parseLetBindings
      return $ head : tail
    TokRparen _ : rest -> do setTokens rest; return []
    _ -> parseError "(<var> <term>)*)"

-- (if <term> <term> <term>)
parseIf :: ParserType Term
parseIf = do
  tokens <- getTokens
  case tokens of
    TokLparen (Dbg start _) : TokIf _ : rest -> do
      setTokens rest
      cond <- parseTerm
      cnsq <- parseTerm
      alt <- parseTerm
      TermIf cond cnsq alt . Dbg start <$> parseTokRparen
    _ -> parseError "(if <term> <term> <term>)"

-- replace bound variables with indices
localState :: ([Text.Text] -> [Text.Text]) -> State [Text.Text] Term -> State [Text.Text] Term
localState f binder = do state <- get; modify f; result <- binder; put state; return result

bindVars :: Term -> State [Text.Text] Term
bindVars fvar@(TermFvar name dbg) = do
  context <- get
  case elemIndex name context of
    Just index -> return $ TermBvar index name dbg
    _ -> return fvar
bindVars abs@(TermAbs body _ varNames _) = do
  body' <- localState (reverse varNames ++) (bindVars body)
  return $ abs {tAbsBody = body'}
bindVars (TermApp fn args dbg) = do
  fn' <- bindVars fn
  args' <- bindVarsMap args
  return $ TermApp fn' args' dbg
bindVars (TermLet letVals body dbg) = do
  context <- get
  letVals' <- bindLetVals letVals
  body' <- bindVars body
  return $ TermLet letVals' body' dbg
bindVars (TermIf cond cnsq alt dbg) = do
  cond' <- bindVars cond
  cnsq' <- bindVars cnsq
  alt' <- bindVars alt
  return $ TermIf cond' cnsq' alt' dbg
bindVars t = return t

bindVarsMap :: [Term] -> State [Text.Text] [Term]
bindVarsMap (term : rest) = do
  term' <- bindVars term
  rest' <- bindVarsMap rest
  return $ term' : rest'
bindVarsMap [] = return []

bindLetVals :: [LetBinding] -> State [Text.Text] [LetBinding]
bindLetVals (letVal@(LetBinding value name dbg) : rest) = do
  modify (name :)
  value' <- bindVars value
  rest' <- bindLetVals rest
  return $ letVal {lbValue = value'} : rest'
bindLetVals [] = return []

-- wrapper around pipeline
parse :: Text.Text -> Either String Term
parse input =
  let (result, state) = runState (runEitherT parseTerm) (ParserState (tokenize input) input)
   in case (result, state) of
        (Right term, ParserState [] _) -> return $ evalState (bindVars term) []
        (Left error, _) -> throwError error
        (_, ParserState (_ : _) _) -> throwError "Parsing error: unexpected input after term"