module Parser where
import Term
import Data.List
import Data.Char
import Data.Maybe
import Data.Maybe.HT
import Data.Either.Extra
import Control.Monad.State.Lazy
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Tuple.Ops as TupleOps
import qualified Text.Regex as Regex

-- tokenizer
data Token
    = Lparen {lpDbg :: Dbg}
    | Rparen {rpDbg :: Dbg}
    | ToAbs {laDbg :: Dbg}
    | ToLet {toLetDbg :: Dbg}
    | ToBlock {toBlkDbg :: Dbg}
    | ToIf {toIfDbg :: Dbg}
    | ToTrue {toTDbg :: Dbg}
    | ToFalse {toFDbg :: Dbg}
    | ToInt {toInt :: Integer, toNDbg :: Dbg}
    | ToVar {toVName :: Text.Text, toVDbg :: Dbg}
    deriving Eq

instance Show Token where
    show (Lparen _) = "'('"
    show (Rparen _) = "')"
    show (ToAbs _) = "\"\"lambda\""
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
getTokenDbg (ToAbs dbg) = dbg
getTokenDbg (ToIf dbg) = dbg
getTokenDbg (ToTrue dbg) = dbg
getTokenDbg (ToFalse dbg) = dbg
getTokenDbg (ToInt _ dbg) = dbg
getTokenDbg (ToVar _ dbg) = dbg

specialChars :: [Char]
specialChars = ['(', ')']

isSpecialChar :: Char -> Bool
isSpecialChar char = char `elem` specialChars

-- TODO: add quit as a keyword
keywords :: Map.HashMap Text.Text (Dbg -> Token)
keywords = (Map.fromList . map (TupleOps.app1 Text.pack))
    [ ("lambda", ToAbs)
    , ("let", ToLet)
    , ("block", ToBlock)
    , ("if", ToIf)
    , ("#true", ToTrue)
    , ("#false", ToFalse) ]

getTokenOfLexeme :: Text.Text -> Int -> Token
getTokenOfLexeme lexeme index = 
    let dbg = Dbg index (index + Text.length lexeme)
    in case Regex.matchRegex (Regex.mkRegex "^-?[0-9]+$") (Text.unpack lexeme) of
        Just _ -> ToInt (read (Text.unpack lexeme)) dbg
        _ -> fromMaybe (ToVar lexeme) (Map.lookup lexeme keywords) dbg

skipSpaces :: State (Text.Text, Int) ()
skipSpaces = do
    (input, index) <- get
    case Text.uncons input of
        Just (head, tail) | isSpace head -> put (tail, index + 1)
        _ -> return ()

getToken :: State (Text.Text, Int) (Maybe Token)
getToken = do
    (input, index) <- get
    case Text.uncons input of
        Just ('(', rest) -> do { put (rest, index + 1); return $ Just (Lparen (Dbg index (index + 1))) }
        Just (')', rest) -> do { put (rest, index + 1); return $ Just (Rparen (Dbg index (index + 1))) }
        Just (char, _) | isSpace char -> do { skipSpaces; getToken }
        _ -> do
            let (lexeme, rest) = Text.break (\char -> isSpecialChar char || isSpace char) input
            let length = Text.length lexeme
            put (rest, index + length)
            return $ toMaybe (length /= 0) (getTokenOfLexeme lexeme index)

tokenizeLoop :: State (Text.Text, Int) [Token]
tokenizeLoop = do
    maybeToken <- getToken
    case maybeToken of
        Just token -> do
            rest <- tokenizeLoop
            return $ token : rest
        Nothing -> do { return [] }

tokenize :: Text.Text -> [Token]
tokenize source = evalState tokenizeLoop (source, 0)


-- parser
type ParserType = Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])

getVarName :: Term -> Text.Text
getVarName (TFVar name _) = name
getVarName (TBVar _ name _) = name

-- does not consume the closing right parentheses
parseExpectRparen :: ParserType -> ParserType
parseExpectRparen parser input tokens context
    | (Right term, rest@(Rparen _ : _)) <- parser input tokens context =
        (Right term, rest)
    | (Right term, rest@(token : _)) <- parser input tokens context =
        let message = "Parsing error: expected ')' but got" ++ show token
        in let dbg = let len = Text.length input in Dbg len len
        in (Left (makeErrorString input dbg message), rest)
    | (Right _, []) <- parser input tokens context =
        let message = "Parsing error: expected ')' but reached end of file"
        in let dbg = let len = Text.length input in Dbg len len
        in (Left (makeErrorString input dbg message), [])
    | error@(Left _, _) <- parser input tokens context = error

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
parseTerm :: ParserType
parseTerm input [] context = (Left "Internal parsing error, token list was empty", [])
parseTerm input tokens@(ToVar _ _ : rest) context = parseVar input tokens context
parseTerm input (ToTrue dbg : rest) context = (Right (TBool True dbg), rest)
parseTerm input (ToFalse dbg : rest) context = (Right (TBool False dbg), rest)
parseTerm input (ToInt value dbg : rest) context = (Right (TInt value dbg), rest)
parseTerm input tokens@(Lparen (Dbg start _) : Rparen (Dbg end _) : rest) _ = (Right (TUnit (Dbg start end)), rest)
parseTerm input tokens@(Lparen _ : ToAbs _ : _) context = parseLambda input tokens context
parseTerm input tokens@(Lparen _ : ToBlock _ : _) context = parseBlock input tokens context
parseTerm input tokens@(Lparen _ : ToIf _ : _) context = parseIf input tokens context
parseTerm input tokens@(Lparen _ : _) context = parseApp input tokens context
parseTerm input tokens _ = (Left "Internal parsing error, could not match against any rules", tokens)

-- <term>*)  does not consume the closing right parentheses
parseTerms :: Text.Text -> [Token] -> [Text.Text] -> (Either String [Term], [Token])
parseTerms input rest@(Rparen _ : _) context = (Right [], rest)
parseTerms input tokens context
    | (Right term, rest) <- parseTerm input tokens context
    , (Right tail, rest2) <- parseTerms input rest  context =
        (Right (term : tail), rest2)
parseTerms input (token : rest) context =
    let message = "Parsing error: expected <term>* but got something else" ++ show token
    in (Left (makeErrorString input (getTokenDbg token) message), rest)
parseTerms input [] context =
    (Left "Parsing error: expected <term>* but reached end of file", [])

-- <var>
parseVar :: ParserType
parseVar input tokens@(ToVar name dbg : rest) context
    | (Just index) <- elemIndex name context = (Right (TBVar index name dbg), rest)
    | otherwise = (Right (TFVar name dbg), rest)
parseVar input (token : rest) context =
    let message = "Parsing error: expected <var> but got " ++ show token
    in (Left (makeErrorString input (getTokenDbg token) message), rest)
parseVar input [] context =
    (Left "Parsing error: expected <var> but reached end of file", [])

-- <var>*)
parseVars :: Text.Text -> [Token] -> [Text.Text] -> (Either String [Term], [Token])
parseVars input (Rparen _ : rest) context = (Right [], rest)
parseVars input tokens context
    | (Right varTerm, rest) <- parseVar input tokens context
    , (Right vars, rest2) <- parseVars input rest context =
        (Right (varTerm : vars), rest2)
parseVars input (token : rest) context =
    let message = "Parsing error: expected <var> or ) but got something else"
    in (Left (makeErrorString input (getTokenDbg token) message), rest)
parseVars input [] context =
    (Left "Parsing error: expected <var> or ) but reached end of file", [])

-- (lambda (<var>*) <term>)
parseLambda :: ParserType
parseLambda input tokens@(Lparen (Dbg start _) : ToAbs _ : Lparen _ : rest) context
    | (Right vars, rest2) <- parseVars input rest context
    , varNames <- map getVarName vars
    , (Right body, Rparen (Dbg _ end) : rest3) <- parseTerm input rest2 (reverse varNames ++ context) =
        (Right (TAbs (length vars) body [] varNames (Dbg start end)), rest3)
parseLambda input (token : rest) context =
    let message = "Parsing error: expected (lambda (<var>*) <term>) but got something else"
    in (Left (makeErrorString input (getTokenDbg token) message), rest)
parseLambda input [] context =
    (Left "Parsing error: expected (lambda (<var>*) <term>) but reached end of file", [])

-- (<term> <term>*)
parseApp :: ParserType
parseApp input tokens@(Lparen (Dbg start _) : rest) context
    | (Right fun, rest2) <- parseTerm input rest context
    , (Right args, Rparen (Dbg _ end) : rest3) <- parseTerms input rest2 context =
        (Right (TApp fun args (Dbg start end)), rest3)
parseApp input (token : rest) context =
    let message = "Parsing error: expected (<term> <term>*) but got something else"
    in (Left (makeErrorString input (getTokenDbg token) message), rest)
parseApp input [] context =
    (Left "Parsing error: expected (<term> <term>*) but reached end of file", [])

-- (let <var> <term>)
parseLet :: ParserType
parseLet input tokens@(Lparen (Dbg start _) : ToLet _ : rest) context
    | (Right var, rest2) <- parseVar input rest context
    , (Right value, Rparen (Dbg _ end) : rest3) <- parseTerm input rest2 (getVarName var : context) =
        -- body is meant to be overridden, so it is a dummy value
        (Right (TLet value (TUnit emptyDbg) (getVarName var) (Dbg start end)), rest3)
parseLet input (token : rest) context =
    let message = "Parsing error: expected (let <var> <term>) but got something else"
    in (Left (makeErrorString input (getTokenDbg token) message), rest)
parseLet input [] context =
    (Left "Parsing error: expected (let <var> <term>) but reached end of file", [])

-- (let <var> <term>)* <term>)  does not consume the closing right parentheses
parseLets :: ParserType
parseLets input tokens context
    -- (let <var> <term>) parseLets
    | (Right term, rest) <- parseLet input tokens context
    , result <- parseExpectRparen parseLets input rest (tLetName term : context) =
        TupleOps.app1 (mapRight (\body -> term {tLetBody = body})) result
    -- <term>)
    | otherwise = parseExpectRparen parseTerm input tokens context

-- (block (let <var> <term>)* <term>)
parseBlock :: ParserType
parseBlock input tokens@(Lparen (Dbg start _) : ToBlock _ : rest) context
    | (Right term, Rparen (Dbg _ end) : rest2) <- parseLets input rest context =
        (Right term, rest2)
    | otherwise = parseExpectRparen parseLets input rest context
parseBlock input tokens _ = (Left "", tokens)

-- (if <term> <term> <term>)
parseIf :: ParserType
parseIf input tokens@(Lparen (Dbg start _) : ToIf _ : rest) context
    | (Right cond, rest2) <- parseTerm input rest context
    , (Right cnsq, rest3) <- parseTerm input rest2 context
    , (Right alt, Rparen (Dbg _ end) : rest4) <- parseTerm input rest3 context =
        (Right (TIf cond cnsq alt (Dbg start end)), rest4)
parseIf input (token : rest) context =
    let message = "Parsing error: expected (if <term> <term> <term>) but got something else"
    in (Left (makeErrorString input (getTokenDbg token) message), rest)
parseIf input [] context =
    (Left "Parsing error: expected (if <term> <term> <term>) but reached end of file", [])