module Parser where
import Term
import Data.List
import Data.Char
import Data.Maybe
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

getTokenOfLexeme :: Text.Text -> Dbg -> Token
getTokenOfLexeme lexeme dbg
    | Just _ <- Regex.matchRegex (Regex.mkRegex "^-?[0-9]+$") (Text.unpack lexeme) =
        let value = read (Text.unpack lexeme) :: Integer
        in ToInt value dbg
    | otherwise = let keyword = Map.lookup lexeme keywords
        in fromMaybe (ToVar lexeme) keyword dbg

skipSpaces :: Text.Text -> (Text.Text, Int)
skipSpaces input =
    f input 0
    where f text index
            | Just (head, tail) <- Text.uncons text, isSpace head = f tail (index + 1)
            | otherwise = (text, index)

getToken :: Text.Text -> (Int -> Dbg) -> Maybe (Token, Text.Text)
getToken input dbg
    | Just ('(', rest) <- Text.uncons input = Just (Lparen (dbg 1), rest)
    | Just (')', rest) <- Text.uncons input = Just (Rparen (dbg 1), rest)
    | Just (char, _) <- Text.uncons input, isSpace char =
        let (rest, skippedSpaces) = skipSpaces input
        in let dbg2 = dbg 0
        in let start = dStart dbg2 + skippedSpaces
        in let dbg3 length = Dbg start (start + length)
        in getToken rest dbg3
    | otherwise =
        let (lexeme, rest) = Text.break (\char -> isSpecialChar char || isSpace char) input
        in let length = Text.length lexeme
        in if length == 0 then Nothing else Just (getTokenOfLexeme lexeme (dbg length), rest)

tokenize :: Text.Text -> [Token]
tokenize source =
    f source 0
    where f text i
            | Just (token, rest) <- getToken text (\length -> Dbg i (i + length)) =
                let dbg = getTokenDbg token
                in token : f rest (dEnd dbg)
            | otherwise = []


-- parser
getVarName :: Term -> Text.Text
getVarName (TFVar name _) = name
getVarName (TBVar _ name _) = name

-- <term> ::=
--     | <var>
--     | (lambda (<var>*) <term>)
--     | (<term> <term>*)
--     | (let <var> <term>)
--     | (block <term>*)
--     | #true
--     | #false
--     | (if <term> <term> <term>)
--     | <int>
--     | ()
parseTerm :: Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])
parseTerm input [] context = (Left "Internal parsing error, token list was empty", [])
parseTerm input tokens@(ToVar _ _ : rest) context = parseVar input tokens context
parseTerm input (ToTrue dbg : rest) context = (Right (TBool True dbg), rest)
parseTerm input (ToFalse dbg : rest) context = (Right (TBool False dbg), rest)
parseTerm input (ToInt value dbg : rest) context = (Right (TInt value dbg), rest)
parseTerm input tokens@(Lparen (Dbg start _) : Rparen (Dbg end _) : rest) _ = (Right (TUnit (Dbg start end)), rest)
parseTerm input tokens@(Lparen _ : ToAbs _ : _) context = parseLambda input tokens context
parseTerm input tokens@(Lparen _ : ToLet _ : _) context = parseLet input tokens context
parseTerm input tokens@(Lparen _ : ToBlock _ : _) context = parseBlock input tokens context
parseTerm input tokens@(Lparen _ : ToIf _ : _) context = parseIf input tokens context
parseTerm input tokens@(Lparen _ : _) context = parseApp input tokens context

-- <term>*)  does not consume the closing right parentheses
parseTerms :: Text.Text -> [Token] -> [Text.Text] -> (Either String [Term], [Token])
parseTerms input rest@(Rparen _ : _) context = (Right [], rest)
parseTerms input tokens context
    | (Right term, rest) <- parseTerm input tokens context
    , (Right tail, rest2) <- parseTerms input rest  context =
        (Right (term : tail), rest2)
    | otherwise = (Left "Parsing error: expected <term>* but got something else", tokens)

-- <var>
parseVar :: Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])
parseVar input tokens@(ToVar name dbg : rest) context
    | (Just index) <- elemIndex name context = (Right (TBVar index name dbg), rest)
    | otherwise = (Right (TFVar name dbg), rest)
parseVar input (token : rest) context =
    let message = "Parsing error: expected <var>, but got " ++ show token
    in (Left (makeErrorString input (getTokenDbg token) message), rest)

-- <var>*)
parseVars :: Text.Text -> [Token] -> [Text.Text] -> (Either String [Term], [Token])
parseVars input (Rparen _ : rest) context = (Right [], rest)
parseVars input tokens context
    | (Right varTerm, rest) <- parseVar input tokens context
    , (Right vars, rest2) <- parseVars input rest context =
        (Right (varTerm : vars), rest2)
parseVars input (token : rest) context =
        let message = "Parsing error: expected <var> or ), but got something else"
        in (Left (makeErrorString input (getTokenDbg token) message), rest)

-- (lambda (<var>*) <term>)
parseLambda :: Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])
parseLambda input tokens@(Lparen (Dbg start _) : ToAbs _ : Lparen _ : rest) context
    | (Right vars, rest2) <- parseVars input rest context
    , varNames <- map getVarName vars
    , (Right body, Rparen (Dbg _ end) : rest3) <- parseTerm input rest2 (reverse varNames ++ context) =
        (Right (TAbs (length vars) body [] varNames (Dbg start end)), rest3)
    | (Left error, _) <- parseVars input rest context = (Left error, tokens)
    | (Right vars, rest2) <- parseVars input rest context
    , varNames <- map getVarName vars
    , (Right body, _) <- parseTerm input rest2 (reverse varNames ++ context) =
        (Left "Parsing error: expected ), but got something else", tokens)
parseLambda input tokens context =
    let message = "Parsing error: expected (lambda (<var>*) <term>), but got something else"
    in (Left message, tokens)

-- (<term> <term>*)
parseApp :: Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])
parseApp input tokens@(Lparen (Dbg start _) : rest) context
    | (Right fun, rest2) <- parseTerm input rest context
    , (Right args, Rparen (Dbg _ end) : rest3) <- parseTerms input rest2 context =
        (Right (TApp fun args (Dbg start end)), rest3)
    | otherwise = (Left "Parsing error: expected (<term> <term>*) but got something else", tokens)

-- (let <var> <term>)
parseLet :: Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])
parseLet input tokens@(Lparen (Dbg start _) : ToLet _ : Lparen _ : rest) context
    | (Right vars, rest2) <- parseVars input rest context
    , (Right body, Rparen (Dbg _ end) : rest3) <- parseTerm input rest2 context =
        -- TAbs {tAbsArity :: Int, tAbsBody :: Term, tAbsEnv :: [Term], tAbsVarNames :: [Text.Text], tAbsDbg :: Dbg}
        (Right (TAbs (length vars) body [] (map getVarName vars) (Dbg start end)), rest3)
    | (Left error, _) <- parseVars input rest context = (Left error, tokens) -- error parsing vars
    | (Right vars, rest2) <- parseVars input rest context, (Right body, _) <- parseTerm input rest2 context =
        (Left "Parsing error: expected ), but got something else", tokens) --
parseLet input tokens context =
    let message = "Parsing error: expected (let <var> <term>), but got something else"
    in (Left message, tokens)

-- (block <term>*)
parseBlock :: Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])
parseBlock input tokens@(Lparen (Dbg start _) : ToAbs _ : Lparen _ : rest) context
    | (Right vars, rest2) <- parseVars input rest context
    , (Right body, Rparen (Dbg _ end) : rest3) <- parseTerm input rest2 context =
        -- TAbs {tAbsArity :: Int, tAbsBody :: Term, tAbsEnv :: [Term], tAbsVarNames :: [Text.Text], tAbsDbg :: Dbg}
        (Right (TAbs (length vars) body [] (map getVarName vars) (Dbg start end)), rest3)
    | (Left error, _) <- parseVars input rest context = (Left error, tokens) -- error parsing vars
    | (Right vars, rest2) <- parseVars input rest context, (Right body, _) <- parseTerm input rest2 context =
        (Left "Parsing error: expected ), but got something else", tokens) --
parseBlock input tokens context =
    let message = "Parsing error: expected (block <term>*), but got something else"
    in (Left message, tokens)

-- (if <term> <term> <term>)
parseIf :: Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])
parseIf input tokens@(Lparen (Dbg start _) : ToIf _ : rest) context
    | (Right cond, rest2) <- parseTerm input rest context
    , (Right cnsq, rest3) <- parseTerm input rest2 context
    , (Right alt, Rparen (Dbg _ end) : rest4) <- parseTerm input rest3 context =
        (Right (TIf cond cnsq alt (Dbg start end)), rest4)
    | otherwise = (Left "Parsing error: expected (if <term> <term> <term>) but got something else", tokens)