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
    | Lambda {laDbg :: Dbg}
    | ToIf {toIfDbg :: Dbg}
    | ToTrue {toTDbg :: Dbg}
    | ToFalse {toFDbg :: Dbg}
    | ToInt {toInt :: Integer, toNDbg :: Dbg}
    | ToVar {toVName :: Text.Text, toVDbg :: Dbg}
    deriving (Eq, Show)

getTokenDbg :: Token -> Dbg
getTokenDbg (Lparen dbg) = dbg
getTokenDbg (Rparen dbg) = dbg
getTokenDbg (Lambda dbg) = dbg
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
    [ ("lambda", Lambda)
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
-- <term> ::=
--     | <var>
--     | (lambda (<var>*) <term>)
--     | (<term> <term>*)
--     | #true
--     | #false
--     | (if <term> <term> <term>)

parseTerm :: Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])
parseTerm input [] context = (Left "Internal parsing error, token list was empty", [])
parseTerm input (ToVar name dbg : rest) context = (Right (TFVar name dbg), rest)
parseTerm input (ToTrue dbg : rest) context = (Right (TBool True dbg), rest)
parseTerm input (ToFalse dbg : rest) context = (Right (TBool False dbg), rest)
parseTerm input (ToInt value dbg : rest) context = (Right (TInt value dbg), rest)
parseTerm input tokens@(Lparen _ : Lambda _ : _) context = parseLambda input tokens context
parseTerm input tokens@(Lparen _ : ToIf _ : _) context = parseIf input tokens context
parseTerm input tokens@(Lparen _ : _) context = parseApp input tokens context

parseLambda :: Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])
parseLambda input tokens@(Lparen (Dbg start _) : Lambda _ : Lparen _ : rest) context
    | (Right vars, rest2) <- parseVars input rest
    , (Right body, Rparen (Dbg _ end) : rest3) <- parseTerm input rest2 context =
        -- TAbs {tAbsArity :: Int, tAbsBody :: Term, tAbsEnv :: [Term], tAbsVarNames :: [Text.Text], tAbsDbg :: Dbg}
        let body2 = lowerTBVars body (reverse (map TupleOps.sel1 vars) ++ context)
        in (Right (TAbs (length vars) body2 [] (map TupleOps.sel1 vars) (Dbg start end)), rest3)
        -- (Right (TAbs (length vars) body [] (map TupleOps.sel1 vars) (Dbg start end)), rest3) -- success
    | (Left error, _) <- parseVars input rest = (Left error, tokens) -- error parsing vars
    | (Right vars, rest2) <- parseVars input rest , (Right body, _) <- parseTerm input rest2 context =
        (Left "Parsing error: expected ), but got something else", tokens) --
parseLambda input tokens context =
    let message = "Parsing error: expected (lambda (<var>*) <term>), but got something else"
    in (Left message, tokens)

lowerTBVars :: Term -> [Text.Text] -> Term
lowerTBVars var@(TFVar name dbg) context
    | (Just index) <- elemIndex name context = TBVar index name dbg
    | otherwise = var
lowerTBVars bvar@(TBVar {}) _ = bvar
lowerTBVars abs@(TAbs _ body _ vars _) context = abs {tAbsBody = lowerTBVars body (reverse vars ++ context)}
lowerTBVars app@(TApp fn args _) context = app {appFn = lowerTBVars fn context, appArgs = map (`lowerTBVars` context) args}
lowerTBVars tbool@(TBool _ _) _ = tbool
lowerTBVars tif@(TIf cond cnsq alt _) context =
    let cond2 = lowerTBVars cond context
    in let cnsq2 = lowerTBVars cnsq context
    in let alt2 = lowerTBVars alt context
    in tif {ifCond = cond2, ifCnsq = cnsq2, ifAlt = alt2}
lowerTBVars tint@(TInt {}) _ = tint

parseVars :: Text.Text -> [Token] -> (Either String [(Text.Text, Dbg)], [Token])
parseVars input tokens
    | (vars, Rparen _ : rest) <- span isFVar tokens =
        (Right (map (\x -> (toVName x, toVDbg x)) vars), rest)
    | otherwise =
        (Left "Parsing error: expected ), but got something else", tokens)
        where
                isFVar (ToVar _ _) = True
                isFVar _ = False

parseIf :: Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])
parseIf input tokens@(Lparen (Dbg start _) : ToIf _ : rest) context
    | (Right cond, rest2) <- parseTerm input rest context
    , (Right cnsq, rest3) <- parseTerm input rest2 context
    , (Right alt, Rparen (Dbg _ end) : rest4) <- parseTerm input rest3 context =
        (Right (TIf cond cnsq alt (Dbg start end)), rest4)
    | otherwise = (Left "Parsing error: expected (if <term> <term> <term>) but got something else", tokens)

parseApp :: Text.Text -> [Token] -> [Text.Text] -> (Either String Term, [Token])
parseApp input tokens@(Lparen (Dbg start _) : rest) context
    | (Right fun, rest2) <- parseTerm input rest context
    , (Right args, Rparen (Dbg _ end) : rest3) <- parseTerms input rest2 context =
        (Right (TApp fun args (Dbg start end)), rest3)
    | otherwise = (Left "Parsing error: expected (<term> <term>*) but got something else", tokens)

parseTerms :: Text.Text -> [Token] -> [Text.Text] -> (Either String [Term], [Token])
parseTerms input rest@(Rparen _ : _) context = (Right [], rest) 
parseTerms input tokens context
    | (Right term, rest) <- parseTerm input tokens context
    , (Right tail, rest2) <- parseTerms input rest  context=
        (Right (term : tail), rest2)
    | otherwise = (Left "Parsing error: expected <term>* but got something else", tokens)