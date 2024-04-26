module Lambda where
import Control.Exception
import Data.Data
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Text as Text
import qualified Data.Tuple.Ops as TupleOps
import qualified Text.Regex as Regex
import Data.Either (partitionEithers)


-- debug info
data Dbg = Dbg {dStart :: Int, dEnd :: Int} deriving (Eq, Show)

emptyDbg :: Dbg
emptyDbg = Dbg {dStart = 0, dEnd = 0}

getStringAtDbg :: Text.Text -> Dbg -> Text.Text
getStringAtDbg input (Dbg start end) = Text.take (end - start) (Text.drop start input)

makeErrorString :: Text.Text -> Dbg -> String -> String
makeErrorString input dbg@(Dbg start end) message =
    let lexeme = Text.unpack (getStringAtDbg input dbg)
    in let (prefix, rest) = Text.splitAt start input
    in let prefixLines = Text.lines prefix
    in let line = if null prefixLines then 0 else length prefixLines - 1
    in let lastline = if null prefixLines then Text.empty else last prefixLines
    in let column = Text.length lastline
    -- in "error Dbg: start = " ++ show start ++ ", end = " ++ show end
    in "Error at " ++ lexeme ++ " (line " ++ show (line + 1) ++ ", column " ++ show (column + 1) ++ "):\n\t" ++ message

-- term
data Term
    = TFVar {tFVarName :: Text.Text, tfVarDbg :: Dbg}
    | TBVar {tBvarIndex :: Int, tBVarName :: Text.Text, barDbg :: Dbg}
    | TAbs {tAbsArity :: Int, tAbsBody :: Term, tAbsEnv :: [Term], tAbsVarNames :: [Text.Text], tAbsDbg :: Dbg}
    | TApp {appFn :: Term, appArgs :: [Term], appDbg :: Dbg}
    | TBool {boolValue :: Bool, boolDbg :: Dbg}
    | TIf {ifCond :: Term, ifCnsq :: Term, ifAlt :: Term, ifDbg :: Dbg}
    | TInt {intValue :: Integer, intDbg :: Dbg}
    deriving Eq

instance Show Term where
    show (TFVar name _) = Text.unpack name
    show (TBVar _ name _) = Text.unpack name
    show (TAbs _ body [] varNames _) = "(lambda (" ++ unwords (map Text.unpack varNames) ++ ") " ++ show body ++ ")"
    show abs@(TAbs _ _ env _ _) = "(closure (" ++ unwords (map show env)  ++ ") (" ++ show (abs {tAbsEnv = []}) ++ "))"
    show (TApp fn args _) = "(app " ++ show fn ++ " " ++ show args ++ ")"
    show (TBool True _) = "#true"
    show (TBool False _) = "#false"
    show (TIf cond cnsq alt _) = "(if " ++ show cond ++ " " ++ show cnsq ++ " " ++ show alt ++ ")"
    show (TInt value _) = show value

-- eval/apply
eval :: Text.Text -> Term -> [Term] -> Either String Term
eval _ fvar@(TFVar _ _) _ = Right fvar
eval input (TBVar index _ dbg) stack
    | index < length stack = Right (stack !! index)
    | otherwise =
        let message = "Could not evaluate bound variable - invalid index" ++ show index
        in Left (makeErrorString input dbg message)
eval _ abs@(TAbs {}) _ = Right abs
eval input (TApp fn args dbg) stack
    | Right fn' <- eval input fn stack
    , ([], args') <- partitionEithers (map (\a -> eval input a stack) args) =
        apply input fn' args' dbg stack
    | Right _ <- eval input fn stack
    , (failedArgs, _) <- partitionEithers (map (\a -> eval input a stack) args)
        = Left (head failedArgs)
    | Left failedFn <- eval input fn stack =
        Left failedFn
eval _ bool@(TBool {}) _ = Right bool
eval input (TIf cond cnsq alt dbg) stack = evalIf input cond cnsq alt dbg stack
eval _ int@(TInt _ _) _ = Right int

intBinaryOps :: Map.HashMap Text.Text (Integer -> Integer -> Dbg -> Term)
intBinaryOps = (Map.fromList . map (TupleOps.app1 Text.pack))
    [ ("+", \l r d -> TInt (l + r) d)
    , ("-", \l r d -> TInt (l - r) d)
    , ("*", \l r d -> TInt (l * r) d)
    , ("/", \l r d -> TInt (quot l r) d)
    , ("<", \l r d -> TBool (l < r) d)
    , ("<=", \l r d -> TBool (l <= r) d)
    , ("=", \l r d -> TBool (l == r) d)
    , (">", \l r d -> TBool (l > r) d)
    , (">=", \l r d -> TBool (l >= r) d)
    , ("/=", \l r d -> TBool (l /= r) d) ]

apply :: Text.Text -> Term -> [Term] -> Dbg -> [Term] -> Either String Term
apply input abs@(TAbs arity body env _ _) args dbg stack
    | length env + length args == arity = eval input body (reverse args ++ env ++ stack)
    | length env + length args < arity = Right (abs {tAbsEnv = reverse args ++ env})
    | otherwise =
        let expectedArgs = arity - length env
        in let actualArgs = length env + length args
        in let message = "Could not evaluate function - expected " ++ show expectedArgs
                ++ " arguments, " ++ "but got " ++ show actualArgs
        in Left (makeErrorString input dbg message)
apply input intBinOp@(TFVar opName _) [l, r] dbg stack
    | Map.member opName intBinaryOps
    , Right (TInt lVal (Dbg start _)) <- eval input l stack
    , Right (TInt rVal (Dbg _ end)) <- eval input r stack =
        Right ((intBinaryOps Map.! opName) lVal rVal (Dbg start end))
    | Map.member opName intBinaryOps
    , Right (TInt _ _) <- eval input l stack
    , Right rTerm <- eval input r stack =
        let message = "Could not evaluate " ++ Text.unpack opName
                ++ ", expected right argument to be an integer but got " ++ show rTerm
        in Left (makeErrorString input dbg message)
    | Map.member opName intBinaryOps, Right lTerm <- eval input l stack =
        let message = "Could not evaluate " ++ Text.unpack opName
                ++ ", expected left argument to be an integer but got " ++ show lTerm
        in Left (makeErrorString input dbg message)
    | Map.member opName intBinaryOps, Left error <- eval input l stack = Left error
    | Map.member opName intBinaryOps, Left error <- eval input r stack = Left error
apply input term _ dbg _ =
    let message = "Could not evaluate function - expected a lambda or primitive function but got " ++ show term
    in Left (makeErrorString input dbg message)

evalIf :: Text.Text -> Term -> Term -> Term -> Dbg -> [Term] -> Either String Term
evalIf input cond cnsq alt dbg stack
    | Right (TBool True _) <- eval input cond stack = eval input cnsq stack
    | Right (TBool False _) <- eval input cond stack = eval input alt stack
    | Left evalError <- eval input cond stack = Left evalError
    | otherwise =
        let message = "Could not evaluate if expression - expected a boolean as the condition but got " ++ show cond
        in Left (makeErrorString input dbg message)


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

data ANFExp
    = AVal ANFVal
    | AApp {aaName :: Text.Text, aaFun :: ANFExp, aaArgs :: [ANFVal], aaDbg :: Dbg, aaIn :: ANFExp}
    | AIf {aiName :: Text.Text, aiCond :: ANFVal, aiCnsq :: ANFExp, aiAlt :: ANFExp, aiDbg :: Dbg, aIn :: ANFExp}
    deriving (Eq, Show)

data ANFVal
    = AVar {avName :: Text.Text, apvDbg :: Dbg}
    | ALambda {alVars :: [ANFVal], alBody :: ANFExp, alDbg :: Dbg}
    | ABool {abBool :: Bool, abDbg :: Dbg}
    deriving (Eq, Show)

evalString :: String -> Either String Term
evalString input
    | (Right term, []) <- parseTerm (Text.pack input) (tokenize (Text.pack input)) []
    , Right result <- eval (Text.pack input) term [] =
        Right result
    | (Right term, []) <- parseTerm (Text.pack input) (tokenize (Text.pack input)) []
    , Left error <- eval (Text.pack input) term [] =
        Left error
    | (Left error, _) <- parseTerm (Text.pack input) (tokenize (Text.pack input)) [] =
        Left error
    | (_, _ : _) <- parseTerm (Text.pack input) (tokenize (Text.pack input)) [] =
        Left "evalString - unexpected input after term"