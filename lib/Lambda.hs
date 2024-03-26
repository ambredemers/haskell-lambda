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


-- debug info
data Dbg = Dbg {dStart :: Int, dEnd :: Int} deriving (Eq, Show)

emptyDbg :: Dbg
emptyDbg = Dbg {dStart = 0, dEnd = 0}


-- term exception
data TermException = TermException deriving (Show, Typeable)

instance Exception TermException


-- term
data Term
    = TFVar {tFVarName :: Text.Text, tfVarDbg :: Dbg}
    | TBVar {tBvarIndex :: Int, barDbg :: Dbg}
    | TAbs {tAbsArity :: Int, tAbsBody :: Term, tAbsEnv :: [Term], tAbsDbg :: Dbg}
    | TApp {appFn :: Term, appArgs :: [Term], appDbg :: Dbg}
    | TBool {boolValue :: Bool, boolDbg :: Dbg}
    | TIf {ifCond :: Term, ifCnsq :: Term, ifAlt :: Term, ifDbg :: Dbg}
    | TError {teMessage :: Text.Text, teDbg :: Dbg}
    deriving Eq

instance Show Term where
    show (TFVar name _) = "(TfVar " ++ show name ++ ")"
    show (TBVar index _) = "(bvar " ++ show index ++ ")"
    show (TAbs arity body _ _) = "(Tabs " ++ show arity ++ " " ++ show body ++ ")"
    show (TApp fn args _) = "(app " ++ show fn ++ " " ++ show args ++ ")"
    show (TBool True _) = "#true"
    show (TBool False _) = "#false"
    show (TIf cond cnsq alt _) = "(if " ++ show cond ++ " " ++ show cnsq ++ show alt ++ ")"

tFVar :: String -> Term
tFVar name = TFVar (Text.pack name) emptyDbg

tBVar :: Int -> Term
tBVar index = TBVar index emptyDbg

tAbs :: Int -> Term -> Term
tAbs arity body = TAbs arity body [] emptyDbg

tApp :: Term -> [Term] -> Term
tApp fn args = TApp fn args emptyDbg

tTrue :: Term
tTrue = TBool True emptyDbg

tFalse :: Term
tFalse = TBool False emptyDbg

tIf :: Term -> Term -> Term -> Term
tIf cond cnsq alt = TIf cond cnsq alt emptyDbg


-- eval/apply
eval :: Term -> [Term] -> Term
eval (TBVar index _) stack | index < length stack = stack !! index
eval (TApp fn args dbg) stack =
    let fn' = eval fn stack
    in let args' = map (`eval` stack) args
    in apply fn' args' dbg stack
eval (TIf cond cnsq alt dbg) stack = evalIf cond cnsq alt dbg stack
eval t _ = t

apply :: Term -> [Term] -> Dbg -> [Term] -> Term
apply abs@(TAbs arity body env _) args _ stack
    | length env + length args == arity = eval body (reverse args ++ env ++ stack)
    | length env + length args < arity = abs {tAbsEnv = reverse args ++ env}
    | otherwise = throw TermException
apply _ _ _ _ = throw TermException

evalIf :: Term -> Term -> Term -> Dbg -> [Term] -> Term
evalIf cond cnsq alt _ stack
    | isTrue (eval cond stack) = eval cnsq stack
    | isFalse (eval cond stack) = eval alt stack
    | otherwise = throw TermException

isTrue :: Term -> Bool
isTrue (TBool True _) = True
isTrue _ = False

isFalse :: Term -> Bool
isFalse (TBool False _) = True
isFalse _ = False


-- combinators
s :: Term
s = tAbs 3 (tApp (tApp (tBVar 2) [tBVar 0]) [tApp (tBVar 1) [tBVar 0]])

k :: Term
k = tAbs 2 (tBVar 1)

i :: Term
i = tAbs 1 (tBVar 0)

b :: Term
b = tAbs 3 (tApp (tBVar 2) [tApp (tBVar 1) [tBVar 0]])

c :: Term
c = tAbs 3 (tApp (tBVar 2) [tBVar 0, tBVar 1])

w :: Term
w = tAbs 2 (tApp (tBVar 1) [tBVar 0, tBVar 0])


-- tokenizer
data Token
    = Lparen {lpDbg :: Dbg}
    | Rparen {rpDbg :: Dbg}
    | Lambda {laDbg :: Dbg}
    | ToIf {toIfDbg :: Dbg}
    | ToTrue {toTDbg :: Dbg}
    | ToFalse {toFDbg :: Dbg}
    | ToVar {name :: Text.Text, toVDbg :: Dbg}
    deriving (Eq, Show)

getTokenDbg :: Token -> Dbg
getTokenDbg (Lparen dbg) = dbg
getTokenDbg (Rparen dbg) = dbg
getTokenDbg (Lambda dbg) = dbg
getTokenDbg (ToIf dbg) = dbg
getTokenDbg (ToTrue dbg) = dbg
getTokenDbg (ToFalse dbg) = dbg
getTokenDbg (ToVar _ dbg) = dbg

specialChars :: [Char]
specialChars = ['(', ')']

isSpecialChar :: Char -> Bool
isSpecialChar char = char `elem` specialChars

keywords :: Map.HashMap Text.Text (Dbg -> Token)
keywords = (Map.fromList . map (TupleOps.app1 Text.pack))
    [ ("lambda", Lambda)
    , ("if", ToIf)
    , ("#true", ToTrue)
    , ("#false", ToFalse) ]

getKeywordOrVar :: Text.Text -> Dbg -> Token
getKeywordOrVar lexeme dbg =
    let keyword = Map.lookup lexeme keywords
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
        in if length == 0 then Nothing else Just (getKeywordOrVar lexeme (dbg length), rest)

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

newtype ParseError = ParseError {peMessage :: Text.Text} deriving (Eq, Show)

data PTerm
    = PVar {pvName :: Text.Text, pvDbg :: Dbg}
    | PLambda {plVars :: [PTerm], plBody :: PTerm, plDbg :: Dbg}
    | PApp {paFun :: PTerm, paArgs :: [PTerm], paDbg :: Dbg}
    | PBool {pbBool :: Bool, pbDbg :: Dbg}
    | PIf {piCond :: PTerm, piCnsq :: PTerm, piAlt :: PTerm, piDbg :: Dbg}
    deriving (Eq, Show)

parseError :: String -> String -> ParseError
parseError function expected =
    let location = "Parse error at " ++ function ++ ": "
    in let message = "expected " ++ expected ++ " but got something else"
    in ParseError (Text.pack (location ++ message))

parseTerm :: [Token] -> (Either ParseError PTerm, [Token])
parseTerm [] = (Left (ParseError (Text.pack "token list was empty")), [])
parseTerm (ToVar name dbg : rest) = (Right (PVar name dbg), rest)
parseTerm (ToTrue dbg : rest) = (Right (PBool True dbg), rest)
parseTerm (ToFalse dbg : rest) = (Right (PBool False dbg), rest)
parseTerm tokens@(Lparen _ : Lambda _ : _) = parseLambda tokens
parseTerm tokens@(Lparen _ : ToIf _ : _) = parseIf tokens
parseTerm tokens@(Lparen _ : _) = parseApp tokens

parseLambda :: [Token] -> (Either ParseError PTerm, [Token])
parseLambda tokens@(Lparen (Dbg start _) : Lambda _ : Lparen _ : rest)
    | (Right vars, rest2) <- parseVars rest
    , (Right body, Rparen (Dbg _ end) : rest3) <- parseTerm rest2 =
        (Right (PLambda vars body (Dbg start end)), rest3) -- success
    | (Left error, _) <- parseVars rest = (Left error, tokens) -- error parsing vars
    | (Right vars, rest2) <- parseVars rest
    , (Right body, _) <- parseTerm rest2 =
        (Left (parseError "parseLambda" ")"), tokens) --
parseLambda tokens = (Left (parseError "parseLambda" "(lambda (<var>*) <term>)"), tokens)

parseVars :: [Token] -> (Either ParseError [PTerm], [Token])
parseVars tokens
    | (vars, Rparen _ : rest) <- span isPVar tokens =
        let vars2 = map pVarOfToVar vars
        in (Right vars2, rest)
    | otherwise =
        (Left (parseError "parseVars" ")"), tokens)
        where
                pVarOfToVar (ToVar name dbg) = PVar name dbg
                pVarOfToVar _ = throw TermException
                isPVar (ToVar _ _) = True
                isPVar _ = False

parseIf :: [Token] -> (Either ParseError PTerm, [Token])
parseIf tokens@(Lparen (Dbg start _) : ToIf _ : rest)
    | (Right cond, rest2) <- parseTerm rest
    , (Right cnsq, rest3) <- parseTerm rest2
    , (Right alt, Rparen (Dbg _ end) : rest4) <- parseTerm rest3 =
        (Right (PIf cond cnsq alt (Dbg start end)), rest4)
    | otherwise = (Left (parseError "parseIf" "(if <term> <term> <term>)"), tokens)

parseApp :: [Token] -> (Either ParseError PTerm, [Token])
parseApp tokens@(Lparen (Dbg start _) : rest)
    | (Right fun, rest2) <- parseTerm rest
    , (Right args, Rparen (Dbg _ end) : rest3) <- parseTerms rest2 =
        (Right (PApp fun args (Dbg start end)), rest3)
    | otherwise = (Left (parseError "parseApp" "(<term> <term>*)"), tokens)

parseTerms :: [Token] -> (Either ParseError [PTerm], [Token])
parseTerms rest@(Rparen _ : _) = (Right [], rest)
parseTerms tokens
    | (Right term, rest) <- parseTerm tokens
    , (Right tail, rest2) <- parseTerms rest =
        (Right (term : tail), rest2)
    | otherwise = (Left (parseError "parseTerms" "<term>*"), tokens)

termOfPTerm :: PTerm -> [Text.Text] -> Term
termOfPTerm (PVar name dbg) context
    | (Just index) <- elemIndex name context = TBVar index dbg
    | otherwise = TFVar name dbg
termOfPTerm (PLambda vars body dbg) context =
    let body2 = termOfPTerm body (reverse (map pvName vars) ++ context)
    in TAbs (length vars) body2 [] dbg
termOfPTerm (PApp fun args dbg) context =
    let fun2 = termOfPTerm fun context
    in let args2 = map (`termOfPTerm` context) args
    in TApp fun2 args2 dbg
termOfPTerm (PBool value dbg) context = TBool value dbg
termOfPTerm (PIf cond cnsq alt dbg) context =
    let cond2 = termOfPTerm cond context
    in let cnsq2 = termOfPTerm cnsq context
    in let alt2 = termOfPTerm alt context
    in TIf cond2 cnsq2 alt2 dbg

evalString :: String -> Maybe Term
evalString input
    | (Right term, []) <- parseTerm (tokenize (Text.pack input)) = Just (eval (termOfPTerm term []) [])
    | otherwise = Nothing