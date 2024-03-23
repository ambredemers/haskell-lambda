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
data Dbg = Dbg { dSource :: Text.Text, dStart :: Int, dLength :: Int } deriving Eq

emptyDbg :: Dbg
emptyDbg = Dbg { dSource = Text.empty, dStart = 0, dLength = 0 }


-- term exception
data TermException = TermException deriving (Show, Typeable)

instance Exception TermException


-- term
data Term
    = Fvar { fvarName :: Text.Text, fvarDbg :: Dbg }
    | Bvar { bvarIndex :: Int, barDbg :: Dbg }
    | Abs { absArity :: Int, absBody :: Term, absEnv :: [Term], absDbg :: Dbg }
    | App { appFn :: Term, appArgs :: [Term], appDbg :: Dbg }
    | Tbool { boolValue :: Bool, boolDbg :: Dbg }
    | Tif { ifCond :: Term, ifCnsq :: Term, ifAlt :: Term, ifDbg :: Dbg }
    deriving Eq

instance Show Term where
    show (Fvar name _) = "(fvar " ++ show name ++ ")"
    show (Bvar index _) = "(bvar " ++ show index ++ ")"
    show (Abs arity body _ _) = "(abs " ++ show arity ++ " " ++ show body ++ ")"
    show (App fn args _) = "(app " ++ show fn ++ " " ++ show args ++ ")"
    show (Tbool True _) = "#true"
    show (Tbool False _) = "#false"
    show (Tif cond cnsq alt _) = "(if " ++ show cond ++ " " ++ show cnsq ++ show alt ++ ")"

fvar :: String -> Term
fvar name = Fvar (Text.pack name) emptyDbg

bvar :: Int -> Term
bvar index = Bvar index emptyDbg

tabs :: Int -> Term -> Term
tabs arity body = Abs arity body [] emptyDbg

app :: Term -> [Term] -> Term
app fn args = App fn args emptyDbg

ttrue = Tbool True emptyDbg

tfalse = Tbool False emptyDbg

tif :: Term -> Term -> Term -> Term
tif cond cnsq alt = Tif cond cnsq alt emptyDbg


-- eval/apply
eval :: Term -> [Term] -> Term
eval (Bvar index _) stack | index < length stack = stack !! index
eval (App fn args dbg) stack =
    let fn' = eval fn stack
    in let args' = map (`eval` stack) args
    in apply fn' args' dbg stack
eval (Tif cond cnsq alt dbg) stack = evalIf cond cnsq alt dbg stack
eval t _ = t

apply :: Term -> [Term] -> Dbg -> [Term] -> Term
apply abs@(Abs arity body env _) args _ stack
    | length env + length args == arity = eval body (reverse args ++ env ++ stack)
    | length env + length args < arity = abs { absEnv = reverse args ++ env }
    | otherwise = throw TermException
apply _ _ _ _ = throw TermException

evalIf :: Term -> Term -> Term -> Dbg -> [Term] -> Term
evalIf cond cnsq alt _ stack
    | isTrue (eval cond stack) = eval cnsq stack
    | isFalse (eval cond stack) = eval alt stack
    | otherwise = throw TermException

isTrue :: Term -> Bool
isTrue (Tbool True _) = True
isTrue _ = False

isFalse :: Term -> Bool
isFalse (Tbool False _) = True
isFalse _ = False


-- combinators
s :: Term
s = tabs 3 (app (app (bvar 2) [bvar 0]) [app (bvar 1) [bvar 0]])

k :: Term
k = tabs 2 (bvar 1)

i :: Term
i = tabs 1 (bvar 0)

b :: Term
b = tabs 3 (app (bvar 2) [app (bvar 1) [bvar 0]])

c :: Term
c = tabs 3 (app (bvar 2) [bvar 0, bvar 1])

w :: Term
w = tabs 2 (app (bvar 1) [bvar 0, bvar 0])

-- tokenizer
data Token
    = Lparen { lpDbg :: Dbg }
    | Rparen { rpDbg :: Dbg }
    | Lambda { laDbg :: Dbg }
    | ToIf { toIfDbg :: Dbg }
    | ToTrue { toTDbg :: Dbg }
    | ToFalse { toFDbg :: Dbg }
    | ToVar { name :: Text.Text, toVDbg :: Dbg }

specialChars :: Set.HashSet Char
specialChars = Set.fromList ['(', ')', ' ', '\t', '\n', '\r', '\f', '\v']

keywords :: Map.HashMap Text.Text (Dbg -> Token)
keywords = (Map.fromList . map (TupleOps.app1 Text.pack))
    [ ("lambda", Lambda)
    , ("if", ToIf)
    , ("#true", ToTrue)
    , ("#false", ToFalse) ]


breakLength :: (Char -> Bool) -> Text.Text -> (Text.Text, Int, Text.Text) 
breakLength predicate text =
    let (head, tail) = Text.break predicate text
    in let length = Text.length head
    in (head, length, tail)

dropWhileLength :: (Char -> Bool) -> Text.Text -> (Text.Text, Int)
dropWhileLength predicate input =
    f input 0
    where f text index
            | Just (head, tail) <- Text.uncons text, predicate head = f tail (index + 1)
            | otherwise = (text, index)


getToken :: Text.Text -> Dbg -> Token
getToken input dbg =
    let (lexeme, length, rest) = breakLength (`Set.member` specialChars) input
    in let dbg2 = dbg{dLength = length}
    in fromMaybe (ToVar lexeme) (Map.lookup lexeme keywords) dbg2