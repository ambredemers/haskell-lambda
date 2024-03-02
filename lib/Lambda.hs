module Lambda where
import Control.Exception
import Data.Data

-- debug info
data Dbg = Dbg { source :: String, dindex :: Int } deriving Eq

emptyDbg :: Dbg
emptyDbg = Dbg { source = "", dindex = 0 }


-- term exception
data TermException = TermException deriving (Show, Typeable)

instance Exception TermException


-- term
data Term
    = Fvar { fvarName :: String, fvarDbg :: Dbg }
    | Bvar { bvarIndex :: Int, barDbg :: Dbg }
    | Abs { absArity :: Int, absBody :: Term, absEnv :: [Term], absDbg :: Dbg }
    | App { appFn :: Term, appArgs :: [Term], appDbg :: Dbg }
    | Tbool { boolValue :: Bool, boolDbg :: Dbg }
    | Tif { ifCond :: Term, ifCnsq :: Term, ifAlt :: Term, ifDbg :: Dbg }
    deriving Eq

instance Show Term where
    show (Fvar name _) = "(fvar " ++ name ++ ")"
    show (Bvar index _) = "(bvar " ++ show index ++ ")"
    show (Abs arity body _ _) = "(abs " ++ show arity ++ " " ++ show body ++ ")"
    show (App fn args _) = "(app " ++ show fn ++ " " ++ show args ++ ")"
    show (Tbool True _) = "#true"
    show (Tbool False _) = "#false"
    show (Tif cond cnsq alt _) = "(if " ++ show cond ++ " " ++ show cnsq ++ show alt ++ ")"

fvar :: String -> Term
fvar name = Fvar name emptyDbg

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