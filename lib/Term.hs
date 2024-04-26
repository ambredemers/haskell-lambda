module Term where
import Control.Exception
import Data.Data
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Text as Text
import qualified Data.Tuple.Ops as TupleOps
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