module Term where
import qualified Data.Text as Text

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
    | TApp {tAppFn :: Term, tAppArgs :: [Term], tAppDbg :: Dbg}
    | TLet {tLetVal :: Term, tLetBody :: Term, tLetName :: Text.Text, tLetDbg :: Dbg}
    | TBool {boolValue :: Bool, boolDbg :: Dbg}
    | TIf {ifCond :: Term, ifCnsq :: Term, ifAlt :: Term, ifDbg :: Dbg}
    | TInt {intValue :: Integer, intDbg :: Dbg}
    | TUnit {tUnitDbg :: Dbg}
    deriving Eq

instance Show Term where
    show (TFVar name _) = Text.unpack name
    show (TBVar _ name _) = Text.unpack name
    show (TAbs _ body [] varNames _) = "(lambda (" ++ Text.unpack (Text.unwords varNames) ++ ") " ++ show body ++ ")"
    show abs@(TAbs _ _ env _ _) = "(closure (" ++ unwords (map show env)  ++ ") (" ++ show (abs {tAbsEnv = []}) ++ "))"
    show (TLet val body name _) = "(let " ++ Text.unpack name ++ " " ++ show val ++ ")"
    show (TApp fn args _) = "(app " ++ show fn ++ " " ++ show args ++ ")"
    show (TBool True _) = "#true"
    show (TBool False _) = "#false"
    show (TIf cond cnsq alt _) = "(if " ++ show cond ++ " " ++ show cnsq ++ " " ++ show alt ++ ")"
    show (TInt value _) = show value
    show (TUnit _) = "()"
        where
            showLet (TLet val body name _) =
                "(let " ++ Text.unpack name ++ " " ++ show val ++ ")"
                ++ " " ++ showLet body