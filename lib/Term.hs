{-# LANGUAGE Strict #-}

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
  "Error at " ++ lexeme ++ " (line " ++ show (line + 1) ++ ", column " ++ show (column + 1) ++ "):\n\t" ++ message
  where
    lexeme = Text.unpack (getStringAtDbg input dbg)
    (prefix, rest) = Text.splitAt start input
    prefixLines = Text.lines prefix
    line = if null prefixLines then 0 else length prefixLines - 1
    lastline = if null prefixLines then Text.empty else last prefixLines
    column = Text.length lastline

-- term
data Term
  = TFVar {tFVarName :: Text.Text, tfVarDbg :: Dbg}
  | TBVar {tBvarIndex :: Int, tBVarName :: Text.Text, barDbg :: Dbg}
  | TAbs {tAbsArity :: Int, tAbsBody :: Term, tAbsEnv :: [Term], tAbsVarNames :: [Text.Text], tAbsDbg :: Dbg}
  | TApp {tAppFn :: Term, tAppArgs :: [Term], tAppDbg :: Dbg}
  | TLet {tLetVals :: [LetBinding], tLetBody :: Term, tLetDbg :: Dbg}
  | TBool {boolValue :: Bool, boolDbg :: Dbg}
  | TIf {ifCond :: Term, ifCnsq :: Term, ifAlt :: Term, ifDbg :: Dbg}
  | TInt {intValue :: Integer, intDbg :: Dbg}
  | TUnit {tUnitDbg :: Dbg}
  deriving Eq

data LetBinding = LetBinding {lbValue :: Term, lbName :: Text.Text, lbDbg :: Dbg} deriving Eq

instance Show Term where
  show (TFVar name _) = Text.unpack name
  show (TBVar _ name _) = Text.unpack name
  show (TAbs _ body [] varNames _) = "(lambda (" ++ Text.unpack (Text.unwords varNames) ++ ") " ++ show body ++ ")"
  show abs@(TAbs _ _ env _ _) = "(closure (" ++ unwords (map show env) ++ ") (" ++ show (abs {tAbsEnv = []}) ++ "))"
--   show letTerm@(TLet val body name _) = "(block " ++ showLet letTerm ++ ")"
--     where
--       showLet (TLet val body name _) = "(let " ++ Text.unpack name ++ " " ++ show val ++ ")" ++ " " ++ showLet body
--       showLet term = show term
  show (TApp fn args _) = "(" ++ show fn ++ foldl (\x y -> x ++ " " ++ show y) "" args ++ ")"
  show (TBool True _) = "#true"
  show (TBool False _) = "#false"
  show (TIf cond cnsq alt _) = "(if " ++ show cond ++ " " ++ show cnsq ++ " " ++ show alt ++ ")"
  show (TInt value _) = show value
  show (TUnit _) = "()"

getTermDbg :: Term -> Dbg
getTermDbg (TFVar _ dbg) = dbg
getTermDbg (TBVar _ _ dbg) = dbg
getTermDbg (TAbs _ _ _ _ dbg) = dbg
getTermDbg (TApp _ _ dbg) = dbg
getTermDbg (TLet _ _ dbg) = dbg
getTermDbg (TBool _ dbg) = dbg
getTermDbg (TIf _ _ _ dbg) = dbg
getTermDbg (TInt _ dbg) = dbg
getTermDbg (TUnit dbg) = dbg