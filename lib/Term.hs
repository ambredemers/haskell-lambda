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
  = TermFvar {tFvarName :: Text.Text, tFvarDbg :: Dbg}
  | TermBvar {tBvarIndex :: Int, tBvarName :: Text.Text, tBvarDbg :: Dbg}
  | TermAbs {tAbsBody :: Term, tAbsEnv :: [Term], tAbsVarNames :: [Text.Text], tAbsDbg :: Dbg}
  | TermBool {tBoolValue :: Bool, tBoolDbg :: Dbg}
  | TermInt {tIntValue :: Integer, tIntDbg :: Dbg}
  | TermUnit {tUnitDbg :: Dbg}
  | TermApp {tAppFn :: Term, tAppArgs :: [Term], tAppDbg :: Dbg}
  | TermLet {tLetBindings :: [LetBinding], tLetBody :: Term, tLetDbg :: Dbg}
  | TermIf {tIfCond :: Term, tIfCnsq :: Term, tIfAlt :: Term, tIfDbg :: Dbg}
  deriving (Eq)

data LetBinding = LetBinding {lbValue :: Term, lbName :: Text.Text, lbDbg :: Dbg} deriving (Eq)

instance Show Term where
  show (TermFvar name _) = Text.unpack name
  show (TermBvar _ name _) = Text.unpack name
  show (TermAbs body [] varNames _) = "(lambda (" ++ Text.unpack (Text.unwords varNames) ++ ") " ++ show body ++ ")"
  show abs@(TermAbs _ env _ _) = "(closure (" ++ unwords (map show env) ++ ") (" ++ show (abs {tAbsEnv = []}) ++ "))"
  --   show letTerm@(TLet val body name _) = "(block " ++ showLet letTerm ++ ")"
  --     where
  --       showLet (TLet val body name _) = "(let " ++ Text.unpack name ++ " " ++ show val ++ ")" ++ " " ++ showLet body
  --       showLet term = show term
  show (TermApp fn args _) = "(" ++ show fn ++ foldl (\x y -> x ++ " " ++ show y) "" args ++ ")"
  show (TermBool True _) = "#true"
  show (TermBool False _) = "#false"
  show (TermIf cond cnsq alt _) = "(if " ++ show cond ++ " " ++ show cnsq ++ " " ++ show alt ++ ")"
  show (TermInt value _) = show value
  show (TermUnit _) = "()"

getTermDbg :: Term -> Dbg
getTermDbg (TermFvar _ dbg) = dbg
getTermDbg (TermBvar _ _ dbg) = dbg
getTermDbg (TermAbs _ _ _ dbg) = dbg
getTermDbg (TermApp _ _ dbg) = dbg
getTermDbg (TermLet _ _ dbg) = dbg
getTermDbg (TermBool _ dbg) = dbg
getTermDbg (TermIf _ _ _ dbg) = dbg
getTermDbg (TermInt _ dbg) = dbg
getTermDbg (TermUnit dbg) = dbg