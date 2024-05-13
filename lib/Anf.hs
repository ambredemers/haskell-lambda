{-# LANGUAGE Strict #-}

module Anf where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashSet as Set
import Data.List
import qualified Data.Text as Text
import qualified Parser
import Term

data AnfExp
  = AnfValue {aValVal :: AnfVal}
  | AnfLet {aLetValue :: AnfVal, aLetBody :: AnfExp, aLetName :: Text.Text, aLetDbg :: Dbg}
  | AnfLetApp {aLetAppFun :: AnfVal, aLetAppArgs :: [AnfVal], aLetAppBody :: AnfExp, aLetAppName :: Text.Text, aLetAppDbg :: Dbg}
  | AnfIf {aIfCond :: AnfVal, aIfCnsq :: AnfExp, aIfAlt :: AnfExp, aIfDbg :: Dbg}
  deriving (Eq)

data AnfVal
  = AnfFvar {aFvarName :: Text.Text, aFvarDbg :: Dbg}
  | AnfBvar {aBvarIndex :: Int, aBvarName :: Text.Text, aBvarDbg :: Dbg}
  | AnfAbs {aAbsBody :: AnfExp, aAbsEnv :: [AnfVal], aAbsVars :: [Text.Text], aAbsDbg :: Dbg}
  | AnfBool {aBoolVal :: Bool, aBoolDbg :: Dbg}
  | AnfInt {aIntValue :: Integer, aIntDbg :: Dbg}
  | AnfUnit {aUnitDbg :: Dbg}
  deriving (Eq)

instance Show AnfExp where
  show (AnfValue v) = show v
  show (AnfLet value body name _) = "(let " ++ Text.unpack name ++ " " ++ show value ++ " " ++ show body ++ ")"
  show (AnfLetApp fn args body name _) =
    "(let " ++ Text.unpack name ++ " (" ++ show fn ++ " " ++ unwords (map show args) ++ ") " ++ show body ++ ")"
  show (AnfIf cond cnsq alt _) = "(if " ++ show cond ++ " " ++ show cnsq ++ " " ++ show alt ++ ")"

instance Show AnfVal where
  show (AnfFvar name _) = Text.unpack name
  show (AnfBvar _ name _) = Text.unpack name
  show (AnfAbs body _ vars _) = "(lambda (" ++ Text.unpack (Text.unwords vars) ++ ") " ++ show body ++ ")"
  show (AnfBool True _) = "#true"
  show (AnfBool False _) = "#false"
  show (AnfInt value _) = show value
  show (AnfUnit _) = "()"

-- lower Term to ANF
type AnfM = State (Set.HashSet Text.Text)

genSym :: AnfM Text.Text
genSym = f 0
  where
    f :: Int -> AnfM Text.Text
    f n = do
      syms <- get
      let sym = Text.pack ("x" ++ show n)
      if Set.member sym syms
        then f (n + 1)
        else do modify (Set.insert (Text.pack ("x" ++ show n))); return sym

lowerTermToAnfVal :: Term -> AnfM (AnfVal, AnfExp -> AnfExp)
lowerTermToAnfVal (TermFvar name dbg) = return (AnfFvar name dbg, id)
lowerTermToAnfVal (TermBvar _ name dbg) = return (AnfFvar name dbg, id)
lowerTermToAnfVal (TermAbs body _ varNames dbg) = do
  body' <- lowerTermToAnfExp body
  return (AnfAbs body' [] varNames dbg, id)
lowerTermToAnfVal (TermApp fn args dbg) = do
  name <- genSym
  (fn', binder) <- lowerTermToAnfVal fn
  (args', binder') <- lowerTermsToAnfVals args
  return (AnfFvar name dbg, \body -> binder $ binder' $ AnfLetApp fn' args' body name dbg)
lowerTermToAnfVal tlet@(TermLet (LetBinding value name dbg : rest) _ _) = do
  (value', binder) <- lowerTermToAnfVal value
  (result, binder') <- lowerTermToAnfVal (tlet {tLetBindings = rest})
  return (result, \next -> binder $ AnfLet value' (binder' next) name dbg)
lowerTermToAnfVal (TermLet [] body dbg) = do
  name <- genSym
  (body', binder) <- lowerTermToAnfVal body
  return (AnfFvar name dbg, \next -> binder $ AnfLet body' next name dbg)
lowerTermToAnfVal (TermBool value dbg) = return (AnfBool value dbg, id)
lowerTermToAnfVal (TermIf cond cnsq alt dbg) = do
  name <- genSym
  (cond', binder) <- lowerTermToAnfVal cond
  cnsq' <- lowerTermToAnfExp cnsq
  alt' <- lowerTermToAnfExp alt
  return (AnfFvar name dbg, \_ -> binder $ AnfIf cond' cnsq' alt' dbg)
lowerTermToAnfVal (TermInt value dbg) = return (AnfInt value dbg, id)
lowerTermToAnfVal (TermUnit dbg) = return (AnfUnit dbg, id)

--   | TApp {tAppFn :: Term, tAppArgs :: [Term], tAppDbg :: Dbg}
--   | TLet {tLetVals :: [LetBinding], tLetBody :: Term, tLetDbg :: Dbg}
--   | TIf {ifCond :: Term, ifCnsq :: Term, ifAlt :: Term, ifDbg :: Dbg}

lowerTermsToAnfVals :: [Term] -> AnfM ([AnfVal], AnfExp -> AnfExp)
lowerTermsToAnfVals (term : rest) = do
  (val, binder) <- lowerTermToAnfVal term
  (rest', binder') <- lowerTermsToAnfVals rest
  return (val : rest', binder . binder')
lowerTermsToAnfVals [] = return ([], id)

lowerTermToAnfExp :: Term -> AnfM AnfExp
lowerTermToAnfExp term = do (val, binder) <- lowerTermToAnfVal term; return $ binder $ AnfValue val

-- replace bound variables with indices
bindAnfExpVars :: AnfExp -> Reader [Text.Text] AnfExp
bindAnfExpVars (AnfValue val) = do val' <- bindAnfValVars val; return $ AnfValue val'
bindAnfExpVars (AnfLet value body name dbg) = do
  value' <- bindAnfValVars value
  body' <- local (name :) (bindAnfExpVars body)
  return $ AnfLet value' body' name dbg
bindAnfExpVars (AnfLetApp fn args next name dbg) = do
  fn' <- bindAnfValVars fn
  args' <- bindAnfValVarsMap args
  next' <- local (name :) (bindAnfExpVars next)
  return $ AnfLetApp fn' args' next' name dbg
bindAnfExpVars (AnfIf cond cnsq alt dbg) = do
  cond' <- bindAnfValVars cond
  cnsq' <- bindAnfExpVars cnsq
  alt' <- bindAnfExpVars alt
  return $ AnfIf cond' cnsq' alt' dbg

bindAnfValVars :: AnfVal -> Reader [Text.Text] AnfVal
bindAnfValVars (AnfFvar name dbg) = do
  context <- ask
  case elemIndex name context of
    Just index -> return $ AnfBvar index name dbg
    _ -> return $ AnfFvar name dbg
bindAnfValVars (AnfBvar _ name dbg) = do
  context <- ask
  case elemIndex name context of
    Just index -> return $ AnfBvar index name dbg
    _ -> return $ AnfFvar name dbg
bindAnfValVars aabs@(AnfAbs body _ varNames _) = do
  body' <- local (reverse varNames ++) (bindAnfExpVars body)
  return $ aabs {aAbsBody = body'}
bindAnfValVars val = return val

bindAnfValVarsMap :: [AnfVal] -> Reader [Text.Text] [AnfVal]
bindAnfValVarsMap (val : rest) = do
  val' <- bindAnfValVars val
  rest' <- bindAnfValVarsMap rest
  return $ val' : rest'
bindAnfValVarsMap [] = return []

lowerTermToAnf :: Term -> AnfExp
lowerTermToAnf term =
  let aExp = evalState (lowerTermToAnfExp term) Set.empty
   in runReader (bindAnfExpVars aExp) []

parseToAnf :: Text.Text -> Either String AnfExp
parseToAnf input = do
  term <- Parser.parse input
  return $ lowerTermToAnf term