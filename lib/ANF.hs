{-# LANGUAGE Strict #-}

module ANF where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashSet as Set
import Data.List
import qualified Data.Text as Text
import qualified Parser
import Term

data AnfExp
  = AVal {avVal :: AnfVal}
  | ALet {aleValue :: AnfVal, aleBody :: AnfExp, aleName :: Text.Text, aleDbg :: Dbg}
  | ALetApp {alaFun :: AnfVal, alaArgs :: [AnfVal], alaBody :: AnfExp, alaName :: Text.Text, alaDbg :: Dbg}
  | ALetIf {aiCond :: AnfVal, aiCnsq :: AnfExp, aiAlt :: AnfExp, aiBody :: AnfExp, aiName :: Text.Text, aifDbg :: Dbg}
  deriving (Eq)

data AnfVal
  = AFVar {afvName :: Text.Text, afvDbg :: Dbg}
  | ABVar {abvIndex :: Int, abvName :: Text.Text, abvDbg :: Dbg}
  | AAbs {aabsBody :: AnfExp, aabsEnv :: [AnfVal], aabsVars :: [Text.Text], aabsDbg :: Dbg}
  | ABool {abBool :: Bool, abDbg :: Dbg}
  | AInt {ainValue :: Integer, ainDbg :: Dbg}
  | AUnit {auDbg :: Dbg}
  deriving (Eq)

instance Show AnfExp where
  show (AVal v) = show v
  show (ALet value body name _) = "(let " ++ Text.unpack name ++ " " ++ show value ++ " " ++ show body ++ ")"
  show (ALetApp fn args body name _) =
    "(let " ++ Text.unpack name ++ " (" ++ show fn ++ " " ++ unwords (map show args) ++ ") " ++ show body ++ ")"
  show (ALetIf cond cnsq alt body name _) = "(let " ++ Text.unpack name ++ ifString ++ show body ++ ")"
    where
      ifString = " (if " ++ show cond ++ " " ++ show cnsq ++ " " ++ show alt ++ ") "

instance Show AnfVal where
  show (AFVar name _) = Text.unpack name
  show (ABVar index name _) = Text.unpack name
  show (AAbs body _ vars _) = "(lambda (" ++ Text.unpack (Text.unwords vars) ++ ") " ++ show body ++ ")"
  show (ABool True _) = "#true"
  show (ABool False _) = "#false"
  show (AInt value _) = show value
  show (AUnit _) = "()"

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
lowerTermToAnfVal (TFVar name dbg) = return (AFVar name dbg, id)
lowerTermToAnfVal (TBVar _ name dbg) = return (AFVar name dbg, id)
lowerTermToAnfVal (TAbs body _ varNames dbg) = do
  body' <- lowerTermToAnfExp body
  return (AAbs body' [] varNames dbg, id)
lowerTermToAnfVal (TApp fn args dbg) = do
  name <- genSym
  (fn', binder) <- lowerTermToAnfVal fn
  (args', binder') <- lowerTermsToAnfVals args
  return (AFVar name dbg, \body -> binder $ binder' $ ALetApp fn' args' body name dbg)
lowerTermToAnfVal tlet@(TLet (LetBinding value name dbg : rest) _ _) = do
  (value', binder) <- lowerTermToAnfVal value
  (result, binder') <- lowerTermToAnfVal (tlet {tLetVals = rest})
  return (result, \next -> binder $ ALet value' (binder' next) name dbg)
lowerTermToAnfVal (TLet [] body dbg) = do
  name <- genSym
  (body', binder) <- lowerTermToAnfVal body
  return (AFVar name dbg, \next -> binder $ ALet body' next name dbg)
lowerTermToAnfVal (TBool value dbg) = return (ABool value dbg, id)
lowerTermToAnfVal (TIf cond cnsq alt dbg) = do
  name <- genSym
  (cond', binder) <- lowerTermToAnfVal cond
  cnsq' <- lowerTermToAnfExp cnsq
  alt' <- lowerTermToAnfExp alt
  return (AFVar name dbg, \body -> binder $ ALetIf cond' cnsq' alt' body name dbg)
lowerTermToAnfVal (TInt value dbg) = return (AInt value dbg, id)
lowerTermToAnfVal (TUnit dbg) = return (AUnit dbg, id)

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
lowerTermToAnfExp term = do (val, binder) <- lowerTermToAnfVal term; return $ binder $ AVal val

-- replace bound variables with indices
bindAnfExpVars :: AnfExp -> Reader [Text.Text] AnfExp
bindAnfExpVars (AVal val) = do val' <- bindAnfValVars val; return $ AVal val'
bindAnfExpVars (ALet value body name dbg) = do
  context <- ask
  value' <- bindAnfValVars value
  body' <- local (name :) (bindAnfExpVars body)
  return $ ALet value' body' name dbg
bindAnfExpVars (ALetApp fn args next name dbg) = do
  fn' <- bindAnfValVars fn
  args' <- bindAnfValVarsMap args
  next' <- local (name :) (bindAnfExpVars next)
  return $ ALetApp fn' args' next' name dbg
bindAnfExpVars (ALetIf cond cnsq alt next name dbg) = do
  cond' <- bindAnfValVars cond
  cnsq' <- bindAnfExpVars cnsq
  alt' <- bindAnfExpVars alt
  next' <- local (name :) (bindAnfExpVars next)
  return $ ALetIf cond' cnsq' alt' next' name dbg

bindAnfValVars :: AnfVal -> Reader [Text.Text] AnfVal
bindAnfValVars (AFVar name dbg) = do
  context <- ask
  case elemIndex name context of
    Just index -> return $ ABVar index name dbg
    _ -> return $ AFVar name dbg
bindAnfValVars (ABVar _ name dbg) = do
  context <- ask
  case elemIndex name context of
    Just index -> return $ ABVar index name dbg
    _ -> return $ AFVar name dbg
bindAnfValVars aabs@(AAbs body _ varNames _) = do
  body' <- local (reverse varNames ++) (bindAnfExpVars body)
  return $ aabs {aabsBody = body'}
bindAnfValVars val = return val

bindAnfValVarsMap :: [AnfVal] -> Reader [Text.Text] [AnfVal]
bindAnfValVarsMap (val : rest) = do
  val' <- bindAnfValVars val
  rest' <- bindAnfValVarsMap rest
  return $ val' : rest'
bindAnfValVarsMap [] = return []

lowerTermToAnf :: Term -> AnfExp
lowerTermToAnf term =
  let exp = evalState (lowerTermToAnfExp term) Set.empty
   in runReader (bindAnfExpVars exp) []

parseToAnf :: Text.Text -> Either String AnfExp
parseToAnf input = do
  term <- Parser.parse input
  return $ lowerTermToAnf term