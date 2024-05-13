{-# LANGUAGE Strict #-}

module InterpretTerm where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Either
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Tuple.Ops as TupleOps
import Parser
import Term

-- eval
data EvalContext = EvalContext {ecStack :: [Term], ecSource :: Text.Text}

type EvalType = EitherT String (Reader EvalContext)

localContext :: ([Term] -> [Term]) -> EvalType a -> EvalType a
localContext f e = do EvalContext stack _ <- ask; local (\s -> s {ecStack = f stack}) e

evalError :: String -> String -> Dbg -> EvalType a
evalError e message dbg = do
  EvalContext _ input <- ask
  let message' = "Could not evaluate " ++ e ++ "; " ++ message
  throwError (makeErrorString input dbg message')

evalErrorExpected :: String -> String -> String -> Dbg -> EvalType a
evalErrorExpected location expected actual = evalError location message
  where
    message = "expected " ++ expected ++ ", but got " ++ actual

evalTerm :: Term -> EvalType Term
evalTerm fvar@(TermFvar _ _) = return fvar
evalTerm (TermBvar index _ dbg) = do
  EvalContext stack _ <- ask
  if index < length stack
    then return $ stack !! index
    else evalError "bound variable" ("invalid index " ++ show index) dbg
evalTerm tAbs@(TermAbs {}) = return tAbs
evalTerm (TermApp fn args dbg) = do
  fn' <- evalTerm fn
  args' <- evalTerms args
  apply fn' args' dbg
evalTerm tlet@(TermLet (LetBinding value _ _ : rest) _ _) = do
  value' <- evalTerm value
  localContext (value' :) (evalTerm (tlet {tLetBindings = rest}))
evalTerm (TermLet [] body _) = evalTerm body
evalTerm bool@(TermBool {}) = return bool
evalTerm (TermIf cond cnsq alt dbg) = evalIf cond cnsq alt dbg
evalTerm int@(TermInt _ _) = return int
evalTerm term = evalError "evaluating term" ("got an unexpected term " ++ show term) (getTermDbg term)

evalTerms :: [Term] -> EvalType [Term]
evalTerms (term : rest) = do
  term' <- evalTerm term
  rest' <- evalTerms rest
  return $ term' : rest'
evalTerms [] = return []

evalIf :: Term -> Term -> Term -> Dbg -> EvalType Term
evalIf cond cnsq alt _ = do
  result <- evalTerm cond
  case result of
    TermBool True _ -> evalTerm cnsq
    TermBool False _ -> evalTerm alt
    term -> evalErrorExpected "if expression" "condition to be a boolean" (show term) (getTermDbg term)

-- apply
intBinaryOps :: Map.HashMap Text.Text (Integer -> Integer -> Dbg -> EvalType Term)
intBinaryOps =
  (Map.fromList . map (TupleOps.app1 Text.pack))
    [ ("+", \l r d -> return $ TermInt (l + r) d),
      ("-", \l r d -> return $ TermInt (l - r) d),
      ("*", \l r d -> return $ TermInt (l * r) d),
      ("/", \l r d -> return $ TermInt (quot l r) d),
      ("<", \l r d -> return $ TermBool (l < r) d),
      ("<=", \l r d -> return $ TermBool (l <= r) d),
      ("=", \l r d -> return $ TermBool (l == r) d),
      (">", \l r d -> return $ TermBool (l > r) d),
      (">=", \l r d -> return $ TermBool (l >= r) d),
      ("/=", \l r d -> return $ TermBool (l /= r) d)
    ]

apply :: Term -> [Term] -> Dbg -> EvalType Term
apply tAbs@(TermAbs body env vars _) args dbg
  | length env + length args == length vars = localContext (\s -> reverse args ++ env ++ s) (evalTerm body)
  | length env + length args < length vars = return $ tAbs {tAbsEnv = reverse args ++ env}
  | otherwise = do EvalContext _ _ <- ask; evalErrorExpected "function" expectedArgs actualArgs dbg
  where
    expectedArgs = show (length vars - length env) ++ " arguments"
    actualArgs = show $ length env + length args
apply (TermFvar opName _) [l, r] dbg | Map.member opName intBinaryOps = do
  l' <- evalTerm l
  r' <- evalTerm r
  case (l', r') of
    (TermInt lVal (Dbg start _), TermInt rVal (Dbg _ end)) ->
      (intBinaryOps Map.! opName) lVal rVal (Dbg start end)
    (TermInt _ _, rTerm) -> evalErrorExpected (Text.unpack opName) "right argument to be an integer" (show rTerm) dbg
    (lTerm, TermInt _ _) -> evalErrorExpected (Text.unpack opName) "left argument to be an integer" (show lTerm) dbg
    (lTerm, rTerm) -> evalErrorExpected (Text.unpack opName) "argument to be integers" (show lTerm ++ " and " ++ show rTerm) dbg
apply term _ dbg = evalErrorExpected "function" "a lambda or primitive function" (show term) dbg

eval :: Term -> Text.Text -> Either String Term
eval term input =
  runReader (runEitherT (evalTerm term)) (EvalContext [] input)

-- evalString
evalString :: String -> Either String Term
evalString input = do term <- parse (Text.pack input); eval term (Text.pack input)