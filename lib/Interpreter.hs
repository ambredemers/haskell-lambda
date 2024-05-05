{-# LANGUAGE Strict #-}

module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict
import Data.Either
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Tuple.Ops as TupleOps
import Parser
import Term

-- eval
data EvalContext = EvalContext {esStack :: [Term], esSource :: Text.Text}

type EvalType = EitherT String (Reader EvalContext)

localContext :: ([Term] -> [Term]) -> EvalType a -> EvalType a
localContext f e = do EvalContext stack _ <- ask; local (\s -> s {esStack = f stack}) e

evalError :: String -> String -> Dbg -> EvalType a
evalError e message dbg = do
  EvalContext _ input <- ask
  let message' = "Could not evaluate " ++ e ++ " - " ++ message
  throwError (makeErrorString input dbg message')

evalErrorExpected :: String -> String -> String -> Dbg -> EvalType a
evalErrorExpected location expected actual = evalError location message
  where message = "expected " ++ expected ++ ", but got " ++ actual

evalTerm :: Term -> EvalType Term
evalTerm fvar@(TFVar _ _) = return fvar
evalTerm (TBVar index _ dbg) = do
  EvalContext stack input <- ask
  if index < length stack
    then return $ stack !! index
    else evalError "bound variable" ("invalid index " ++ show index) dbg
evalTerm abs@(TAbs {}) = return abs
evalTerm (TApp fn args dbg) = do
  EvalContext _ input <- ask
  fn' <- evalTerm fn
  args' <- evalTerms args
  apply fn' args' dbg
evalTerm tlet@(TLet (LetBinding value _ _ : rest) body _) = do
  value' <- evalTerm value
  localContext (value' :) (evalTerm (tlet {tLetVals = rest}))
evalTerm tlet@(TLet [] body _) = evalTerm body
evalTerm bool@(TBool {}) = return bool
evalTerm (TIf cond cnsq alt dbg) = evalIf cond cnsq alt dbg
evalTerm int@(TInt _ _) = return int

evalTerms :: [Term] -> EvalType [Term]
evalTerms (term : rest) = do
  term' <- evalTerm term
  rest' <- evalTerms rest
  return $ term' : rest'
evalTerms [] = return []

evalIf :: Term -> Term -> Term -> Dbg -> EvalType Term
evalIf cond cnsq alt dbg = do
  result <- evalTerm cond
  case result of
    TBool True _ -> evalTerm cnsq
    TBool False _ -> evalTerm alt

-- apply
intBinaryOps :: Map.HashMap Text.Text (Integer -> Integer -> Dbg -> EvalType Term)
intBinaryOps =
  (Map.fromList . map (TupleOps.app1 Text.pack))
    [ ("+", \l r d -> return $ TInt (l + r) d),
      ("-", \l r d -> return $ TInt (l - r) d),
      ("*", \l r d -> return $ TInt (l * r) d),
      ("/", \l r d -> return $ TInt (quot l r) d),
      ("<", \l r d -> return $ TBool (l < r) d),
      ("<=", \l r d -> return $ TBool (l <= r) d),
      ("=", \l r d -> return $ TBool (l == r) d),
      (">", \l r d -> return $ TBool (l > r) d),
      (">=", \l r d -> return $ TBool (l >= r) d),
      ("/=", \l r d -> return $ TBool (l /= r) d)
    ]

apply :: Term -> [Term] -> Dbg -> EvalType Term
apply abs@(TAbs arity body env _ _) args dbg
  | length env + length args == arity = localContext (\s -> reverse args ++ env ++ s) (evalTerm body)
  | length env + length args < arity = return $ abs {tAbsEnv = reverse args ++ env}
  | otherwise = do EvalContext _ input <- ask; evalErrorExpected "function" expectedArgs actualArgs dbg
  where
    expectedArgs = show (arity - length env) ++ " arguments"
    actualArgs = show $ length env + length args
apply intBinOp@(TFVar opName _) [l, r] dbg | Map.member opName intBinaryOps = do
  EvalContext _ input <- ask
  l' <- evalTerm l
  r' <- evalTerm r
  case (l', r') of
    (TInt lVal (Dbg start _), TInt rVal (Dbg _ end)) ->
      (intBinaryOps Map.! opName) lVal rVal (Dbg start end)
    (TInt _ _, rTerm) ->
      let message = "Could not evaluate " ++ Text.unpack opName ++ ", expected right argument to be an integer but got " ++ show rTerm
       in throwError $ makeErrorString input dbg message
    (lTerm, TInt _ _) ->
      let message = "Could not evaluate " ++ Text.unpack opName ++ ", expected left argument to be an integer but got " ++ show lTerm
       in throwError $ makeErrorString input dbg message
apply term _ dbg = do
  EvalContext _ input <- ask
  let message = "Could not evaluate function - expected a lambda or primitive function but got " ++ show term
  throwError $ makeErrorString input dbg message

eval :: Term -> Text.Text -> Either String Term
eval term input =
  runReader (runEitherT (evalTerm term)) (EvalContext [] input)

-- evalString
evalString :: String -> Either String Term
evalString input = do
  term <- parse (Text.pack input)
  eval term (Text.pack input)