{-# LANGUAGE Strict #-}

module InterpretTerm where

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
data EvalContext = EvalContext {ecStack :: [Term], ecSource :: Text.Text}

type EvalType = EitherT String (Reader EvalContext)

localContext :: ([Term] -> [Term]) -> EvalType a -> EvalType a
localContext f e = do EvalContext stack _ <- ask; local (\s -> s {ecStack = f stack}) e

evalError :: String -> String -> Dbg -> EvalType a
evalError e message dbg = do
  EvalContext _ input <- ask
  let message' = "Could not evaluate " ++ e ++ " - " ++ message
  throwError (makeErrorString input dbg message')

evalErrorExpected :: String -> String -> String -> Dbg -> EvalType a
evalErrorExpected location expected actual = evalError location message
  where
    message = "expected " ++ expected ++ ", but got " ++ actual

evalTerm :: Term -> EvalType Term
evalTerm fvar@(TermFvar _ _) = return fvar
evalTerm (TermBvar index _ dbg) = do
  EvalContext stack input <- ask
  if index < length stack
    then return $ stack !! index
    else evalError "bound variable" ("invalid index " ++ show index) dbg
evalTerm abs@(TermAbs {}) = return abs
evalTerm (TermApp fn args dbg) = do
  EvalContext _ input <- ask
  fn' <- evalTerm fn
  args' <- evalTerms args
  apply fn' args' dbg
evalTerm tlet@(TermLet (LetBinding value _ _ : rest) body _) = do
  value' <- evalTerm value
  localContext (value' :) (evalTerm (tlet {tLetBindings = rest}))
evalTerm tlet@(TermLet [] body _) = evalTerm body
evalTerm bool@(TermBool {}) = return bool
evalTerm (TermIf cond cnsq alt dbg) = evalIf cond cnsq alt dbg
evalTerm int@(TermInt _ _) = return int

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
apply abs@(TermAbs body env vars _) args dbg
  | length env + length args == length vars = localContext (\s -> reverse args ++ env ++ s) (evalTerm body)
  | length env + length args < length vars = return $ abs {tAbsEnv = reverse args ++ env}
  | otherwise = do EvalContext _ input <- ask; evalErrorExpected "function" expectedArgs actualArgs dbg
  where
    expectedArgs = show (length vars - length env) ++ " arguments"
    actualArgs = show $ length env + length args
apply intBinOp@(TermFvar opName _) [l, r] dbg | Map.member opName intBinaryOps = do
  EvalContext _ input <- ask
  l' <- evalTerm l
  r' <- evalTerm r
  case (l', r') of
    (TermInt lVal (Dbg start _), TermInt rVal (Dbg _ end)) ->
      (intBinaryOps Map.! opName) lVal rVal (Dbg start end)
    (TermInt _ _, rTerm) ->
      let message = "Could not evaluate " ++ Text.unpack opName ++ ", expected right argument to be an integer but got " ++ show rTerm
       in throwError $ makeErrorString input dbg message
    (lTerm, TermInt _ _) ->
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