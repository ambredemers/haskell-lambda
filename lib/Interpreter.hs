{-# LANGUAGE Strict #-}

module Interpreter where

import Control.Monad.Except
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict
import Data.Either
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Tuple.Ops as TupleOps
import Parser
import Term

-- eval
eval :: Text.Text -> Term -> [Term] -> Either String Term
eval _ fvar@(TFVar _ _) _ = Right fvar
eval input (TBVar index _ dbg) stack =
  if index < length stack
    then Right (stack !! index)
    else Left (makeErrorString input dbg message)
  where
    message = "Could not evaluate bound variable - invalid index" ++ show index
eval _ abs@(TAbs {}) _ = Right abs
eval input (TApp fn args dbg) stack =
  case (eval input fn stack, partitionEithers (map (\a -> eval input a stack) args)) of
    (Right fn', ([], args')) -> apply input fn' args' dbg stack
    (Right _, (failedArgs, _)) -> Left (head failedArgs)
    (Left failedFn, _) -> Left failedFn
eval input (TLet value body _ dbg) stack =
  case eval input value stack of
    Right value2 -> eval input body (value2 : stack)
    Left error -> Left error
eval _ bool@(TBool {}) _ = Right bool
eval input (TIf cond cnsq alt dbg) stack = evalIf input cond cnsq alt dbg stack
eval _ int@(TInt _ _) _ = Right int

evalIf :: Text.Text -> Term -> Term -> Term -> Dbg -> [Term] -> Either String Term
evalIf input cond cnsq alt dbg stack =
  case eval input cond stack of
    Right (TBool True _) -> eval input cnsq stack
    Right (TBool False _) -> eval input alt stack
    Left evalError -> Left evalError
    _ -> Left (makeErrorString input dbg message)
      where
        message = "Could not evaluate if expression - expected a boolean as the condition but got " ++ show cond

-- apply
intBinaryOps :: Map.HashMap Text.Text (Integer -> Integer -> Dbg -> Term)
intBinaryOps =
  (Map.fromList . map (TupleOps.app1 Text.pack))
    [ ("+", \l r d -> TInt (l + r) d),
      ("-", \l r d -> TInt (l - r) d),
      ("*", \l r d -> TInt (l * r) d),
      ("/", \l r d -> TInt (quot l r) d),
      ("<", \l r d -> TBool (l < r) d),
      ("<=", \l r d -> TBool (l <= r) d),
      ("=", \l r d -> TBool (l == r) d),
      (">", \l r d -> TBool (l > r) d),
      (">=", \l r d -> TBool (l >= r) d),
      ("/=", \l r d -> TBool (l /= r) d)
    ]

apply :: Text.Text -> Term -> [Term] -> Dbg -> [Term] -> Either String Term
apply input abs@(TAbs arity body env _ _) args dbg stack
  | length env + length args == arity = eval input body (reverse args ++ env ++ stack)
  | length env + length args < arity = Right (abs {tAbsEnv = reverse args ++ env})
  | otherwise = throwError $ makeErrorString input dbg message
  where
    expectedArgs = show $ arity - length env
    actualArgs = show $ length env + length args
    message = "Could not evaluate function - expected " ++ expectedArgs ++ " arguments, but got " ++ actualArgs
apply input intBinOp@(TFVar opName _) [l, r] dbg stack | Map.member opName intBinaryOps =
  case (eval input l stack, eval input r stack) of
    (Right (TInt lVal (Dbg start _)), Right (TInt rVal (Dbg _ end))) ->
      Right $ (intBinaryOps Map.! opName) lVal rVal (Dbg start end)
    (Right (TInt _ _), Right rTerm) ->
      let message = "Could not evaluate " ++ Text.unpack opName ++ ", expected right argument to be an integer but got " ++ show rTerm
       in Left (makeErrorString input dbg message)
    (Right lTerm, Right (TInt _ _)) ->
      let message = "Could not evaluate " ++ Text.unpack opName ++ ", expected left argument to be an integer but got " ++ show lTerm
       in Left (makeErrorString input dbg message)
    (Left error, _) -> Left error
    (_, Left error) -> Left error
apply input term _ dbg _ =
  let message = "Could not evaluate function - expected a lambda or primitive function but got " ++ show term
   in Left (makeErrorString input dbg message)

-- evalString
evalString :: String -> Either String Term
evalString input =
  case parse (Text.pack input) of
    Right term -> eval (Text.pack input) term []
    Left error -> Left error