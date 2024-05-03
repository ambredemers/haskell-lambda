module Interpreter where

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
eval input (TBVar index _ dbg) stack
  | index < length stack = Right (stack !! index)
  | otherwise =
      let message = "Could not evaluate bound variable - invalid index" ++ show index
       in Left (makeErrorString input dbg message)
eval _ abs@(TAbs {}) _ = Right abs
eval input (TApp fn args dbg) stack
  | Right fn' <- eval input fn stack,
    ([], args') <- partitionEithers (map (\a -> eval input a stack) args) =
      apply input fn' args' dbg stack
  | Right _ <- eval input fn stack,
    (failedArgs, _) <- partitionEithers (map (\a -> eval input a stack) args) =
      Left (head failedArgs)
  | Left failedFn <- eval input fn stack =
      Left failedFn
eval input (TLet value body _ dbg) stack
  | Right value2 <- eval input value stack = eval input body (value2 : stack)
  | Left error <- eval input value stack = Left error
eval _ bool@(TBool {}) _ = Right bool
eval input (TIf cond cnsq alt dbg) stack = evalIf input cond cnsq alt dbg stack
eval _ int@(TInt _ _) _ = Right int

evalIf :: Text.Text -> Term -> Term -> Term -> Dbg -> [Term] -> Either String Term
evalIf input cond cnsq alt dbg stack
  | Right (TBool True _) <- eval input cond stack = eval input cnsq stack
  | Right (TBool False _) <- eval input cond stack = eval input alt stack
  | Left evalError <- eval input cond stack = Left evalError
  | otherwise =
      let message = "Could not evaluate if expression - expected a boolean as the condition but got " ++ show cond
       in Left (makeErrorString input dbg message)

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
  | otherwise =
      let expectedArgs = arity - length env
       in let actualArgs = length env + length args
           in let message =
                    "Could not evaluate function - expected "
                      ++ show expectedArgs
                      ++ " arguments, "
                      ++ "but got "
                      ++ show actualArgs
               in Left (makeErrorString input dbg message)
apply input intBinOp@(TFVar opName _) [l, r] dbg stack
  | Map.member opName intBinaryOps,
    Right (TInt lVal (Dbg start _)) <- eval input l stack,
    Right (TInt rVal (Dbg _ end)) <- eval input r stack =
      Right ((intBinaryOps Map.! opName) lVal rVal (Dbg start end))
  | Map.member opName intBinaryOps,
    Right (TInt _ _) <- eval input l stack,
    Right rTerm <- eval input r stack =
      let message =
            "Could not evaluate "
              ++ Text.unpack opName
              ++ ", expected right argument to be an integer but got "
              ++ show rTerm
       in Left (makeErrorString input dbg message)
  | Map.member opName intBinaryOps,
    Right lTerm <- eval input l stack =
      let message =
            "Could not evaluate "
              ++ Text.unpack opName
              ++ ", expected left argument to be an integer but got "
              ++ show lTerm
       in Left (makeErrorString input dbg message)
  | Map.member opName intBinaryOps, Left error <- eval input l stack = Left error
  | Map.member opName intBinaryOps, Left error <- eval input r stack = Left error
apply input term _ dbg _ =
  let message = "Could not evaluate function - expected a lambda or primitive function but got " ++ show term
   in Left (makeErrorString input dbg message)

-- evalString
evalString :: String -> Either String Term
evalString input =
  case parse (Text.pack input) of
    Right term -> eval (Text.pack input) term []
    Left error -> Left error