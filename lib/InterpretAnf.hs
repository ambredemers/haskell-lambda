{-# LANGUAGE Strict #-}

module InterpretAnf where

import ANF
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Either
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Tuple.Ops as TupleOps
import Term

data AnfEvalContext = AnfEvalContext {aecStack :: [AnfVal], aecSource :: Text.Text}

type AnfEvalType = EitherT String (Reader AnfEvalContext)

localContext :: ([AnfVal] -> [AnfVal]) -> AnfEvalType a -> AnfEvalType a
localContext f e = do AnfEvalContext stack _ <- ask; local (\s -> s {aecStack = f stack}) e

-- eval
evalError :: String -> String -> Dbg -> AnfEvalType a
evalError e message dbg = do
  AnfEvalContext _ input <- ask
  let message' = "Could not evaluate " ++ e ++ " - " ++ message
  throwError (makeErrorString input dbg message')

evalErrorExpected :: String -> String -> String -> Dbg -> AnfEvalType a
evalErrorExpected location expected actual = evalError location message
  where
    message = "expected " ++ expected ++ ", but got " ++ actual

evalAnfExp :: AnfExp -> AnfEvalType AnfVal
evalAnfExp (AVal val) = evalAnfVal val
evalAnfExp alet@(ALet value body _ _) = do
  value' <- evalAnfVal value
  localContext (value' :) (evalAnfExp body)
evalAnfExp (ALetApp fn args body name dbg) = do
  AnfEvalContext _ input <- ask
  fn' <- evalAnfVal fn
  args' <- evalAnfVals args
  result <- applyAnf fn' args' dbg
  localContext (result :) (evalAnfExp body)
evalAnfExp (ALetIf cond cnsq alt body _ dbg) = do
  cond' <- evalAnfVal cond
  result <- case cond' of
    ABool True _ -> evalAnfExp cnsq
    ABool False _ -> evalAnfExp alt
    val -> evalErrorExpected "if expression" "condition to be a boolean" (show val) dbg
  localContext (result :) (evalAnfExp body)

evalAnfVal :: AnfVal -> AnfEvalType AnfVal
evalAnfVal (ABVar index _ dbg) = do
  AnfEvalContext stack input <- ask
  if index < length stack
    then return $ stack !! index
    else evalError "bound variable" ("invalid index " ++ show index) dbg
evalAnfVal val = return val

evalAnfVals :: [AnfVal] -> AnfEvalType [AnfVal]
evalAnfVals (term : rest) = do
  term' <- evalAnfVal term
  rest' <- evalAnfVals rest
  return $ term' : rest'
evalAnfVals [] = return []

-- apply
intBinaryOps :: Map.HashMap Text.Text (Integer -> Integer -> Dbg -> AnfEvalType AnfVal)
intBinaryOps =
  (Map.fromList . map (TupleOps.app1 Text.pack))
    [ ("+", \l r d -> return $ AInt (l + r) d),
      ("-", \l r d -> return $ AInt (l - r) d),
      ("*", \l r d -> return $ AInt (l * r) d),
      ("/", \l r d -> return $ AInt (quot l r) d),
      ("<", \l r d -> return $ ABool (l < r) d),
      ("<=", \l r d -> return $ ABool (l <= r) d),
      ("=", \l r d -> return $ ABool (l == r) d),
      (">", \l r d -> return $ ABool (l > r) d),
      (">=", \l r d -> return $ ABool (l >= r) d),
      ("/=", \l r d -> return $ ABool (l /= r) d)
    ]

applyAnf :: AnfVal -> [AnfVal] -> Dbg -> AnfEvalType AnfVal
applyAnf abs@(AAbs body env vars _) args dbg
  | length env + length args == length vars = localContext (\s -> reverse args ++ env ++ s) (evalAnfExp body)
  | length env + length args < length vars = return $ abs {aabsEnv = reverse args ++ env}
  | otherwise = do AnfEvalContext _ input <- ask; evalErrorExpected "function" expectedArgs actualArgs dbg
  where
    expectedArgs = show (length vars - length env) ++ " arguments"
    actualArgs = show $ length env + length args
applyAnf intBinOp@(AFVar opName _) [l, r] dbg | Map.member opName intBinaryOps = do
  AnfEvalContext _ input <- ask
  l' <- evalAnfVal l
  r' <- evalAnfVal r
  case (l', r') of
    (AInt lVal (Dbg start _), AInt rVal (Dbg _ end)) ->
      (intBinaryOps Map.! opName) lVal rVal (Dbg start end)
    (AInt _ _, rTerm) ->
      let message = "Could not evaluate " ++ Text.unpack opName ++ ", expected right argument to be an integer but got " ++ show rTerm
       in throwError $ makeErrorString input dbg message
    (lTerm, AInt _ _) ->
      let message = "Could not evaluate " ++ Text.unpack opName ++ ", expected left argument to be an integer but got " ++ show lTerm
       in throwError $ makeErrorString input dbg message
applyAnf term _ dbg = do
  AnfEvalContext _ input <- ask
  let message = "Could not evaluate function - expected a lambda or primitive function but got " ++ show term
  throwError $ makeErrorString input dbg message

evalAnf :: AnfExp -> Text.Text -> Either String AnfVal
evalAnf exp input =
  runReader (runEitherT (evalAnfExp exp)) (AnfEvalContext [] input)

-- evalString
evalAnfString :: String -> Either String AnfVal
evalAnfString input = do
  exp <- parseToAnf (Text.pack input)
  evalAnf exp (Text.pack input)