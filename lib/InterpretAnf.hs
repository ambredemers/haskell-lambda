{-# LANGUAGE Strict #-}

module InterpretAnf where

import Anf
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
evalAnfExp (AnfValue val) = evalAnfVal val
evalAnfExp (AnfLet value body _ _) = do
  value' <- evalAnfVal value
  localContext (value' :) (evalAnfExp body)
evalAnfExp (AnfLetApp fn args body _ dbg) = do
  fn' <- evalAnfVal fn
  args' <- evalAnfVals args
  result <- applyAnf fn' args' dbg
  localContext (result :) (evalAnfExp body)
evalAnfExp (AnfIf cond cnsq alt dbg) = do
  cond' <- evalAnfVal cond
  case cond' of
    AnfBool True _ -> evalAnfExp cnsq
    AnfBool False _ -> evalAnfExp alt
    val -> evalErrorExpected "if expression" "condition to be a boolean" (show val) dbg

evalAnfVal :: AnfVal -> AnfEvalType AnfVal
evalAnfVal (AnfBvar index _ dbg) = do
  AnfEvalContext stack _ <- ask
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
    [ ("+", \l r d -> return $ AnfInt (l + r) d),
      ("-", \l r d -> return $ AnfInt (l - r) d),
      ("*", \l r d -> return $ AnfInt (l * r) d),
      ("/", \l r d -> return $ AnfInt (quot l r) d),
      ("<", \l r d -> return $ AnfBool (l < r) d),
      ("<=", \l r d -> return $ AnfBool (l <= r) d),
      ("=", \l r d -> return $ AnfBool (l == r) d),
      (">", \l r d -> return $ AnfBool (l > r) d),
      (">=", \l r d -> return $ AnfBool (l >= r) d),
      ("/=", \l r d -> return $ AnfBool (l /= r) d)
    ]

applyAnf :: AnfVal -> [AnfVal] -> Dbg -> AnfEvalType AnfVal
applyAnf tAbs@(AnfAbs body env vars _) args dbg
  | length env + length args == length vars = localContext (\s -> reverse args ++ env ++ s) (evalAnfExp body)
  | length env + length args < length vars = return $ tAbs {aAbsEnv = reverse args ++ env}
  | otherwise = evalErrorExpected "function" expectedArgs actualArgs dbg
  where
    expectedArgs = show (length vars - length env) ++ " arguments"
    actualArgs = show $ length env + length args
applyAnf (AnfFvar opName _) [l, r] dbg | Map.member opName intBinaryOps = do
  l' <- evalAnfVal l
  r' <- evalAnfVal r
  let opNameText = "'" ++ Text.unpack opName ++ "'"
  case (l', r') of
    (AnfInt lVal (Dbg start _), AnfInt rVal (Dbg _ end)) -> (intBinaryOps Map.! opName) lVal rVal (Dbg start end)
    (AnfInt _ aiDbg, rTerm) -> evalErrorExpected opNameText "right argument to be an integer" (show rTerm) aiDbg
    (lTerm, AnfInt _ _) -> evalErrorExpected opNameText "left argument to be an integer" (show lTerm) dbg
    (lTerm, rTerm) -> evalErrorExpected opNameText "arguments to be integers" (show lTerm ++ " and " ++ show rTerm) dbg
applyAnf term _ dbg = do
  AnfEvalContext _ input <- ask
  let message = "Could not evaluate function - expected a lambda or primitive function but got " ++ show term
  throwError $ makeErrorString input dbg message

evalAnf :: AnfExp -> Text.Text -> Either String AnfVal
evalAnf aExp input =
  runReader (runEitherT (evalAnfExp aExp)) (AnfEvalContext [] input)

-- evalString
evalAnfString :: String -> Either String AnfVal
evalAnfString input = do
  aExp <- parseToAnf (Text.pack input)
  evalAnf aExp (Text.pack input)