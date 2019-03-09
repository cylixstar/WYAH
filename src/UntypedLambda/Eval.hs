module UntypedLambda.Eval (
  runEval
) where

import UntypedLambda.Syntax
import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.Writer

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr UntypedLambda.Eval.Scope

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure{} = "<<closure>>"

newtype EvalState = EvalState{depth :: Int} deriving (Show)

type Step = (Int, Expr)
type Eval a = WriterT [Step] (State EvalState) a

type Scope = Map.Map String Value

red :: Expr -> Eval ()
red expr = do
  d <- gets depth
  tell [(d, expr)]
  return ()

incDepth :: Eval a -> Eval a
incDepth m = do
  modify $ \s -> s{depth=depth s + 1}
  out <- m
  modify $ \s -> s{depth=depth s - 1}
  return out

eval :: UntypedLambda.Eval.Scope -> Expr -> Eval Value
eval env expr = case expr of
  Lit (LInt x) -> return $ VInt (fromIntegral x)
  Lit (LBool x) -> return $ VBool x
  Var x -> do
    red expr
    return $ env Map.! x
  Lam x body -> return (VClosure x body env)
  App a b -> incDepth $ do
    f <- eval env a
    red a
    x <- eval env b
    red b
    apply f x

apply :: Value -> Value -> Eval Value
apply (VClosure v body env) x = eval (Map.insert v x env) body
apply _ _ = error "Attempted to apply non-closure."

emptyScope :: Map.Map String Value
emptyScope = Map.empty

runEval :: Expr -> (Value, [Step])
runEval e = evalState (runWriterT (eval emptyScope e)) (EvalState 0)
