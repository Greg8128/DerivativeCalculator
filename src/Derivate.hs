module Derivate
  ( derivate
  ) where
import Expr ( Func(..), Fexp(Fexp), Expr(..), fsum, fprod)
import Control.Monad.Writer
import Control.Monad.RWS
import Data.DList
import Simplify

type DerivateM = RWS String (DList String) ()

derivate sym exp = do
  e' <- simplify exp
  e'' <- writer $ evalRWS (derivate' e') sym ()
  simplify e''

derivate' :: Expr -> DerivateM Expr
derivate' exp = do
  sym <- ask
  case exp of
    Num _ -> pure $ Num 0
    v@(Var a) | a == sym -> pure $ Num 1
              | otherwise -> pure $ Num 0
    Fex f -> df f


df :: Fexp -> DerivateM Expr
df f@(Fexp (Func {fn=fn, gradient=gradient}) args) = do
  sym <- ask
  let gr = gradient args
  tell . pure $ "Gradient of " ++ show f ++ " is " ++ show gr
  dargs <- traverse derivate' args
  let difs = zipWith (\a b -> Fex (Fexp fprod [a, b])) gr dargs
  pure $ Fex (Fexp fsum difs)
