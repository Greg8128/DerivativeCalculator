{-# LANGUAGE LambdaCase #-}

module Bind where

import Expr
import Simplify
import Data.Function

class Bind v where
  bind :: String -> v -> Expr -> Expr

instance Bind Double where bind = bindNum

instance Bind Expr where bind = bindExpr

bindNum sym n = bind sym (Num n)

bindExpr :: String -> Expr -> Expr -> Expr
bindExpr sym exp = simplify' . bindExpr' sym exp

bindExpr' :: String -> Expr -> Expr -> Expr
bindExpr' sym exp = fix $ \f -> \case
      Var v | v == sym -> exp
      Fex (Fexp fn ls) -> fnexpr fn (map f ls)
      other -> other
