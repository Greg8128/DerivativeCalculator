{-# LANGUAGE LambdaCase#-}

module Listvars where
import Expr
import Data.List (foldl')

import Data.Set ( Set, singleton, union, empty,  toList )
import Data.Functor

listVars :: Expr -> [String]
listVars = toList . listVars'

listVars' :: Expr -> Set String
listVars' = \case
  Num n -> empty
  Var v -> singleton v
  FnExpr _ ls -> foldl' union empty . fmap listVars' $ ls
