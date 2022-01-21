{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Expr
  ( Expr (..),
    Fexp (..),
    Func (..),
    Label (..),
    pattern FnExpr,
    fnexpr,
    fsum,
    fprod,
    fpow,
    fln,
    fsin,
    fcos,
  )
where

import Data.Function
import Data.Functor
import Control.Monad
import Data.List
{-
An expression may be a number,
  a variable,
  or a function whose inputs are expressions.
-}

pattern FnExpr :: Label -> [Expr] -> Expr
pattern FnExpr label args = Fex (Fexp (Func {label = label}) args)

fnexpr :: Func -> [Expr] -> Expr
fnexpr a b = Fex (Fexp a b)

data Expr
  = Num Double
  | Var String
  | Fex Fexp
  deriving (Eq, Ord)

instance Show Expr where
  show (Num n) = show n
  show (Var v) = v
  show (Fex a) = show a

data Fexp = Fexp Func [Expr] deriving (Eq, Ord)

instance Show Fexp where
  show f@(Fexp Func{_show = show} ls) = show f

data Func = Func
  { fn :: [Double] -> Double,
    gradient :: [Expr] -> [Expr],
    _show :: Fexp -> String,
    label :: Label
  }


instance Eq Func where (==) = (==) `on` label

instance Ord Func where compare = compare `on` label

data Label
  = Sum
  | Product
  | Power
  | Ln
  | Sin
  | Cos
  deriving (Eq, Ord, Show)

showFn str (Fexp fn ls) = str ++ "(" ++ intercalate ", " (map show ls) ++ ")"


-- Sum one or more arguments
fsum :: Func
fsum =
  Func
    { fn = sum,
      gradient = ($> Num 1),
      _show = showFn "+",
      label = Sum
    }

-- Multiply one or more arguments
fprod :: Func
fprod =
  Func
    { fn = product,
      gradient =
        \ls ->
          map
            (Fex . Fexp fprod . remove ls)
            (zipWith const [0 ..] ls),
      _show = showFn "*",
      label = Product
    }

-- Arg1 ^ arg2
fpow :: Func
fpow =
  Func
    { fn = \[i, j] -> i ** j,
      gradient = \[i, j] ->
        [ fnexpr fprod [j, fnexpr fpow [i, fnexpr fsum [j, Num (-1)]]],
          fnexpr fprod [fnexpr fln [i], fnexpr fpow [i, j]]
        ],
      _show = showFn "^",
      label = Power
    }


-- Ln (arg1)

fln :: Func
fln =
  Func
    { fn = \[i] -> log i,
      gradient = \[i] -> [fnexpr fpow [i, Num (-1)]],
      _show = showFn "ln",
      label = Ln
    }
-- Sin (arg1)

fsin :: Func
fsin =
  Func
    { fn = \[i] -> sin i,
      gradient = \[i] -> [fnexpr fcos [i]],
      _show = showFn "sin",
      label = Sin
    }
-- Cos(arg1)
fcos :: Func
fcos =
  Func
    { fn = \[i] -> cos i,
      gradient = \[i] -> [fnexpr fprod [Num (-1), fnexpr fsin [i]]],
      _show = showFn "cos",
      label = Cos
    }


remove ls n = map snd . filter ((/= n) . fst) $ zip [0 ..] ls
