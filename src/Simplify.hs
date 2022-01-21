{-# LANGUAGE LambdaCase, BlockArguments#-}

module Simplify where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Map (toList, fromListWith)
import Expr
import Data.DList (DList, fromList)
import qualified Data.DList
import Control.Monad.RWS (RWS, tell, evalRWS)
import Control.Monad.Writer (Writer, tell, runWriter)
type Log = DList String
type SimplifyM = Writer (DList String)

simplify' x = fst $ runWriter (simplify x)

simplify :: Expr -> SimplifyM Expr
simplify x = do
  y <- simplifyOnce x
  if x == y
    then do
    -- tell (pure (show x ++ " could not be simplified further."))
    pure x
    else do
      -- tell (pure ("Simplified " ++ show x ++ " to " ++ show y))
      simplify y

logSimplificationM sh f x = do
  y <- f x
  unless (y == x) $ tell $ pure $ sh x y
  pure y

logSimplification sh f = logSimplificationM sh (pure . f)

simplifyOnce :: Expr -> SimplifyM Expr
simplifyOnce = simplifyChildren >=> \case
  n@(Num a) -> pure n
  v@(Var a) -> pure v
  FnExpr Sum [] -> pure $ Num 0
  FnExpr Product [] -> pure $ Num 1
  FnExpr Power [a,Num 1] -> pure a
  FnExpr Power [a, Num 0] -> pure $ Num 1
  FnExpr Sum [a] -> pure a
  FnExpr Ln [Var "e"] -> pure $ Num 1 --Shitty hack lol. Implement actual constants
  FnExpr Product [a] -> pure a
  Fex (Fexp (Func {fn=fn}) ls) | Just ns <- toNums ls -> pure . Num $ fn ns
  e@(FnExpr Sum ls) -> fmap (fnexpr fsum) . mergeConstsWith fsum (+)
                       <=< clear fsum (Num 0) <=< combineSumTerms . flattenSum $ ls
  e@(FnExpr Product ls) | Num 0 `elem` ls -> pure $ Num 0
                        | otherwise -> fmap (fnexpr fprod ) . mergeConstsWith fprod (*)
                                       <=< clear fprod (Num 1) <=< combineProductTerms . flattenProduct $ ls
  other -> pure other

clear :: Func -> Expr -> [Expr] -> SimplifyM [Expr]
clear d n = logSimplification
          (\x y -> "Removed occurrences of " ++ show n ++ " from "
            ++ show (fnexpr d x) ++ ", leaving " ++ show (fnexpr d y))
          (filter (/= n))

simplifyChildren :: Expr -> SimplifyM Expr
simplifyChildren = \case
  n@(Num a) -> pure n
  v@(Var a) -> pure v
  Fex (Fexp fn ls) -> fnexpr fn <$> traverse simplify ls

toNum :: Expr -> Maybe Double
toNum (Num a) = Just a
toNum x = Nothing

toNums :: [Expr] -> Maybe [Double]
toNums = traverse toNum

mergeConstsWith :: Func -> (Double -> Double -> Double) -> [Expr] -> SimplifyM [Expr]
mergeConstsWith d f = logSimplification
  (\x y ->"Combined constant terms in" ++ show (fnexpr d x) ++ ", leaving " ++ show y) (go f) where
  go f ls =
    let (c, nc) = partition (\x -> [0 | Num a <- [x]] == [0]) ls
    in if null c
        then nc

        else foldl1' (\(Num a) (Num b) -> Num (f a b)) c : nc

combineTermsWith toTuple combine fromTuple = logSimplification
  (\x y -> "Sorted and combined similar terms of " ++ show x ++ ", resulting in " ++ show y)
  (combineTermsWith' toTuple combine fromTuple)

combineTermsWith' toTuple combine fromTuple ls =
  let normal = map toTuple ls
      dedup = toList . fromListWith combine $ normal
  in map (simplify' . fromTuple) dedup

combineSumTerms = combineTermsWith
                  (\case FnExpr Product (a@(Num n): xs) -> (fnexpr fprod xs, a)
                         other -> (other, Num 1))
                  simplifiedSum
                  (\(e, n) -> fnexpr fprod [n, e])

combineProductTerms = combineTermsWith
                      (\case FnExpr Power [a, n] -> (a, n)
                             other -> (other, Num 1))
                      simplifiedSum
                      (\(e, n) -> fnexpr fpow [e,n])

simplifiedSum a b = simplify' (fnexpr fsum [a,b] )

flattenWith f ls = ls >>= f

flattenSum :: [Expr] -> [Expr]
flattenSum = flattenWith (\case
  (FnExpr Sum ls) -> ls
  other -> [other])

flattenProduct :: [Expr] -> [Expr]
flattenProduct = flattenWith (\case
  (FnExpr Product ls) -> ls
  other -> [other])
