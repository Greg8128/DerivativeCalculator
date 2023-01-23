module Main where

import Bind
import Control.Monad ( join, (<=<), forM_ )
import Control.Monad.Writer (runWriter)
import Data.DList (toList)
import Data.Functor
import Derivate
import Expr
import Listvars
import Parser
import Simplify
import Text.Parsec

main :: IO ()
main = putStrLn "Hello, Haskell!"

expr :: [Char] -> Expr
expr = (\(Right v) -> v) . parseExpr

testF :: (Expr -> (a, b)) -> [Char] -> a
testF f str = do
  let Right exp = parseExpr str
      (res, log) = f exp
  res
test :: [Char] -> Expr
test = testF $ fmap toList . runWriter . derivate "x"

testS :: [Char] -> Expr
testS = testF $ fmap toList . runWriter . simplify

for :: [a] -> (a -> b) -> [b]
for = flip fmap

fn' :: Either ParseError Expr
fn' = parseExpr "(y-x^2)*(y-2*x^2) + z"

fn_ :: Expr
fn_ = (\(Right v) -> v) fn'

resx :: [[(String, String, Expr)]]
resx =
  let vars = listVars fn_
      mtr =
        for vars $ \i ->
          for vars $ \j ->
            (i, j, fst . runWriter . (derivate j <=< derivate i) $ fn_)
   in mtr

resy :: [(String, String, Expr)]
resy = map (\(a, b, c) -> (a, b, c)) (join resx)

presy :: IO ()
presy = forM_ resy print

-- >>> test "x^ 2"
-- *(2.0, x)

-- >>> test "3 * x * y + x ^ 4"
-- +(*(3.0, y), *(4.0, ^(x, 3.0)))
