module Main where
import Derivate
import Expr
import Parser
import Bind
import Text.Parsec
import Simplify
import Data.Functor
import Control.Monad.Writer (runWriter)
import Data.DList (toList)
import Control.Monad
import Listvars
main :: IO ()
main = putStrLn "Hello, Haskell!"

expr = (\(Right v) -> v) . parseExpr

testF f = do
  str <- getLine
  let Right exp = parseExpr str
      (res, log) = f exp
  mapM_ putStrLn log
  putStrLn ""
  putStrLn $ "result is: " ++ show res

test = testF $ fmap toList . runWriter . derivate "x"

testS = testF $ fmap toList . runWriter . simplify

for = flip fmap

fn' = parseExpr "(y-x^2)*(y-2*x^2) + z"

fn_ = (\(Right v) -> v) fn'

resx =
  let vars = listVars fn_
      mtr = for vars $ \i ->
                for vars $ \j ->
                    (i,j, fst . runWriter . (derivate j <=< derivate i) $ fn_)
   in mtr

resy = map (\(a,b,c) -> (a,b, c)) (join resx)
presy = forM_ resy print
