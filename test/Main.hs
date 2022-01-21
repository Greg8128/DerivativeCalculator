{-# LANGUAGE TemplateHaskell #-}
module Main where
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Distribution.Simple.Command (OptDescr(BoolOpt))
import Data.Functor
import Expr

tests :: IO Bool
tests = checkParallel $$(discover)

main = tests $> ()

prop_test :: Property
prop_test = property $ do
  "abcd" === "abcdef"


genName :: MonadGen m => m String
genName = Gen.choice [pure "x", pure "y", pure "z"]

genDouble :: MonadGen m => m Double
genDouble = Gen.realFloat $ Range.linearFrac 0 100
