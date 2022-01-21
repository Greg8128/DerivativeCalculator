module Parser where

import Control.Applicative (liftA3)
import Control.Monad
import Data.Bool
import Data.Functor
import Data.List
import Expr
import Text.Parsec.Number
import Text.ParserCombinators.Parsec

parseExpr = parse sumP "Expression"

numP, varP, anyFnP :: Parser Expr

exprP = spaces >> (try numP <|> try varP <|> try parenP <|> anyFnP) <* spaces

numP = Num <$> (sign <*> floating3 False)

varP = Var . pure <$> oneOf "abcdertuvwxyz"

groupOpP op lowerP opstr invopstr invop =
  liftA3 bool (fnexpr op) head (null . tail) <$> go
  where
    go = (pure <$> lowerP) `chainr1` term
    term =
      try (string opstr $> (++))
        <|> ( string invopstr $> \a (b : bs) ->
                a ++ [fnexpr invop [b, Num (-1)]] ++ bs
            )

sumP = groupOpP fsum prodP "+" "-" fprod

prodP = groupOpP fprod powP "*" "/" fpow

powP = exprP `chainr1` (string "^" $> \a b -> fnexpr fpow [a, b])

parenP = string "(" >> sumP <* string ")"

anyFnP =
  foldl1 (<|>) $
    map
      (try . fnP)
      [ ("+", fsum),
        ("*", fprod),
        ("^", fpow),
        ("ln", fln),
        ("sin", fsin),
        ("cos", fcos)
      ]

fnP (name, fn) = do
  string name
  char '('
  ls <- sumP `sepBy` char ','
  char ')'
  pure $ Fex (Fexp fn ls)
