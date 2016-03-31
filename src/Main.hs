module Main where

import Hylogen
import Data.Function
import Data.Monoid





testExpr :: Vec1
testExpr = V2 0.1 (sin_ 0.1) & X

testExpr2 :: Vec1
testExpr2 = sin_ 0.1


main :: IO ()
main = do
  run $ V4 0.8 1 0.2 1
