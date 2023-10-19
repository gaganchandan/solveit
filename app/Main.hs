module Main (main) where

import LiteralElim
import Proposition
import Solve

main :: IO ()
main = do
  solve $ literalElim (And (Or (Var "A") (Or (Not (Var "B")) (Var "C"))) (Or (Not (Var "D")) (Or (Var "E") (Var "F"))))
