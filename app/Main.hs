module Main (main) where

import Proposition (Proposition (..))
import Solve (satisfiable)

main :: IO ()
main = do
  let prop = And (Or (Var "A") (Or (Not (Var "B")) (Var "C"))) (Or (Not (Var "D")) (Or (Var "E") (Var "F")))
  print $ satisfiable prop
