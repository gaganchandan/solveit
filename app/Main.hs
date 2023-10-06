module Main (main) where

import Backtrack
import Proposition

main :: IO ()
main = do
  solve (And (Or (Var 'A') (Or ((Not (Var 'B'))) (Var 'C'))) (Or (Not (Var 'D')) (Or (Var 'E') (Var 'F'))))
