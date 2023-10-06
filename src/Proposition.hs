module Proposition where

data Proposition
  = Var Char
  | And Proposition Proposition
  | Or Proposition Proposition
  | Not Proposition
  | Constant Bool
  deriving (Show, Eq)

-- toCNF :: Proposition -> Proposition
-- toCNF (Var v) = Var v
-- toCNF (Constant b) = Constant b
-- toCNF (And p q) = And (toCNF p) (toCNF q)
