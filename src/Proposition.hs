module Proposition where

data Proposition
  = Var String
  | And Proposition Proposition
  | Or Proposition Proposition
  | Not Proposition
  | Constant Bool
  deriving (Show, Eq)

convertNegation :: Proposition -> Proposition
convertNegation (Not (Not p)) = convertNegation p
convertNegation (Not (And p q)) = Or (convertNegation $ Not p) (convertNegation $ Not q)
convertNegation (Not (Or p q)) = And (convertNegation $ Not p) (convertNegation $ Not q)
convertNegation (Not (Constant b)) = Constant (not b)
convertNegation (Not p) = Not (convertNegation p)
convertNegation (And p q) = And (convertNegation p) (convertNegation q)
convertNegation (Or p q) = Or (convertNegation p) (convertNegation q)
convertNegation p = p

distribute :: Proposition -> Proposition
distribute (Or p (And q r)) =
  And
    (Or (distribute p) (distribute q))
    (Or (distribute p) (distribute r))
distribute (Or (And q r) p) =
  And
    (Or (distribute p) (distribute q))
    (Or (distribute p) (distribute r))
distribute (Or p q) = Or (distribute p) (distribute q)
distribute (And p q) = And (distribute p) (distribute q)
distribute (Not p) = Not (distribute p)
distribute p = p

toCNF :: Proposition -> Proposition
toCNF expr = if updated == expr then expr else toCNF updated
  where
    updated = distribute (convertNegation expr)
