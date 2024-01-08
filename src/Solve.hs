module Solve (satisfiable) where

import Control.Applicative (Alternative ((<|>)))
import Data.Maybe (mapMaybe)
import LiteralElim (literalElim)
import Proposition (Proposition (..), toCNF)

findFree :: Proposition -> Maybe String
findFree (Var v) = Just v
findFree (And f1 f2) = findFree f1 <|> findFree f2
findFree (Or f1 f2) = findFree f1 <|> findFree f2
findFree (Not f) = findFree f
findFree (Constant _) = Nothing

guessVariable :: String -> Bool -> Proposition -> Proposition
guessVariable v b (Var v') = if v == v' then Constant b else Var v'
guessVariable v b (And f1 f2) = And (guessVariable v b f1) (guessVariable v b f2)
guessVariable v b (Or f1 f2) = Or (guessVariable v b f1) (guessVariable v b f2)
guessVariable v b (Not f) = Not (guessVariable v b f)
guessVariable _ _ f = f

simplify :: Proposition -> Proposition
simplify (Constant b) = Constant b
simplify (Var v) = Var v
simplify (Not f) =
  case simplify f of
    Constant b -> Constant (not b)
    f' -> Not f'
simplify (And f1 f2) =
  let fs = filter (/= Constant True) [simplify f1, simplify f2]
   in if Constant False `elem` fs
        then Constant False
        else case fs of
          [] -> Constant True
          [f] -> f
          [f1', f2'] -> And f1' f2'
          _ -> error "Precondition violated"
simplify (Or f1 f2) =
  let fs = filter (/= Constant False) [simplify f1, simplify f2]
   in if Constant True `elem` fs
        then Constant True
        else case fs of
          [] -> Constant False
          [f] -> f
          [f1', f2'] -> Or f1' f2'
          _ -> error "Precondition violated"

unConstant :: Proposition -> Bool
unConstant (Constant b) = b
unConstant _ = error "Not a Constant "

unitClause :: Proposition -> Maybe (String, Bool)
unitClause (Var v) = Just (v, True)
unitClause (Not (Var v)) = Just (v, False)
unitClause _ = Nothing

getUnitClauses' :: Proposition -> [Proposition]
getUnitClauses' (And p q) = getUnitClauses' p ++ getUnitClauses' q
getUnitClauses' p = [p]

getUnitClauses :: Proposition -> [(String, Bool)]
getUnitClauses = mapMaybe unitClause . getUnitClauses'

unitProp :: Proposition -> Proposition
unitProp p = replaceAll p
  where
    assigments = getUnitClauses p
    replaceAll = foldl (.) id (map (uncurry guessVariable) assigments)

satisfiable :: Proposition -> Bool
satisfiable p =
  case findFree p' of
    Nothing -> unConstant $ simplify p'
    Just v -> do
      satisfiable (simplify $ guessVariable v True p) || satisfiable (simplify $ guessVariable v False p)
  where
    p' = literalElim $ toCNF $ unitProp p
