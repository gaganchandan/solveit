module LiteralElim where

import Data.Map (Map)
import qualified Data.Map as Map
-- import Data.Set (Set)
-- import qualified Data.Set as Set
import Proposition

-- getLiterals' :: Proposition -> Set String
-- getLiterals' (Var v) = Set.singleton v
-- getLiterals' (Not p) = getLiterals' p
-- getLiterals' (And p q) = Set.union (getLiterals' p) (getLiterals' q)
-- getLiterals' (Or p q) = Set.union (getLiterals' p) (getLiterals' q)
-- getLiterals' _ = Set.empty

-- getLiterals :: Proposition -> [String]
-- getLiterals = Set.toList . getLiterals'

data Polarity = Positive | Negative | Mixed deriving (Show, Eq)

updatePolarity :: Map String (Maybe Polarity) -> String -> Polarity -> Map String (Maybe Polarity)
updatePolarity polarityMap v polarity =
  let oldPolarity = Map.lookup v polarityMap
   in case oldPolarity of
        Nothing -> Map.insert v (Just polarity) polarityMap
        Just (Just Mixed) -> polarityMap
        Just oldPolarity' ->
          if oldPolarity' == Just polarity
            then polarityMap
            else Map.insert v (Just Mixed) polarityMap

getPolarities ::
  Proposition ->
  Map String (Maybe Polarity) ->
  Map String (Maybe Polarity)
getPolarities (Var v) polarityMap = updatePolarity polarityMap v Positive
getPolarities (Not (Var v)) polarityMap = updatePolarity polarityMap v Negative
getPolarities (Not p) polarityMap = getPolarities p polarityMap
getPolarities (And p q) polarityMap =
  let polarityMap' = getPolarities p polarityMap
   in getPolarities q polarityMap'
getPolarities (Or p q) polarityMap =
  let polarityMap' = getPolarities p polarityMap
   in getPolarities q polarityMap'
getPolarities _ polarityMap = polarityMap

literalElim' :: Proposition -> Map String (Maybe Polarity) -> Proposition
literalElim' p polarityMap =
  case p of
    Var v -> case Map.lookup v polarityMap of
      Just (Just Positive) -> Constant True
      Just (Just Negative) -> Constant False
      Just (Just Mixed) -> Var v
      _ -> Var v
    Not p' -> Not (literalElim' p' polarityMap)
    And p' q' -> And (literalElim' p' polarityMap) (literalElim' q' polarityMap)
    Or p' q' -> Or (literalElim' p' polarityMap) (literalElim' q' polarityMap)
    _ -> p

literalElim :: Proposition -> Proposition
literalElim p = literalElim' p (getPolarities p Map.empty)
