module Backtrack (satisfiable, solve) where

import Control.Applicative (Alternative ((<|>)))
import Proposition (Proposition (..))

findFree :: Proposition -> Maybe Char
findFree (Var v) = Just v
findFree (And f1 f2) = findFree f1 <|> findFree f2
findFree (Or f1 f2) = findFree f1 <|> findFree f2
findFree (Not f) = findFree f
findFree (Constant _) = Nothing

guessVariable :: Char -> Bool -> Proposition -> Proposition
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
unConstant _ = error "Not a Constant"

satisfiable :: Proposition -> Bool
satisfiable f =
  case findFree f of
    Nothing -> unConstant f
    Just v ->
      let trueGuess = simplify $ guessVariable v True f
          falseGuess = simplify $ guessVariable v False f
       in satisfiable trueGuess || satisfiable falseGuess

printSol' :: [(Char, Bool)] -> IO ()
printSol' [] = putStr ""
printSol' [(v, b)] = do
  putStr $ v : " = "
  putStr $ show b
printSol' ((v, b) : vs) = do
  putStr $ v : " = "
  putStr $ show b ++ ", "
  printSol' vs

printSol :: [(Char, Bool)] -> IO ()
printSol sol = do
  putStr "{"
  printSol' sol
  putStrLn "}"

solve' :: Proposition -> [(Char, Bool)] -> IO ()
solve' f vars =
  case findFree f of
    Nothing ->
      if unConstant f
        then printSol vars
        else return ()
    Just v -> do
      solve' (simplify $ guessVariable v True f) ((v, True) : vars)
      solve' (simplify $ guessVariable v False f) ((v, False) : vars)

solve :: Proposition -> IO ()
solve f = solve' f []
