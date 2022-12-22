import Data.List (delete, find, findIndex, foldl', sort)
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Debugging
import Utils

type Monkey = String

data Operation = Plus | Minus | Multiply | Divide | Equals deriving (Eq)

data Expression = Num Int | Equation Monkey Operation Monkey

type Expressions = M.Map Monkey Expression

parseOperation :: Char -> Operation
parseOperation x = case x of
  '+' -> Plus
  '-' -> Minus
  '*' -> Multiply
  '/' -> Divide

opToFn :: Operation -> (Int -> Int -> Int)
opToFn x = case x of
  Plus -> (+)
  Minus -> (-)
  Multiply -> (*)
  Divide -> div
  Equals -> (\a b -> if a == b then 1 else 0)

inverse :: Operation -> Operation
inverse x = case x of
  Plus -> Minus
  Minus -> Plus
  Multiply -> Divide
  Divide -> Multiply
  Equals -> Equals

parseInput :: String -> Expressions
parseInput = M.fromList . map readLine . lines
  where
    readLine = readWordsInLine . words
    readWordsInLine (x : xs) = (init x, expression xs)
    expression [a] = Num (read a)
    expression [a, op, b] = Equation a (parseOperation $ head op) b

solveExpression :: Expressions -> Monkey -> Int
solveExpression exprs monkey = solveExpression' expr
  where
    expr = M.findWithDefault (Num 0) monkey exprs
    solveExpression' (Num a) = a
    solveExpression' (Equation a op b) = opToFn op (solveExpression exprs a) (solveExpression exprs b)

setExpressionToEquals :: Expression -> Expression
setExpressionToEquals (Equation a _ b) = Equation a Equals b
setExpressionToEquals a = a

findWithKey :: (k -> v -> Bool) -> M.Map k v -> Maybe (k, v)
findWithKey f = find (uncurry f) . M.toList

reverseSolve :: Expressions -> Monkey -> Int
reverseSolve exprs monkey = solved
  where
    findRelevantExpression _ (Equation b _ c) = b == monkey || c == monkey
    findRelevantExpression _ _ = False
    (a, Equation b op c) = fromJust . findWithKey findRelevantExpression $ exprs
    otherMonkey = if b == monkey then c else b
    aSolved = reverseSolve exprs a
    otherSolved = solveExpression exprs otherMonkey
    solved
      | op == Equals = otherSolved
      | b == monkey = opToFn (inverse op) aSolved otherSolved
      | otherwise =
        if op == Plus || op == Multiply
          then opToFn (inverse op) aSolved otherSolved
          else opToFn op otherSolved aSolved

part1 :: String -> Int
part1 input = answer
  where
    exprs = parseInput input
    answer = solveExpression exprs "root"

part2 :: String -> Int
part2 input = answer
  where
    exprs = parseInput input
    fixedRootExpr = M.update (Just . setExpressionToEquals) "root" exprs
    answer = reverseSolve fixedRootExpr "humn"

main :: IO ()
main = solve "21" part1 part2
