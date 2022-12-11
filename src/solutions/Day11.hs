import Data.List (partition, sortOn)
import Data.List.Split
import Utils

data Var = Old deriving (Show, Read)

data Value = Val Int | Var Var deriving (Show, Read)

data Op = Plus | Multiply | Div deriving (Show, Read)

data EquationToken = ValueToken Value | OpToken Op deriving (Show, Read)

type Equation = [EquationToken]

type Items = [Int]

data Monkey = Monkey {items :: Items, equation :: Equation, testDivisor :: Int, ifTrue :: Int, ifFalse :: Int, throws :: Int} deriving (Show, Read)

parseEquation :: String -> Equation
parseEquation eqn =
  let readValue x = ValueToken (if x == "old" then Var Old else Val (read x :: Int))
      readOp x = OpToken (if x == "+" then Plus else Multiply)
      (a : b : c : _) = words eqn
   in [readValue a, readOp b, readValue c]

substitute :: Value -> Int -> Int
substitute (Val x) _ = x
substitute (Var Old) old = old

operator :: Op -> (Int -> Int -> Int)
operator op = case op of
  Plus -> (+)
  Multiply -> (*)
  Div -> div

solveEquation :: Equation -> Int -> Int
solveEquation [ValueToken a] x = substitute a x
solveEquation (ValueToken a : OpToken op : ValueToken b : eqn) x =
  let one = substitute a x
      two = substitute b x
   in solveEquation (ValueToken (Val (operator op one two)) : eqn) x
solveEquation _ _ = error "Invalid equation"

parseMonkey :: [String] -> Monkey
parseMonkey stuff =
  let (_ : items : operation : divisor : ifTrue : ifFalse : _) = stuff
      parseItems = map (read . filter (/= ',')) . tail . tail . words
      parseOperation = parseEquation . unwords . tail . tail . tail . words
      parseDivisor = read . last . words
      parseTrue = read . last . words
      parseFalse = read . last . words
   in Monkey
        { items = parseItems items,
          equation = parseOperation operation,
          testDivisor = parseDivisor divisor,
          ifTrue = parseTrue ifTrue,
          ifFalse = parseFalse ifFalse,
          throws = 0
        }

parseInput :: String -> [Monkey]
parseInput = map parseMonkey . splitWhen (== "") . lines

addItemsToMonkey :: Int -> Items -> [Monkey] -> [Monkey]
addItemsToMonkey num extraItems monkeys =
  let monkey = monkeys !! num
      lcm' = foldl1 lcm . map testDivisor $ monkeys
      worryModifiedItems = map (`mod` lcm') extraItems
      newMonkey = monkey {items = items monkey ++ worryModifiedItems}
   in replace num newMonkey monkeys

throwAtIndex :: [Monkey] -> Int -> [Monkey]
throwAtIndex monkeys num =
  let monkey = monkeys !! num
      itemsToThrow = items monkey
      score = solveEquation (equation monkey)
      newItems = map score itemsToThrow
      (throwToTrue, throwToFalse) = partition (\i -> i `mod` testDivisor monkey == 0) newItems
      newMonkey = monkey {items = [], throws = throws monkey + length itemsToThrow}
      trueMonkeyNum = ifTrue monkey
      falseMonkeyNum = ifFalse monkey
   in replace num newMonkey . addItemsToMonkey trueMonkeyNum throwToTrue . addItemsToMonkey falseMonkeyNum throwToFalse $ monkeys

generation :: [Monkey] -> [Monkey]
generation monkeys = foldl throwAtIndex monkeys [0 .. length monkeys - 1]

generations :: Int -> [Monkey] -> [Monkey]
generations gens = (!! gens) . iterate generation

solveForGenerations :: Int -> [Monkey] -> Int
solveForGenerations gens input =
  let monkeys = reverse . sortOn throws . generations gens $ input
      mostThrows = slice 0 2 monkeys
   in product . map throws $ mostThrows

addToEquation :: Equation -> [Monkey] -> [Monkey]
addToEquation eqn = map (\monkey -> monkey {equation = equation monkey ++ eqn})

part1 :: String -> Int
part1 = solveForGenerations 20 . addToEquation [OpToken Div, ValueToken . Val $ 3] . parseInput

part2 :: String -> Int
part2 = solveForGenerations 10000 . parseInput

main :: IO ()
main = solve "11" part1 part2
