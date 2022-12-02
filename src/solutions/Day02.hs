import Utils

parseInput :: String -> [String]
parseInput = lines

toNumericValue :: Char -> Int
toNumericValue x
  | x `elem` ['A', 'X'] = 1
  | x `elem` ['B', 'Y'] = 2
  | x `elem` ['C', 'Z'] = 3
  | otherwise = error ("missing numeric value score " ++ [x])

outcomeScore :: String -> Int
outcomeScore x = case t of
  0 -> 3
  1 -> 0
  2 -> 6
  _ -> error ("missing outcome score " ++ show t)
  where
    t = (toNumericValue (head x) - toNumericValue (last x)) `mod` 3

part1 :: String -> Int
part1 =
  let score y = outcomeScore y + toNumericValue (last y)
   in sum . map score . parseInput

calcShapeScore :: String -> Int
calcShapeScore x =
  let outcomeModifier z = case z of
        'X' -> -1
        'Y' -> 0
        'Z' -> 1
        _ -> error ("missing shape score " ++ [z])
   in (toNumericValue (head x) + outcomeModifier (last x) - 1) `mod` 3 + 1

outcomeScore' :: Char -> Int
outcomeScore' x = case x of
  'X' -> 0
  'Y' -> 3
  'Z' -> 6
  _ -> error ("missing outcome score " ++ [x])

part2 :: String -> Int
part2 =
  let score y = calcShapeScore y + outcomeScore' (last y)
   in sum . map score . parseInput

main :: IO ()
main = solve "02" part1 part2
