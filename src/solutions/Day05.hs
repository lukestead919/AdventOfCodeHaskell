import Data.List (transpose, find)
import Data.List.Split
import Utils
import Data.Maybe

type Crates = [String]

type Instruction = (Int, Int, Int)

parseInstruction :: String -> Instruction
parseInstruction = (\xs -> (read (xs !! 1), read (xs !! 3), read (xs !! 5))) . words

parseCrates :: [String] -> Crates
parseCrates = let sanitizeCrate = reverse . trim . init
                  findCrate num = fmap sanitizeCrate . find (\crate -> last crate == num)
                  findCrates x = map (fromMaybe "" . (`findCrate` x)) ['0'..'9'] -- add in a blank index 0 to avoid having to alter the instruction indices
              in findCrates . transpose

parseInput :: String -> (Crates, [Instruction])
parseInput =
  let parsed x = (parseCrates $ head x, map parseInstruction $ last x)
   in parsed . splitWhen (== "") . lines

tops :: Crates -> String
tops = map last . drop 1

applyInstruction :: Instruction -> Crates -> Crates
applyInstruction (a, b, c) crates =
  let (x, y) = splitAt (length (crates !! b) - a) (crates !! b)
   in replace b x . replace c ((crates !! c) ++ reverse y) $ crates

part1 :: String -> String
part1 =
  let apply (crates, instructions) = foldl (flip applyInstruction) crates instructions
   in tops . apply . parseInput

applyInstruction' :: Instruction -> Crates -> Crates
applyInstruction' (a, b, c) crates =
  let (x, y) = splitAt (length (crates !! b) - a) (crates !! b)
   in replace b x . replace c ((crates !! c) ++ y) $ crates

part2 :: String -> String
part2 =
  let apply (crates, instructions) = foldl (flip applyInstruction') crates instructions
   in tops . apply . parseInput

main :: IO ()
main = solve "05" part1 part2
