{-# LANGUAGE OverloadedStrings #-}
import Data.List (sort, elemIndex)
import qualified Data.Text as T
import Data.Sequence (mapWithIndex, fromList)
import Data.List.Split
import Data.Maybe
import Utils

data NestedList a = Elem a | List [NestedList a] deriving (Read)

instance (Show a) => Show (NestedList a) where
  show (Elem a) = show a
  show (List a) = show a

instance (Eq a) => Eq (NestedList a) where
  Elem a == Elem b = a == b
  List a == Elem b = List a == List [Elem b]
  Elem a == List b = List [Elem a] == List b
  List a == List b = a == b

instance (Ord a) => Ord (NestedList a) where
  Elem a <= Elem b = a <= b
  Elem a <= List b = List [Elem a] <= List b
  List a <= Elem b = List a <= List [Elem b]
  List [] <= List _ = True
  List _ <= List [] = False
  List (x:xs) <= List (y:ys) = if x == y then List xs <= List ys else x <= y

splitAtFirst :: Char -> String -> (String, String)
splitAtFirst char = fmap (drop 1) . break (==char)

getMatchingBracket' :: String -> Int -> Int -> Int
getMatchingBracket' str index count =
  let val = str !! index
      newCount = if val == '[' then count + 1 else if val == ']' then count - 1 else count
   in if newCount == 0 then index else getMatchingBracket' str (index+1) newCount

getMatchingBracket :: String -> Int
getMatchingBracket str = getMatchingBracket' str 0 0

parseString :: String -> [NestedList Int]
parseString "" = []
parseString (',':xs) = parseString xs
parseString (']':xs) = parseString xs
parseString str@('[':xs) = let matchingBracket = getMatchingBracket str
                               (nest, rest) = splitAt matchingBracket str
                            in List (parseString (drop 1 nest)) : parseString rest
parseString str
  | ',' `notElem` str = [Elem (read str)]
  | otherwise         = let (num, rest) = splitAtFirst ',' str
                         in Elem (read num :: Int) : parseString rest

parseNestedList :: String -> NestedList Int
parseNestedList = head . parseString

-- auto parsing
--doParse :: String -> NestedList Int
--doParse = read . T.unpack . T.replace "[Elem ]" "[]" . T.replace "[" "List [" . T.replace "Elem [" "[" . T.replace "[" "[Elem " . T.replace "," ",Elem " . T.pack

parseInput :: String -> [(NestedList Int, NestedList Int)]
parseInput = map (toPair . map parseNestedList) . splitWhen (== "") . lines

index :: Int -> Bool -> Int
index _ False = 0
index a True = a+1

part1 :: String -> Int
part1 = sum . mapWithIndex index . fromList . map (uncurry (<=)) . parseInput

part2 :: String -> Int
part2 x =
  let dividers = ["[[2]]", "[[6]]"]
      sorted = sort . map parseNestedList . (++ dividers) . filter (/="") . lines $ x
   in product . map ((+1) . fromJust . (`elemIndex` sorted) . parseNestedList) $ dividers


main :: IO ()
main = solve "13" part1 part2
