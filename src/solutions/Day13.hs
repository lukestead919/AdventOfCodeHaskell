{-# LANGUAGE OverloadedStrings #-}

import Data.List (elemIndex, sort)
import Data.List.Split
import Data.Maybe
import Data.Sequence (fromList, mapWithIndex)
import qualified Data.Text as T
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
  List (x : xs) <= List (y : ys) = if x == y then List xs <= List ys else x <= y

parseNestedList :: String -> NestedList Int
parseNestedList = read . T.unpack . T.replace "[Elem ]" "[]" . T.replace "[" "List [" . T.replace "Elem [" "[" . T.replace "[" "[Elem " . T.replace "," ",Elem " . T.pack

parseInput :: String -> [(NestedList Int, NestedList Int)]
parseInput = map (toPair . map parseNestedList) . splitWhen (== "") . lines

index :: Int -> Bool -> Int
index _ False = 0
index a True = a + 1

part1 :: String -> Int
part1 = sum . mapWithIndex index . fromList . map (uncurry (<=)) . parseInput

part2 :: String -> Int
part2 x =
  let dividers = ["[[2]]", "[[6]]"]
      sorted = sort . map parseNestedList . (++ dividers) . filter (/= "") . lines $ x
   in product . map ((+ 1) . fromJust . (`elemIndex` sorted) . parseNestedList) $ dividers

main :: IO ()
main = solve "13" part1 part2
