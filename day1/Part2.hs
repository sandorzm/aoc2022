module Part2 where

import Data.List

import Part1 (paragraphLines, maxParagraphs)

maxCaloriesTop3 :: String -> Int
maxCaloriesTop3 c = sum (take 3 (sortBy (flip compare) (maxParagraphs c)))

main = do
  calories <- readFile "input.txt"
  print (maxCaloriesTop3 calories)
