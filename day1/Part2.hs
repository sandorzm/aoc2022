module Part2 where

import Data.List

import Part1 (maxParagraphs)

maxCaloriesTop3 :: String -> Int
maxCaloriesTop3 = sum . take 3 . sortBy (flip compare) . maxParagraphs

main = do
  calories <- readFile "input.txt"
  print (maxCaloriesTop3 calories)
