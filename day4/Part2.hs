module Part2 where

import Part1 ( readTwoRanges )

rangesOverlap :: ((Int, Int), (Int, Int)) -> Bool
rangesOverlap ((a, b), (c, d)) = b >= c && a <= d

main = do
  elfRanges <- readFile "input.txt"
  print (length (filter (rangesOverlap . readTwoRanges) (lines elfRanges)))
