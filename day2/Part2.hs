module Part2 where

import Part1 ( ordDiff )

roundScore :: Int -> Int -> Int
roundScore their outcome = 1 + (their + outcome - 1) `mod` 3 + outcome * 3

plannedScore :: [String] -> Int
plannedScore (their:outcome:rest) = roundScore (ordDiff 'A' their) (ordDiff 'X' outcome)
                                    + plannedScore rest
plannedScore _ = 0

main = do
  guide <- readFile "input.txt"
  print (plannedScore (words guide))
