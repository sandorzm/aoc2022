module Part1 where

import Data.Char

ordDiff :: Char -> String -> Int
ordDiff c s = ord (head s) - ord c

roundScore :: Int -> Int -> Int
roundScore their our = 1 + our + (our - their + 1) `mod` 3 * 3

plannedScore :: [String] -> Int
plannedScore (their:our:rest) = roundScore (ordDiff 'A' their) (ordDiff 'X' our) + plannedScore rest
plannedScore _ = 0

main = do
  guide <- readFile "input.txt"
  print (plannedScore (words guide))
