module Part1 ( ordDiff ) where

import Data.Char ( ord )

ordDiff :: Char -> String -> Int
ordDiff c s = ord (head s) - ord c

-- (our - their + 1) `mod` 3 gives 0 for loss, 1 for tie, 2 for win
roundScore :: Int -> Int -> Int
roundScore their our = 1 + our + (our - their + 1) `mod` 3 * 3

plannedScore :: [String] -> Int
plannedScore (their:our:rest) = roundScore (ordDiff 'A' their) (ordDiff 'X' our) + plannedScore rest
plannedScore _ = 0

main = do
  guide <- readFile "input.txt"
  print (plannedScore (words guide))
