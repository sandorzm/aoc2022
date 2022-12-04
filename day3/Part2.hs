module Part2 where

import qualified Data.Set as Set

import Part1 ( priority, firstMember )

firstInAll :: String -> String -> String -> Char
firstInAll a b = firstMember (Set.fromList a `Set.intersection` Set.fromList b)

badgePriorities :: [String] -> Int
badgePriorities (a:b:c:t) = priority (firstInAll a b c) + badgePriorities t
badgePriorities _ = 0

main = do
  sacks <- readFile "input.txt"
  print (badgePriorities (lines sacks))
