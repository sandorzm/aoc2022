module Part1 ( priority, firstMember ) where

import Data.Char ( ord )
import Data.Set ( Set )
import qualified Data.Set as Set

priority :: Char -> Int
priority c | c >= 'a'  = 1 + ord c - ord 'a'
           | otherwise = 27 + ord c - ord 'A'

halve :: String -> (String, String)
halve s = splitAt (length s `div` 2) s

firstMember :: Set Char -> String -> Char
firstMember s = head . filter (`Set.member` s)

firstInBoth :: String -> String -> Char
firstInBoth = firstMember . Set.fromList

sackPriority :: String -> Int
sackPriority s = priority (firstInBoth s1 s2) where (s1, s2) = halve s

main = do
  sacks <- readFile "input.txt"
  print (sum (map sackPriority (lines sacks)))
