module Part1 ( topCrates ) where

import Data.List ( foldl' )
import Data.Vector ( Vector, (!), (//) ) -- Or Data.Array
import qualified Data.Vector as Vector

-- Exploit that input lines have trailing spaces to the full width of the stacks
readCrateRow :: String -> String
readCrateRow [] = []
readCrateRow (_:crateChar:_:t) = crateChar : case t of []   -> []
                                                       _:t' -> readCrateRow t'
readCrateRow _ = error "Invalid crate row"

stackCrateRow :: String -> [String] -> [String]
stackCrateRow = zipWith (\crateChar -> if crateChar == ' ' then id else (crateChar:))

-- Infinite list of [] makes this quite beautiful
stackCrates :: [String] -> [String]
stackCrates = foldr (stackCrateRow . readCrateRow) (repeat [])

-- Also convert to zero-based indices
readCraneMove :: String -> (Int, Int, Int)
readCraneMove line = let [_, n, _, from, _, to] = words line in (read n, read from - 1, read to - 1)

-- Transform Vector of stacks based on crane move (zero-based indices)
doCraneMove :: (String -> String) -> Vector String -> (Int, Int, Int) -> Vector String
doCraneMove craneOrder stacks (n, from, to) = stacks // [(from, bot), (to, craneOrder top ++ toStack)]
  where (top, bot) = splitAt n (stacks!from)
        toStack    = stacks!to

splitCrateLines :: [String] -> ([String], [String])
splitCrateLines lines = let (a, b) = break null lines in (init a, tail b)

-- Convert to and from Vector for faster random access than linked list. Not
-- sure how efficient the immutable Vector is, but it's enough.
topCrates :: (String -> String) -> String -> String
topCrates craneOrder input = Vector.toList (Vector.map head (foldl' (doCraneMove craneOrder) initStacks
                                                             (map readCraneMove craneMoves)))
  where (crateRows, craneMoves) = splitCrateLines (lines input)
        initStacks              = Vector.fromList (stackCrates crateRows)

-- Cute equivalent of 'do', shorter in this case
main = readFile "input.txt" >>= putStrLn . topCrates reverse
