module Part1 ( readTwoRanges ) where

-- Split at every single occurence of chars in delims (not grouping consecutive
-- delimiters together) and convert tokens to Ints
splitSingleInt :: String -> String -> [Int]
splitSingleInt _ "" = [] -- See day 1 Part1.pargaraphLines
splitSingleInt delims s = case break (`elem` delims) s of
                            (token, [])  -> [read token]
                            (token, _:t) -> read token : splitSingleInt delims t

-- [a, b, c, d] pattern asserts exactly 4 integers in input
readTwoRanges :: String -> ((Int, Int), (Int, Int))
readTwoRanges s = ((a, b), (c, d)) where [a, b, c, d] = splitSingleInt "-," s

eitherSubset :: ((Int, Int), (Int, Int)) -> Bool
eitherSubset ((a, b), (c, d)) = a == c || b == d || (a > c) == (b < d)

main = do
  elfRanges <- readFile "input.txt"
  print (length (filter (eitherSubset . readTwoRanges) (lines elfRanges)))
