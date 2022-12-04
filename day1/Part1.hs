module Part1 ( maxParagraphs ) where

-- Returns list of paragraphs, each a list of lines
paragraphLines :: [String] -> [[String]]
paragraphLines lines = case break null lines of
                         ([], _)    -> []
                         (par, [])  -> [par]
                         (par, _:t) -> par : paragraphLines t

maxParagraphs :: String -> [Int]
maxParagraphs = map (sum . map read) . paragraphLines . lines

maxCalories :: String -> Int
maxCalories = maximum . maxParagraphs

main = do
  calories <- readFile "input.txt"
  print (maxCalories calories)
