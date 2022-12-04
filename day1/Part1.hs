module Part1 ( maxParagraphs ) where

-- Returns list of paragraphs, each a list of lines
paragraphLines :: [String] -> [[String]]
paragraphLines [] = []
paragraphLines lines = let (paragraph, rest) = break null lines
                        in paragraph : case rest of
                                         []     -> []
                                         (_:rt) -> paragraphLines rt

maxParagraphs :: String -> [Int]
maxParagraphs = map (sum . map read) . paragraphLines . lines

maxCalories :: String -> Int
maxCalories = maximum . maxParagraphs

main = do
  calories <- readFile "input.txt"
  print (maxCalories calories)
