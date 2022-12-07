module Part1 ( findSignal ) where

-- For greater signal lengths, a multiset (or mapping to number of occurrences)
-- of working set chars along with a ring buffer for lastN would be more
-- efficient (not sure how efficient given everything is immutable). We get away
-- with this algorithm for now.
findSignal' :: Int -> Int -> Int -> String -> String -> Int
findSignal' n pos workingSetSize _ _ | workingSetSize == n = pos
findSignal' _ _ _ _ [] = -1
findSignal' n pos 0 _ (h:t) = findSignal' n (pos + 1) 1 [h] t
findSignal' n pos workingSetSize lastN (h:t) | length lastN == n =
  let keep  = init lastN
      exit  = if last lastN `elem` keep then 0 else -1
      entry = if h          `elem` keep then 0 else 1
   in findSignal' n (pos + 1) (workingSetSize + exit + entry) (h:keep) t
findSignal' n pos workingSetSize lastN (h:t) =
  let entry = if h `elem` lastN then 0 else 1
   in findSignal' n (pos + 1) (workingSetSize + entry) (h:lastN) t

findSignal :: Int -> String -> Int
findSignal signalLength = findSignal' signalLength 0 0 ""

main = readFile "input.txt" >>= print . findSignal 4
