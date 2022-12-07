module Part2 where

import Part1 ( findSignal )

-- Not efficient, but still completes almost instantly
main = readFile "input.txt" >>= print . findSignal 14
