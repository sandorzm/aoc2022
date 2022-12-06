module Part2 where

import Part1 ( topCrates )

main = readFile "input.txt" >>= putStrLn . topCrates id
