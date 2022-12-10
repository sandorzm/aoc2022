module Main where

import Data.List ( foldl' )
import Data.HashMap.Strict ( HashMap, (!) )
import qualified Data.HashMap.Strict as M
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

main = do
  session <- readFile "input.txt"
  let trueSizes = case lines session of
                    "$ cd /":t -> findTrueSize [] (dirSizes t)
                    _          -> error "First command not '$ cd /'"
  putStr "Small dir total: "
  print (smallDirTotal (10^5) trueSizes)
  putStr "Minimum required directory deletion: "
  print (minDeleteDir (70 * 10^6) (30 * 10^6) trueSizes)

type Path = [String]
type SizeMap = HashMap Path Int
type SubdirMap = HashMap Path (HashSet String)
type PathMapPair = (SizeMap, SubdirMap)

smallDirTotal :: Int -> SizeMap -> Int
smallDirTotal maxSize = M.foldl (+) 0 . M.filter (<= maxSize)

minDeleteDir :: Int -> Int -> SizeMap -> Int
minDeleteDir totalSpace neededSpace sizes
  | deleteNeeded <= 0 = 0
  | otherwise         = M.foldl (\a b -> if b >= deleteNeeded then min a b else a) totalSpace sizes
  where deleteNeeded = sizes![] - (totalSpace - neededSpace)

dirSizes :: [String] -> PathMapPair
dirSizes = snd . foldl' interpretLine ([], (M.empty, M.singleton [] S.empty)) . map words

interpretLine :: (Path, PathMapPair) -> [String] -> (Path, PathMapPair)
interpretLine (_, maps) ["$", "cd", "/"] = ([], maps)
interpretLine (_:parent, maps) ["$", "cd", ".."] = (parent, maps)
-- When cd-ing to dir, ensure it exists in SubdirMap so current dir is always there
interpretLine (path, (sizes, subs)) ["$", "cd", dir] =
  (dir:path, (sizes, M.insert (dir:path) S.empty $ M.adjust (S.insert dir) path subs))
interpretLine state ["$", "ls"] = state
interpretLine state ["dir", _] = state -- No need to save listed dir until it is cd-ed to
interpretLine (path, (sizes, subs)) [size, _] = (path, (M.insertWith (+) path (read size) sizes, subs))
interpretLine _ term = error $ "Invalid terminal line: '" ++ unwords term ++ "'"

-- This part is a bit awkward and reveals that I should be using a custom data
-- structure like
-- newtype DirTree = Dir (Int, (HashMap String DirTree))
--
-- But maybe that's not practical in a functional language because I would want
-- to maintain a tree as well as a pointer to the current dir (to avoid lots of
-- HashMap lookups while allowing fast 'cd /'), but then updating the subtree
-- wouldn't update the full tree because they're pure and immutable! You would
-- have to maintain your own list of parents of the current dir so you can
-- incrementally build the new tree on update, but would those be guaranteed to
-- point to the same structure or could they be wasting a lot of memory? Hmm.
findTrueSize :: Path -> PathMapPair -> SizeMap
findTrueSize path maps@(sizes, subs) =
  let (subSizes, subSizeMaps) =
        unzip $ map (\dir -> let subSizes = findTrueSize (dir:path) maps
                              in (subSizes!(dir:path), subSizes)) (S.toList (subs!path))
   in M.insertWith (+) path (sum (M.findWithDefault 0 path sizes : subSizes))
                                  (M.unions subSizeMaps)

sample = unlines [ "$ cd /",
                   "$ ls",
                   "dir a",
                   "14848514 b.txt",
                   "8504156 c.dat",
                   "dir d",
                   "$ cd a",
                   "$ ls",
                   "dir e",
                   "29116 f",
                   "2557 g",
                   "62596 h.lst",
                   "$ cd e",
                   "$ ls",
                   "584 i",
                   "$ cd ..",
                   "$ cd ..",
                   "$ cd d",
                   "$ ls",
                   "4060174 j",
                   "8033020 d.log",
                   "5626152 d.ext",
                   "7214296 k"
                 ]
