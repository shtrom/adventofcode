import System.IO
import AoCUtils

main = do line <-getContents
          print $ lines line

-- | Compute risk
-- >>> day91 ["2199943210","3987894921","9856789892","8767896789","9899965678"]
--15
-- day91 :: [String] -> Int
-- day91 l = sum $ map (1+) $ map $ findLowPoints l
day91 l = findLowPoints l

-- | Find low points in a 2D matrix
-- >>> findLowPoints ["219", "398","985"]
-- [1,5]
findLowPoints :: Ord a => [[a]] -> [a]
findLowPoints s = findLowPoints' [] [] s


-- | Find low points given a north and west row, and the rest of a matrix
-- >>> findLowPoints' [] []  ["219", "398","985"]
-- [1,5]
-- >>> findLowPoints' "19" "39" ["98", "85"]
-- [5]
findLowPoints' :: Ord a => [a] -> [a] -> [[a]] -> [a]
findLowPoints' ns ws l@(x:xs) = let (n, e, s, w, p) = getPoints ns ws l
                                 in case isLowPoint n e s w p of
                                    True -> [p]
                                    False -> []

-- | Extract the points, wich sensible defaults
-- >>> getPoints [1,2] [3,4] [[5,6],[7,8]]
-- (1,6,7,3,5)
getPoints :: [a] -> [a] -> [[a]] -> (a, a, a, a, a)
getPoints _ _ [] = error "nothing left to extract"
getPoints ns ws (x:xs) = let p = head x
                             n = head ns
                             e = head (drop 1 x)
                             s = head (head xs)
                             w = head ws
                           in (n, e, s, w, p)

-- | Determine if a point is the lowest of its vicinity
-- >>> isLowPoint 9 9 9 9 1
-- True
-- >>> isLowPoint 1 9 9 9 5
-- False
-- >>> isLowPoint 9 1 9 9 5
-- False
-- >>> isLowPoint 9 9 1 9 5
-- False
-- >>> isLowPoint 9 9 9 1 5
-- False
isLowPoint :: Ord a => a -> a -> a -> a -> a -> Bool
isLowPoint n e s w p = all (>p) [n,e,s,w]
