import System.IO
import AoCUtils

data LanternFish = LanternFish Int deriving Show

main = do
        line <- getContents
        print $ day6' 80 $ map toInt $ splitStringAt (==',') line
        print $ day6' 256 $ map toInt $ splitStringAt (==',') line

-- | Day 6 test
-- >>> day6 80 [3,4,3,1,2]
-- 5934
day6 :: Int -> [Int] -> Int
day6 i xs = length $ simulateLanternFish i $ map (LanternFish) xs

-- | Day 6 test, faster computation
-- >>> day6' 80 [3,4,3,1,2]
-- 5934
day6' :: Int -> [Int] -> Int
day6' i l = let a = (aggregate . sort) l
                in foldl (sumCounts i) 0 a

-- | Simulate a numuber of lantern fish at a given age and time point, and sum
-- to existing
-- >>> sumCounts 80 0 (2,3)
-- 2308
sumCounts :: Int -> Int -> (Int, Int) -> Int
sumCounts i s (c,n) = s + c * day6 i [n]

-- | Given a sorted list, count the re-occurence of each element
-- >>> aggregate [1,2,2,3,3,3]
-- [(1,1),(2,2),(3,3)]
aggregate :: Eq a => [a] -> [(Int, a)]
aggregate [] = []
aggregate l@(x:xs) = let (xs, ys) = break (/=x) l
                         c = length xs
                    in (
                       (c, x)
                       :aggregate ys
                    )
-- | Sort a list of comparables
-- >>> sort [1,9,2,8]
-- [1,2,8,9]
sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:xs) = (sort $ filter (<x) xs) ++ [x] ++ (sort $ filter (>=x) xs)

-- | Simulate lantern fish for the given number of generations
-- >>> simulateLanternFish 3 [LanternFish 1]
-- [LanternFish 5,LanternFish 7]
simulateLanternFish :: Int -> [LanternFish] -> [LanternFish]
simulateLanternFish 0 xs = xs
simulateLanternFish n xs = concat $ map iterateLanternFish $ simulateLanternFish (n-1) xs

-- | Iterate a LanternFish
-- >>> iterateLanternFish (LanternFish 0)
-- [LanternFish 6,LanternFish 8]
-- >>> iterateLanternFish (LanternFish 4)
-- [LanternFish 3]
iterateLanternFish :: LanternFish -> [LanternFish]
iterateLanternFish (LanternFish 0) = [LanternFish 6, LanternFish 8]
iterateLanternFish (LanternFish x) = [LanternFish (x-1)]
