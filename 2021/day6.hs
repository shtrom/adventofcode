import System.IO
import AoCUtils

data LanternFish = LanternFish Int deriving Show

main = do
        line <- getContents
        print $ day6 80 $ map toInt $ splitStringAt (==',') line
        print $ day6 256 $ map toInt $ splitStringAt (==',') line

-- | Day 6 test
-- >>> day6 80 [3,4,3,1,2]
-- 5934
day6 :: Int -> [Int] -> Int
day6 i xs = length $ simulateLanternFish i $ map (LanternFish) xs

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
