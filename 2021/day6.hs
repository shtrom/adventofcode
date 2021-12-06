import System.IO

data LanternFish = LanternFish Int deriving Show

main = do
        line <- getContents
        print $ day6 80 $ map toInt $ splitStringAt (==',') line

-- | Day 6 test
-- >>> day6 80 [3,4,3,1,2]
-- 5934
day6 :: Int -> [Int] -> Int
day6 _ xs = length $ map (LanternFish) xs

--d | Split String at arbitrary character matching predicate
-- >>> splitStringAt (==',') "1,2,3"
-- ["1","2","3"]
splitStringAt :: (Char -> Bool) -> String -> [String]
splitStringAt _ "" = [""]
splitStringAt p s = let (xs, ys) = break p s
          in case ys of
             [] -> [xs]
             [y] -> [xs, [y]]
             otherwise -> (xs
                          : splitStringAt p (tail ys)
                          )

-- | Convert string to an Int
-- >>> toInt "2"
-- 2
toInt :: String -> Int
toInt s = read s ::Int

-- | Iterate a LanternFish
-- >>> iterateLanternFish (LanternFish 0)
-- [LanternFish 6,LanternFish 8]
-- >>> iterateLanternFish (LanternFish 4)
-- [LanternFish 3]
iterateLanternFish :: LanternFish -> [LanternFish]
iterateLanternFish (LanternFish 0) = [LanternFish 6, LanternFish 8]
iterateLanternFish (LanternFish x) = [LanternFish (x-1)]
