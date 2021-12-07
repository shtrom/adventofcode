import System.IO
import AoCUtils

data LanternFish = LanternFish Int deriving (Show, Ord, Eq)

instance Num LanternFish where
        (+) (LanternFish a) (LanternFish b) = LanternFish (a+b)
        negate (LanternFish a) = LanternFish (-a)
        (*) (LanternFish a) (LanternFish b) = LanternFish (a*b)
        (abs) (LanternFish a) = LanternFish (abs a)
        (signum) (LanternFish a) = LanternFish (div a a)
        fromInteger a = LanternFish (fromInteger a)


data FishSchool = FishSchool Int LanternFish deriving Show
instance Eq FishSchool where
        (==) (FishSchool n1 a1) (FishSchool n2 a2) = a1 == a2
instance Ord FishSchool where
        (>) (FishSchool n1 a1) (FishSchool n2 a2) = a1 > a2
        (<=) (FishSchool n1 a1) (FishSchool n2 a2) = a1 <= a2
instance Num FishSchool where
        (+) (FishSchool 0 _) (FishSchool n2 a2) = (FishSchool n2 a2)
        (+) (FishSchool n1 a1) (FishSchool 0 _) = (FishSchool n1 a1)
        (+) (FishSchool n1 a1) (FishSchool n2 a2) = case a1 == a2 of
                                                      True -> (FishSchool (n1+n2) a1)
                                                      False -> error "FishSchool of different ages don't add"
        negate (FishSchool n a) = (FishSchool (-n) a)
        (*) (FishSchool _ _) = error "FishSchools don't multiply"
        (abs) (FishSchool n _) = error "FishSchools don't have absolute values"
        (signum) (FishSchool _ _) = error "FishSchools don't have absolute signums"
        (fromInteger) _ = error "FishSchools can't be made from Integers"

-- | Make a FishSchools from a number of LanternFish
-- >>> makeFishSchool (1,2)
-- FishSchool 1 (LanternFish 2)
makeFishSchool :: (Int, LanternFish) -> FishSchool
makeFishSchool (a, b) = FishSchool a b
-- | Count fish in a school
-- >>> countFish (FishSchool 12 13)
-- 12
countFish :: FishSchool -> Int
countFish (FishSchool n _) = n

main = do
        line <- getContents
        print $ day6' 80 $ map toInt $ splitStringAt (==',') line
        print $ day6' 256 $ map toInt $ splitStringAt (==',') line

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

-- | Day 6 test, faster computation
-- >>> day6' 0 [3,4,3,1,2]
-- 5
-- >>> day6' 1 [0,1,2]
-- 4
day6' :: Int -> [Int] -> Int
day6' i xs = day6'' (i) $ map makeFishSchool $ (aggregate . sort) $ map LanternFish xs

-- | Day 6 work on aggregated fish
-- >>> day6'' 0 [FishSchool 1 (LanternFish 3),FishSchool 1 (LanternFish 4),FishSchool 1 (LanternFish 3),FishSchool 1 (LanternFish 1),FishSchool 1 (LanternFish 2)]
-- 5
-- >>> day6'' 1 [FishSchool 1 (LanternFish 0),FishSchool 1 (LanternFish 1),FishSchool 1 (LanternFish 2)]
-- 4
day6'' :: Int -> [FishSchool] -> Int
day6'' 0 xs = sum $ map countFish xs
day6'' n xs = day6'' (n-1) $ simulateFishSchool xs

-- | Given a sorted list, count the re-occurence of each element
-- >>> aggregate [1,2,2,3,3,3]
-- [(1,1),(2,2),(3,3)]
-- >>> aggregate [LanternFish 1,LanternFish 2,LanternFish 2,LanternFish 3,LanternFish 3,LanternFish 3]
-- [(1,LanternFish 1),(2,LanternFish 2),(3,LanternFish 3)]
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
-- >>> sort [LanternFish 1,LanternFish 9,LanternFish 2,LanternFish 8]
-- [LanternFish 1,LanternFish 2,LanternFish 8,LanternFish 9]
sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:xs) = (sort $ filter (<x) xs) ++ [x] ++ (sort $ filter (>=x) xs)

-- | Simulate a whole school of fish
-- >>> simulateFishSchool [FishSchool 1 (LanternFish 1),FishSchool 2 (LanternFish 2),FishSchool 3 (LanternFish 3)]
-- [FishSchool 1 (LanternFish 0),FishSchool 2 (LanternFish 1),FishSchool 3 (LanternFish 2)]
-- >>> simulateFishSchool [FishSchool 1 (LanternFish 0),FishSchool 2 (LanternFish 2),FishSchool 3 (LanternFish 3)]
-- [FishSchool 2 (LanternFish 1),FishSchool 3 (LanternFish 2),FishSchool 1 (LanternFish 6),FishSchool 1 (LanternFish 8)]
simulateFishSchool :: [FishSchool] -> [FishSchool]
simulateFishSchool xs = mergeFishSchools $ sort $ foldl (++) [] $ map iterateFishSchool xs

-- | Iterate a single school of fish
-- >>> iterateFishSchool (FishSchool 1 (LanternFish 1))
-- [FishSchool 1 (LanternFish 0)]
-- >>> iterateFishSchool (FishSchool 1 (LanternFish 0))
-- [FishSchool 1 (LanternFish 6),FishSchool 1 (LanternFish 8)]
iterateFishSchool :: FishSchool -> [FishSchool]
iterateFishSchool (FishSchool n (LanternFish 0)) = [FishSchool n (LanternFish 6), FishSchool n (LanternFish 8)]
iterateFishSchool (FishSchool n (LanternFish x)) = [FishSchool n (LanternFish x-1)]

-- | Merge sorted schools back together
-- >>> mergeFishSchools [FishSchool 1 (LanternFish 1),FishSchool 2 (LanternFish 1),FishSchool 3 (LanternFish 3)]
-- [FishSchool 3 (LanternFish 1),FishSchool 3 (LanternFish 3)]
mergeFishSchools:: [FishSchool] -> [FishSchool]
mergeFishSchools [] = []
mergeFishSchools l@(x:xs) = let (xs, ys) = break (/=x) l
                         in (
                        foldl (+) (FishSchool 0 0) xs
                        :mergeFishSchools ys
                            )
