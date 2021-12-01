import System.IO

main = do
        input <- getContents
        let measurements = makeInts $ lines input
        let differences = makeDiffs measurements
        let drops = length $ filter (<0) differences
        print drops
        let groups = groupSums $ makeGroups measurements
        let groupDiff = makeDiffs groups
        let raises = length $ filter (<0) groupDiff
        print raises

-- | Make a list of Ints from a list of Strings
-- >>> makeInts ["1", "2"]
-- [1,2]
makeInts :: [String] -> [Int]
makeInts [] = []
makeInts (x:xs) = (makeInt x : makeInts xs)

-- | Make an Int from a String
-- >>> makeInt "1"
-- 1
makeInt :: String -> Int
makeInt s = read s :: Int

-- | Calculate differentes from successive numbers
-- >>> makeDiffs []
-- []
-- >>> makeDiffs [1]
-- []
-- >>> makeDiffs [1,2]
-- [-1]
makeDiffs :: Num a => [a] -> [a]
makeDiffs [] = []
makeDiffs (x:xs) = zipWith (-) (x:xs) xs

-- | Make consecutive groups of three int
-- >>> makeGroups []
-- []
-- >>> makeGroups [1]
-- []
-- >>> makeGroups [1,2]
-- []
-- >>> makeGroups [1,2,3]
-- [(1,2,3)]
-- >>> makeGroups [1,2,3,4]
-- [(1,2,3),(2,3,4)]
-- >>> makeGroups [1,2,3,4,5]
-- [(1,2,3),(2,3,4),(3,4,5)]
makeGroups :: [a] -> [(a, a, a)]
makeGroups l = zip3 l (drop 1 l) (drop 2 l)

-- | Make sums from triples
-- >>> groupSums [(1,2,3),(2,3,4),(3,4,5)]
-- [6,9,12]
groupSums :: Num a => [(a,a,a)] -> [a]
groupSums l= [x+y+z | (x,y,z) <- l]
