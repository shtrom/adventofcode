import System.IO

main = do
        input <- getContents
        let measurements = makeInts $ lines input
        let differences = makeDiffs measurements
        let drops = length $ filter (<1) differences
        print drops

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
