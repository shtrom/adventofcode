import System.IO

main = do
        input <- getContents
        print $ day31 $ lines input
        -- print $ day32 $ lines input

-- | Day 3 part 1
-- >>> day31 ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
-- 198
day31 :: [String] -> Int
day31 l = let bitCounts = countBits $ transpose $ map splitToInts l
              gamma = bitsToDec $ map mostCommon bitCounts
              epsilon = bitsToDec $ map leastCommon bitCounts
          in gamma*epsilon

-- | Day 3 part 2
-- > >> day32 ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
-- 230
-- day32 :: Num a => [String] -> a
-- day32 l = let matrix = map splitToInts l
--               co2 = selectRow (mostCommon) $ countBits1 $ map head matrix
--               o2 = selectRow (leastCommon) $ countBits1 $ map head matrix
--           in co2*o2

-- | Recusively filter rows based on the bit criterion
-- >>> selectRow (mostCommon) [[0,0,1],[0,1,0],[0,1,1]]
-- [0,1,1]
selectRow :: ((Int,Int) -> Int) -> [[Int]] -> [Int]
selectRow f m = let bitCriterion = f $ countBits1 $ map (head) m
                in [bitCriterion]

-- | Splint a string of digits into an array of ints
-- >>> splitToInts "01189991819992453"
-- [0,1,1,8,9,9,9,1,8,1,9,9,9,2,4,5,3]
splitToInts :: [Char] -> [Int]
splitToInts [] = []
splitToInts (x:xs) = ((read [x] :: Int) : splitToInts xs)

-- | Transpose a matrix
-- https://stackoverflow.com/questions/2578930/understanding-this-matrix-transposition-function-in-haskell
-- >>> transpose [[1,2],[3,4]]
-- [[1,3],[2,4]]
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- | Count the number of 0/1 bits in each array of the list
-- >>> countBits [[0,0,1],[0,1,1]]
-- [(2,1),(1,2)]
countBits :: [[Int]] -> [(Int, Int)]
countBits l = [countBits1 ll | ll <- l]

-- | Count the number of 0/1 bits in one array
-- >>> countBits1 [0,0,1]
-- (2,1)
countBits1 :: [Int] -> (Int, Int)
countBits1 ll = (
  (length $ filter (==0) ll)
   ,(length $ filter (==1) ll)
  )

-- | Find the most common bit value from a bitcount
-- >>> mostCommon(1,2)
-- 1
-- >>> mostCommon(2,1)
-- 0
mostCommon :: (Int,Int) -> Int
mostCommon (z, o) = case (z>o) of
                       True -> 0
                       False -> 1

leastCommon:: (Int,Int) -> Int
leastCommon x = 1 - (mostCommon x)

-- | Convert an array of bits to a decimal value
-- >>> bitsToDec [1,0,0]
-- 4
-- >>> bitsToDec [1,0,1]
-- 5
bitsToDec :: [Int] -> Int
bitsToDec l = rBitsToDec $ reverse l

rBitsToDec :: [Int] -> Int
rBitsToDec [] = 0
rBitsToDec (x:xs) = x + 2 * (rBitsToDec xs)
