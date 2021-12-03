import System.IO

main = do
        input <- getContents
        print $ day31 $ lines input

day31 :: [String] -> Int
day31 l = let bitCounts = countBits $ transpose $ map splitToInts l
              gamma = bitsToDec $ map mostCommon bitCounts
              epsilon = bitsToDec $ map leastCommon bitCounts
          in gamma*epsilon

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
countBits l = [(
  (length $ filter (==0) ll)
   ,(length $ filter (==1) ll)
  ) | ll <- l]

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
