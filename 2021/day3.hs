import System.IO
import AoCUtils

main = do
        input <- getContents
        print $ day31 $ lines input
        print $ day32 $ lines input

-- | Day 3 part 1
-- >>> day31 ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
-- 198
day31 :: [String] -> Int
day31 l = let bitCounts = countBits $ transpose $ map splitToInts l
              gamma = bitsToDec $ map mostCommon bitCounts
              epsilon = bitsToDec $ map leastCommon bitCounts
          in gamma*epsilon

-- | Day 3 part 2
-- >>> day32 ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
-- 230
day32 :: [String] -> Int
day32 l = let matrix = map splitToInts l
              co2 = bitsToDec $ selectRow (mostCommon) matrix
              o2 = bitsToDec $ selectRow (leastCommon) matrix
          in co2*o2

-- | Recusively filter rows based on the bit criterion
-- >>> selectRow (mostCommon) []
-- []
-- >>> selectRow (mostCommon) [[]]
-- []
-- >>> selectRow (mostCommon) [[1]]
-- [1]
-- >>> selectRow (mostCommon) [[1,0],[1,1]]
-- [1,1]
-- >>> selectRow (mostCommon) [[0,0,1],[0,1,0],[0,1,1]]
-- [0,1,1]
selectRow :: ((Int,Int) -> Int) -> [[Int]] -> [Int]
selectRow _ [] = []
selectRow _ [l] = l
selectRow f m = let bitCriterion = f $ countBits1 $ map (head) m
                 in (bitCriterion : (selectRow f $
                         [xs | (x:xs) <- m, x == bitCriterion] ))

-- | Splint a string of digits into an array of ints
-- >>> splitToInts "01189991819992453"
-- [0,1,1,8,9,9,9,1,8,1,9,9,9,2,4,5,3]
splitToInts :: [Char] -> [Int]
splitToInts [] = []
splitToInts (x:xs) = ((read [x] :: Int) : splitToInts xs)

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
-- >>> mostCommon (1,2)
-- 1
-- >>> mostCommon (2,1)
-- 0
-- >>> mostCommon (1,1)
-- 1
mostCommon :: (Int,Int) -> Int
mostCommon (z, o) = case (z>o) of
                       True -> 0
                       False -> 1

leastCommon:: (Int,Int) -> Int
leastCommon x = 1 - (mostCommon x)
