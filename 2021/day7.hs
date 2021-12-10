import System.IO
import AoCUtils

main = do
        line <- getContents
        print $ day71 $ map toInt $ splitStringAt (==',') $ head $ lines line
        print $ day72 $ map toInt $ splitStringAt (==',') $ head $ lines line

-- | Day7 part 1
-- >>> day71 [16,1,2,0,4,2,7,1,2,14]
-- 37
day71 :: [Int] -> Int
day71 l = sum $ map (abs . ((-) $ median l)) l

-- | Day7 part 2
-- >>> day72 [16,1,2,0,4,2,7,1,2,14]
-- 168
day72 :: [Int] -> Int
day72 l = sum $ map (fuelConsumption . abs . ((-) $ mean l)) l

-- | Calculate an integral mean, almost correctly
-- >>> mean [1,2,8,10,25]
-- 9
mean :: [Int] -> Int
mean l = round $ realToFrac ((fromIntegral $ sum l) / (fromIntegral $ length l))

-- | Calculate the fuel consumption for a given movement
-- >>> fuelConsumption 11
-- 66
fuelConsumption :: Int -> Int
fuelConsumption n = sum $ [1..n]
