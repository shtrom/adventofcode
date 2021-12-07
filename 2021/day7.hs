import System.IO
import AoCUtils

main = do
        line <- getContents
        print $ day71 $ map toInt $ splitStringAt (==',') $ head $ lines line

-- | Day7 part 1
-- >>> day71 [16,1,2,0,4,2,7,1,2,14]
-- 37
day71 :: [Int] -> Int
day71 l = sum $ map (abs . ((-) $ median l)) l

-- | Calculate a median, almost correctly
-- >>> median [7,1,3,2,3,9,1]
-- 3
median :: [Int] -> Int
median l = head $ drop (div (length l) 2) $ sort l
