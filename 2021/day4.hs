import System.IO
import AoCUtils

data Number = Number Int
            | Marked
            deriving (Show, Eq)
type Sheet = [[Number]]

main = do
        input <- getContents
        print input
        -- print $ day41 $ parseInput input

-- | Parse an input string into a series of numbers and sheets
-- >>> parseInput "1,2,3\n\n1  2  3\n1  2  3\n\n3  4  5\n3  4  5\n"
-- ([Number 1,Number 2,Number 3],[[[Number 1,Number 2,Number 3],[Number 1,Number 2,Number 3]],[[Number 3,Number 4,Number 5],[Number 3,Number 4,Number 5]]])
parseInput :: String -> ([Number], [Sheet])
parseInput input = let l = lines2 input in
                       (parseNumbers $ head l
                       ,parseSheets $ tail l
                       )

-- | Split string on every two consecutive newlines
-- >>> lines2 "1\n2\n\n3,4\n"
-- ["1\n2","3,4\n"]
lines2 :: String -> [String]
lines2 "" = []
lines2 s = let (xs, ys) = break2 (=='\n') s
          in case ys of
             [] -> [xs]
             [y] -> [xs, [y]]
             otherwise -> (xs
                          : lines2 (drop 2 ys)
                          )

-- | Split string on two consecutive characters matching the predicate
-- >>> break2 (>1) [1,1,2,1,2,2,1]
-- ([1,1,2,1],[2,2,1])
-- >>> break2 (>1) [2]
-- ([2],[])
break2 :: (a -> Bool) -> [a]-> ([a], [a])
break2 p [] = ([], [])
break2 p (x1:[]) = ([x1], [])
break2 p l@(x1:x2:xs) = case and [p x1, p x2] of
                          True -> ([], l)
                          False -> let (y1, ys) = break2 p (x2:xs)
                                   in ((x1:y1), ys)

-- | Parse an input string into a series of numbers
-- >>> parseNumbers "1,2,3"
-- [Number 1,Number 2,Number 3]
parseNumbers :: String -> [Number]
parseNumbers x = map toNumber $ splitStringAt (==',') x

-- | Parse a list of input strings into a series of sheets
-- >>> parseSheets ["1  2  3\n1  2  3", "3  4  5\n3  4  5\n"]
-- [[[Number 1,Number 2,Number 3],[Number 1,Number 2,Number 3]],[[Number 3,Number 4,Number 5],[Number 3,Number 4,Number 5]]]
parseSheets :: [String] -> [Sheet]
parseSheets l = map parseSheet l

-- | Parse an input string into a sheet
-- >>> parseSheet "1  2  3\n1  2  3"
-- [[Number 1,Number 2,Number 3],[Number 1,Number 2,Number 3]]
-- >>> parseSheet "3  4  5\n3  4  5\n"
-- [[Number 3,Number 4,Number 5],[Number 3,Number 4,Number 5]]
parseSheet :: String -> Sheet
parseSheet s = map ((map toNumber) . words) $ lines s

-- | Parse an input string into a numbers
-- >>> toNumber "1"
-- Number 1
toNumber :: String -> Number
toNumber s = Number $ toInt s

-- markSheet :: Number -> Sheet -> Sheet
-- markSheet n (Sheet s) = map $ (markNumber n) s

-- | Mark a number in a list, if present
-- >>> markNumber (Number 1) [Number 8,Number 1,Number 3,Number 4,Number 2]
-- [Number 8,Marked,Number 3,Number 4,Number 2]
-- >>> markNumber (Number 9) [Number 8,Number 1,Number 3,Number 4,Number 2]
-- [Number 8,Number 1,Number 3,Number 4,Number 2]
markNumber :: Number -> [Number] -> [Number]
markNumber _ [] = []
markNumber n (x:xs) | x == n = (Marked:xs)
  | otherwise = (x:markNumber n xs)
