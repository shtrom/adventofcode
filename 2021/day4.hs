import System.IO
import AoCUtils

data Number = Number Int
            | Marked
            deriving (Show, Eq)
type Sheet = [[Number]]

main = do
        input <- getContents
        let (numbers, sheets) = parseInput input
        print $ day41 numbers sheets
        print $ day42 numbers sheets

day41 :: [Number] -> [Sheet] -> Int
day41 [] _ = error "No winner"
day41 (n:ns) ss = let ss' = map (markSheet n) ss
                  in case filter sheetMarked ss' of
                       (s':_) -> (fromNumber n) * scoreSheet s'
                       otherwise -> day41 ns ss'

-- day42 :: [Number] -> [Sheet] -> Int
day42 [] _ = error "No winner"
day42 (n:ns) ss = let ss' = filter (not . sheetMarked) $ map (markSheet n) ss
                  in case ss' of
                       (s':[]) -> day41 ns ss'
                       otherwise -> day42 ns ss'

-- | Parse an input string into a series of numbers and sheets
-- >>> parseInput "1,2,3\n\n1  2  3\n1  2  3\n\n3  4  5\n3  4  5\n"
-- ([Number 1,Number 2,Number 3],[[[Number 1,Number 2,Number 3],[Number 1,Number 2,Number 3]],[[Number 3,Number 4,Number 5],[Number 3,Number 4,Number 5]]])
parseInput :: String -> ([Number], [Sheet])
parseInput input = let l = lines2 input in
                       (parseNumbers $ head l
                       ,parseSheets $ tail l
                       )

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

-- Convert a number back to an int, with a sensible value for marked ones
-- >>> fromNumber Marked
-- 0
-- >>> fromNumber Number 10
-- 10
fromNumber :: Number -> Int
fromNumber Marked = 0
fromNumber (Number n) = n


-- | Mark a number in a Sheet if present
-- >>> markSheet (Number 1) [[Number 8,Number 1,Number 3,Number 4,Number 2],[Number 9,Number 5,Number 7,Number 6,Number 2]]
-- [[Number 8,Marked,Number 3,Number 4,Number 2],[Number 9,Number 5,Number 7,Number 6,Number 2]]
-- >>> markSheet (Number 9) [[Number 8,Number 1,Number 3,Number 4,Number 2],[Number 9,Number 5,Number 7,Number 6,Number 2]]
-- [[Number 8,Number 1,Number 3,Number 4,Number 2],[Marked,Number 5,Number 7,Number 6,Number 2]]
markSheet :: Number -> Sheet -> Sheet
markSheet n s = map (markNumber n) s

-- | Mark a number in a list, if present
-- >>> markNumber (Number 1) [Number 8,Number 1,Number 3,Number 4,Number 2]
-- [Number 8,Marked,Number 3,Number 4,Number 2]
-- >>> markNumber (Number 9) [Number 8,Number 1,Number 3,Number 4,Number 2]
-- [Number 8,Number 1,Number 3,Number 4,Number 2]
markNumber :: Number -> [Number] -> [Number]
markNumber _ [] = []
markNumber n (x:xs) | x == n = (Marked:xs)
  | otherwise = (x:markNumber n xs)

-- | Test if all numbers in a line are Marked
-- >>> sheetMarked [[Number 1,Marked],[Number 3,Number 4]]
-- False
-- >>> sheetMarked [[Marked,Marked],[Number 3,Number 4]]
-- True
-- >>> sheetMarked [[Number 1,Marked],[Number 3,Marked]]
-- True
sheetMarked :: Sheet -> Bool
sheetMarked s = (any (==True) $ map lineMarked s)
        || (any (==True) $ map lineMarked $ transpose s)

-- | Test if all numbers in a line are Marked
-- >>> lineMarked [Number 1,Marked]
-- False
-- >>> lineMarked [Marked,Marked]
-- True
lineMarked :: [Number] -> Bool
lineMarked = all (==Marked)

-- | Score a sheet by summing the unmarked numbers
-- >>> scoreSheet [[Number 1,Marked],[Number 3,Number 4]]
-- 8
-- >>> scoreSheet [[Marked,Marked],[Number 3,Number 4]]
-- 7
-- >>> scoreSheet [[Number 1,Marked],[Number 3,Marked]]
-- 4
scoreSheet :: Sheet -> Int
scoreSheet s = sum $ map (sum . map fromNumber) s
