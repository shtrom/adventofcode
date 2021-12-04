import System.IO

type Number = Int
type Sheet = [Number]

main = do
        input <- getContents
        print input

-- | Parse an input string into a series of numbers and sheets
-- >>> parseInput "1,2,3\n\n1  2  3\n1  2  3\n\n3  4  5\n3  4  5\n"
-- ([1,2,3],[[[1,2,3],[1,2,3]],[[3,4,5],[3,4,5]]])
parseInput :: String -> ([Number], [Sheet])
parseInput input = let l = lines2 input in
                       (parseNumbers $ head l
                       ,parseSheets $ tail l
                       )

-- | Split input on two consecutive newlines
-- >>> lines2 "1\n2\n\n3,4\n"
-- ["1\n2","3,4\n"]
lines2 :: String -> [String]
lines2 _ = []

-- | Parse an input string into a series of numbers
-- >>> parseNumbers "1,2,3"
-- [1,2,3]
parseNumbers :: String -> [Number]
parseNumbers _ = []

-- | Parse a list of input strings into a series of sheets
-- >>> parseSheets ["1  2  3\n1  2  3", "3  4  5\n3  4  5\n"]
-- [[[1,2,3],[1,2,3]],[[3,4,5],[3,4,5]]]
parseSheets :: [String] -> [Sheet]
parseSheets l = map parseSheet l
--
-- | Parse an input string into a sheet
-- >>> parseSheet "1  2  3\n1  2  3"
-- [[1,2,3],[1,2,3]]
-- >>> parseSheet "3  4  5\n3  4  5\n"
-- [[3,4,5],[3,4,5]]
parseSheet :: String -> Sheet
parseSheet _ = []
