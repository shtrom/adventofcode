module AoCUtils (
        aggregate
        ,break2
        ,lines2
        ,median
        ,sort
        ,splitStringAt
        ,toInt
        ,cToInt
        ,transpose
                ) where

-- | Given a sorted list, count the re-occurence of each element
-- >>> aggregate [1,2,2,3,3,3]
-- [(1,1),(2,2),(3,3)]
aggregate :: Eq a => [a] -> [(Int, a)]
aggregate [] = []
aggregate l@(x:xs) = let (xs, ys) = break (/=x) l
                         c = length xs
                    in (
                       (c, x)
                       :aggregate ys
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

-- | Calculate a median, almost correctly
-- >>> median [7,1,3,2,3,9,1]
-- 3
median :: [Int] -> Int
median l = head $ drop (div (length l) 2) $ sort l

-- | Sort a list of comparables
-- >>> sort [1,9,2,8]
-- [1,2,8,9]
sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:xs) = (sort $ filter (<x) xs) ++ [x] ++ (sort $ filter (>=x) xs)

-- | Split String at arbitrary character matching predicate
-- >>> splitStringAt (==',') "1,2,3"
-- ["1","2","3"]
splitStringAt :: (Char -> Bool) -> String -> [String]
splitStringAt _ "" = [""]
splitStringAt p s = let (xs, ys) = break p s
          in case ys of
             [] -> [xs]
             [y] -> [xs, [y]]
             otherwise -> (xs
                          : splitStringAt p (tail ys)
                          )

-- | Convert string to an Int
-- >>> toInt "2"
-- 2
toInt :: String -> Int
toInt s = read s ::Int

-- | Convert char to an Int
-- >>> cToInt '2'
-- 2
cToInt :: Char -> Int
cToInt c = read [c] ::Int

-- | Transpose a matrix
-- https://stackoverflow.com/questions/2578930/understanding-this-matrix-transposition-function-in-haskell
-- >>> transpose [[1,2],[3,4]]
-- [[1,3],[2,4]]
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

