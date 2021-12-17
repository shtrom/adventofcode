module AoCUtils (
        Aggregate
        ,BitString
        ,aggregate
        ,bitStringToBits
        ,bitsToDec
        ,break2
        ,lines2
        ,median
        ,mergeAggregates
        ,multAggregate
        ,pivotSort
        ,sort
        ,sortAggregates
        ,splitStringAt
        ,toInt
        ,cToInt
        ,transpose
                ) where

type Aggregate a = (Int, a)

-- | Given a sorted list, count the re-occurence of each element
-- >>> aggregate [1,2,2,3,3,3]
-- [(1,1),(2,2),(3,3)]
aggregate :: Eq a => [a] -> [Aggregate a]
aggregate [] = []
aggregate l@(x:xs) = let (xs, ys) = break (/=x) l
                         c = length xs
                    in (
                       (c, x)
                       :aggregate ys
                    )

type BitString = String
type Bit = Int

-- | Convert a bit string to a list of Int-like bits
-- >>> bitStringToBits "0011"
-- [0,0,1,1]
bitStringToBits :: String -> [Bit]
bitStringToBits = map (toInt . (flip (:) []))

-- | Convert an array of bits to a decimal value
-- >>> bitsToDec [1,0,0]
-- 4
-- >>> bitsToDec [1,0,1]
-- 5
bitsToDec :: [Bit] -> Int
bitsToDec l = bitsToDec' $ reverse l

bitsToDec' :: [Int] -> Int
bitsToDec' [] = 0
bitsToDec' (x:xs) = x + 2 * (bitsToDec' xs)

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

-- | Merge sorted aggregates
-- >>> mergeAggregates [(1,1),(2,2),(3,3)] [(1,1),(2,2),(3,3)]
-- [(2,1),(4,2),(6,3)]
mergeAggregates :: (Ord a, Eq a) => [Aggregate a] -> [Aggregate a] -> [Aggregate a]
mergeAggregates a [] = a
mergeAggregates [] b = b
mergeAggregates a@((ac,av):as) b@((bc,bv):bs)
  | av==bv = [(ac+bc,av)] ++ mergeAggregates as bs
  | av<bv = [(ac,av)] ++ mergeAggregates as b
  | av>bv = [(bc,bv)] ++ mergeAggregates a bs

-- | Multiply a aggregate count by a given number
-- >>> multAggregate 3 (1,2)
-- (3,2)
multAggregate :: Int -> Aggregate a -> Aggregate a
multAggregate n (c,v) = (c*n,v)

-- | Pivot sort with arbitrary comparison functions
-- >>> pivotSort (<) (>=) [2,8,4,9,1,0]
-- [0,1,2,4,8,9]
-- >>> pivotSort (>) (<=) [2,8,4,9,1,0]
-- [9,8,4,2,1,0]
pivotSort :: (a -> a -> Bool) -> (a -> a -> Bool) -> [a] -> [a]
pivotSort _ _ [] = []
pivotSort _ _ [x] = [x]
pivotSort lt ge (x:xs) = (pivotSort lt ge $ filter (flip lt x) xs) ++ [x] ++ (pivotSort lt ge $ filter (flip ge x) xs)

-- | Sort a list of comparables
-- >>> sort [1,9,2,8]
-- [1,2,8,9]
sort :: Ord a => [a] -> [a]
sort = pivotSort (<) (>=)

-- | Sort aggregates by aggregated value
-- >>> sortAggregates [(1,3),(2,1),(3,2)]
-- [(2,1),(3,2),(1,3)]
sortAggregates :: Ord a => [Aggregate a] -> [Aggregate a]
sortAggregates = pivotSort (\(_,v1) -> \(_,v2) -> v1 < v2) (\(_,v1) -> \(_,v2) -> v1 >= v2)

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

