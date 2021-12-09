import System.IO
import AoCUtils

main = do line <-getContents
          print $ day91 $ lines line
          print $ day92 $ lines line

-- | Compute risk
-- >>> day91 ["2199943210","3987894921","9856789892","8767896789","9899965678"]
-- 15
day91 :: [String] -> Int
day91 l = sum $ map (1+) $ map cToInt $ findLowPoints l

-- | Find basins
-- >>> day92 ["2199943210","3987894921","9856789892","8767896789","9899965678"]
-- 1134
day92 :: [String] -> Int
day92 l = 0

-- | Find low points in a 2D matrix
-- >>> findLowPoints ["219", "398","985"]
-- "15"
findLowPoints :: (Bounded a, Ord a) => [[a]] -> [a]
findLowPoints [] = []
findLowPoints s = findLowPointsEast [] [] s ++ findLowPointsSouth s

findLowPointsSouth [] = []
findLowPointsSouth (x:xs) = findLowPointsEast x [] xs ++ findLowPointsSouth xs


-- | Find low points given a north and west row, and the rest of a matrix to the
-- east
-- >>> findLowPointsEast [] []  ["219", "398","985"]
-- "1"
-- >>> findLowPointsEast "19" "39" ["98", "85"]
-- ""
-- >>> findLowPointsEast "9" "9" ["8", "5"]
-- ""
-- >>> findLowPointsEast "98" "9" ["85"]
-- "5"
findLowPointsEast :: (Bounded a, Ord a) => [a] -> [a] -> [[a]] -> [a]
findLowPointsEast _ _ [] = []
findLowPointsEast _ _ [[]] = []
findLowPointsEast ns ws l@(x:xs) = let (n, e, s, w, p) = getPoints ns ws l
                                       (ens, ews, el) = getNextEast ns ws l
                                       elp = findLowPointsEast ens ews el
                                       -- (sns, sws, sl) = getNextSouth ns ws l
                                       -- slp = findLowPoints' sns sws sl
                                    in case isLowPoint n e s w p of
                                         True -> [p] ++ elp -- ++ slp
                                         False -> [] ++ elp -- ++ slp

-- | Extract the points, wich sensible defaults
-- >>> getPoints [1,2] [3,4] [[5,6],[7,8 :: Int]]
-- (1,6,7,3,5)
-- >>> getPoints [] [] [[5,6],[7,8 :: Int]]
-- (9223372036854775807,6,7,9223372036854775807,5)
-- >>> getPoints [] []  ["219", "398","985"]
-- ('\1114111','1','3','\1114111','2')
-- >>> getPoints "98" "9" ["85"]
-- ('9','5','\1114111','9','8')
-- >>> getPoints "8" "8" ["5"]
-- ('8','\1114111','\1114111','8','5')
-- >>> getPoints "85" "9" []
-- *** Exception: nothing left to extract
-- ...
getPoints :: Bounded a => [a] -> [a] -> [[a]] -> (a, a, a, a, a)
getPoints _ _ [] = error "nothing left to extract"
getPoints ns ws (x:xs) = let p = case x of
                                   [] -> maxBound
                                   _ -> head x
                             n = case ns of
                                   [] -> maxBound
                                   otherwise -> head ns
                             e = case x of
                                   [] -> maxBound
                                   (y:[]) -> maxBound
                                   (y:ys) -> head ys
                             s = case xs of
                                   [] -> maxBound
                                   (y:ys) -> head y
                             w = case ws of
                                   [] -> maxBound
                                   otherwise -> head ws
                           in (n, e, s, w, p)

-- | Shift the data east
-- >>> getNextEast [] []  ["219", "398","985"]
-- ("","239",["19","98","85"])
-- >>> getNextEast [] "198"  ["9", "8","5"]
-- ("","985",[])
-- >>> getNextEast "" "985" []
-- ("","",[])
getNextEast:: Bounded a => [a] -> [a] -> [[a]] -> ([a], [a], [[a]])
getNextEast _ _ [] = ([],[],[])
getNextEast ns ws l = let ens = case ns of
                                  [] -> []
                                  _ -> tail ns
                          ews = case l of
                                  -- [] -> []
                                  ([]:_) -> []
                                  _ -> map head l
                          el = case l of
                                 -- [] -> []
                                 _ -> filter (not . empty) $ map tail l
                       in (ens, ews, el)

empty :: Foldable t => t a -> Bool
empty x = length x == 0

-- | Shift the data south
-- >>> getNextSouth [] []  ["219", "398","985"]
-- ("219","",["398","985"])
-- >>> getNextSouth "398" []  ["985"]
-- ("985","",[])
-- >>> getNextSouth "985" "" []
-- ("","",[])
getNextSouth:: Bounded a => [a] -> [a] -> [[a]] -> ([a], [a], [[a]])
getNextSouth _ _ [] = ([],[],[])
getNextSouth ns ws l = let sns = case l of
                                  -- [] -> []
                                  _ -> head l
                           sws = case ws of
                                  [] -> []
                                  _ -> tail ws
                           sl = case l of
                                 -- [] -> []
                                 _ -> tail l
                         in (sns, sws, sl)

-- | Determine if a point is the lowest of its vicinity
-- >>> isLowPoint 9 9 9 9 1
-- True
-- >>> isLowPoint 1 9 9 9 5
-- False
-- >>> isLowPoint 9 1 9 9 5
-- False
-- >>> isLowPoint 9 9 1 9 5
-- False
-- >>> isLowPoint 9 9 9 1 5
-- False
-- >>> isLowPoint '\1114111' '1' '3' '\1114111' '2'
-- False
-- >>> isLowPoint '\1114111' '4' '3' '\1114111' '2'
-- True
-- >>> isLowPoint 9 9 9 9 9
-- False
isLowPoint :: Ord a => a -> a -> a -> a -> a -> Bool
isLowPoint n e s w p = all (>p) [n,e,s,w]
