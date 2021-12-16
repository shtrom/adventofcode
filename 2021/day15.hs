import System.IO
import AoCUtils

type Risk = Int
type Cave = ([Risk], [Risk], [[Risk]])

main = do input <- getContents
          let cave =  map (map (\x -> read [x] ::Int)) $ lines input
              parsedCave = parseCave cave
          -- print $ day151 parsedCave
          -- print cave
          print $ day151' cave

-- | Find the shortest path in the cave
-- >>> day151 ([1,6,3,7],[1,2],[[3,8,1,3],[1,3,6,5]])
-- 18
-- >>> day151 ([2,3],[4,7],[[5,6],[8,9]])
-- 20
-- >>> day151 ([1,6,3,7,5,1,7,4,2],[1,2,3,7,1,1,3,1,2],[[3,8,1,3,7,3,6,7,2],[1,3,6,5,1,1,3,2,8],[6,9,4,9,3,1,5,6,9],[4,6,3,4,1,7,1,1,1],[3,1,9,1,2,8,1,3,7],[3,5,9,9,1,2,4,2,1],[1,2,5,4,2,1,6,3,9],[2,9,3,1,3,8,5,2,1],[3,1,1,9,4,4,5,8,1]])
-- 40
day151 = sum . lowestRiskPath

-- | Find the shortest path in the cave
-- >>> day151' [[1,1,6,3,7,5,1,7,4,2],[1,3,8,1,3,7,3,6,7,2],[2,1,3,6,5,1,1,3,2,8],[3,6,9,4,9,3,1,5,6,9],[7,4,6,3,4,1,7,1,1,1],[1,3,1,9,1,2,8,1,3,7],[1,3,5,9,9,1,2,4,2,1],[3,1,2,5,4,2,1,6,3,9],[1,2,9,3,1,3,8,5,2,1],[2,3,1,1,9,4,4,5,8,1]]
-- 40
day151' c = let w = length $ head c
                h = length c
             in sum $ lowestRiskPath' c (w-1) (h-1)

-- | Parse a cave into up, left and rest components
-- >>> parseCave [[1,1,6,3,7,5,1,7,4,2],[1,3,8,1,3,7,3,6,7,2],[2,1,3,6,5,1,1,3,2,8],[3,6,9,4,9,3,1,5,6,9],[7,4,6,3,4,1,7,1,1,1],[1,3,1,9,1,2,8,1,3,7],[1,3,5,9,9,1,2,4,2,1],[3,1,2,5,4,2,1,6,3,9],[1,2,9,3,1,3,8,5,2,1],[2,3,1,1,9,4,4,5,8,1]]
-- ([1,6,3,7,5,1,7,4,2],[1,2,3,7,1,1,3,1,2],[[3,8,1,3,7,3,6,7,2],[1,3,6,5,1,1,3,2,8],[6,9,4,9,3,1,5,6,9],[4,6,3,4,1,7,1,1,1],[3,1,9,1,2,8,1,3,7],[3,5,9,9,1,2,4,2,1],[1,2,5,4,2,1,6,3,9],[2,9,3,1,3,8,5,2,1],[3,1,1,9,4,4,5,8,1]])
parseCave :: [[Int]] -> Cave
parseCave (x:xs) = let up = tail x
                       left = map head xs
                       rest = map tail xs
                    in (up, left, rest)

-- | From [0]
-- [0] https://stackoverflow.com/a/5553390/10660788
memo :: (Int -> Int -> a) -> [[a]]
memo f = map (\x -> map (f x) [0..]) [0..]

pathStore :: [[Risk]] -> [[[Risk]]]
pathStore c = memo (lowestRiskPath' c)

fastLowestRiskPath :: [[Risk]] -> Int -> Int -> [Risk]
fastLowestRiskPath c x y = pathStore c !! x !! y

-- | Memoised version of lowestRiskPath'
-- >>> lowestRiskPath' [[1,2,3],[4,5,6],[7,8,9]] 0 0
-- []
-- >>> lowestRiskPath' [[1,2,3],[4,5,6],[7,8,9]] 1 1
-- [6,9]
-- >>> lowestRiskPath' [[1,2,3],[4,5,6],[7,8,9]] 2 2
-- [2,3,6,9]
lowestRiskPath' :: [[Risk]] -> Int -> Int -> [Risk]
lowestRiskPath' c 0 0 = []
lowestRiskPath' c x y
  | y >= (length c) = error "y too large"
  | x >= (length $ head c) = error "x too large"
  | otherwise = let w = length c
                    h = length $ head c
                    x' = w-x-1
                    y' = h-y-1
                    rightPath = case x-1 >= 0 of
                                  True -> [c !! (x'+1) !! y'] ++ (fastLowestRiskPath c (x-1) y)
                                  False -> [div maxBound 2]
                    downPath = case y-1 >= 0 of
                                 True -> [c !! x' !! (y'+1)] ++ (fastLowestRiskPath c x (y-1))
                                 False -> [div maxBound 2]
                 in case (sum downPath)<(sum rightPath) of
                      True -> downPath
                      False -> rightPath

-- | Find the lowest-risk path through the cave
-- [9]
-- >>> lowestRiskPath ([9],[],[])
-- [9]
-- >>> lowestRiskPath ([],[9],[])
-- [9]
-- >>> lowestRiskPath ([6],[8],[[9]])
-- [6,9]
-- >>> lowestRiskPath ([2,3],[4,7],[[5,6],[8,9]])
-- [2,3,6,9]
lowestRiskPath :: Cave -> [Risk]
lowestRiskPath ([],[x],[]) = [x]
lowestRiskPath ([x],[],[]) = [x]
-- lowestRiskPath ([],[],[[x]]) = [x]
-- lowestRiskPath cave@([],[],_) = []
lowestRiskPath cave@(up,left,rest) = let rightPath = case up of
                                                       [] -> [div maxBound 2]
                                                       _ -> [head up] ++ (lowestRiskPath $ goRight cave)
                                         downPath = case left of
                                                      [] -> [div maxBound 2]
                                                      _ -> [head left] ++ (lowestRiskPath $ goDown cave)
                                      in case (sum downPath)<(sum rightPath) of
                                           True -> downPath
                                           False -> rightPath

-- | Go right in the cave
-- >>> goRight ([2,3],[4,7],[[5,6],[8,9]])
-- ([3],[5,8],[[6],[9]])
-- >>> goRight ([3],[5,8],[[6],[9]])
-- ([],[6,9],[])
-- >>> goRight ([6],[8],[[9]])
-- ([],[9],[])
-- >>> goRight ([8,9],[],[])
-- ([9],[],[])
goRight :: Cave -> Cave
goRight ((x:xs),_,[]) = (xs,[],[])
goRight (_,_,[]) = ([],[],[])
goRight (up,left,rest) = let newRest = map tail rest
                          in (
  tail up
  ,map head rest
  ,case (sum $ map length newRest) of
      0 -> []
      _ -> newRest
  )

-- | Go down in the cave
-- >>> goDown ([2,3],[4,7],[[5,6],[8,9]])
-- ([5,6],[7],[[8,9]])
-- >>> goDown ([5,6],[7],[[8,9]])
-- ([8,9],[],[])
-- >>> goDown([6],[8],[[9]])
-- ([9],[],[])
-- >>> goDown ([],[6,9],[])
-- ([],[9],[])
goDown :: Cave -> Cave
goDown (_,(x:xs),[]) = ([],xs,[])
goDown (_,_,[]) = ([],[],[])
goDown (up,left,rest) = (
  head rest
  ,tail left
  ,tail rest)
