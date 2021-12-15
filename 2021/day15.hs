import System.IO
import AoCUtils

type Risk = Int
type Cave = ([Risk], [Risk], [[Risk]])

main = do input <- getContents
          let cave = parseCave $ map (map (\x -> read [x] ::Int)) $ lines input
          print $ day151 cave

-- | Find the shortest path in the cave
-- >>> day151([1,6,3,7,5,1,7,4,2],[2,3,7,1,1,3,1,2],[[3,8,1,3,7,3,6,7,2],[1,3,6,5,1,1,3,2,8],[6,9,4,9,3,1,5,6,9],[4,6,3,4,1,7,1,1,1],[3,1,9,1,2,8,1,3,7],[3,5,9,9,1,2,4,2,1],[1,2,5,4,2,1,6,3,9],[2,9,3,1,3,8,5,2,1],[3,1,1,9,4,4,5,8,1]])
-- 40
day151 = sum . lowestRiskPath

-- | Parse a cave into up, left and rest components
-- >>> parseCave [[1,1,6,3,7,5,1,7,4,2],[1,3,8,1,3,7,3,6,7,2],[2,1,3,6,5,1,1,3,2,8],[3,6,9,4,9,3,1,5,6,9],[7,4,6,3,4,1,7,1,1,1],[1,3,1,9,1,2,8,1,3,7],[1,3,5,9,9,1,2,4,2,1],[3,1,2,5,4,2,1,6,3,9],[1,2,9,3,1,3,8,5,2,1],[2,3,1,1,9,4,4,5,8,1]]
-- ([1,6,3,7,5,1,7,4,2],[2,3,7,1,1,3,1,2],[[3,8,1,3,7,3,6,7,2],[1,3,6,5,1,1,3,2,8],[6,9,4,9,3,1,5,6,9],[4,6,3,4,1,7,1,1,1],[3,1,9,1,2,8,1,3,7],[3,5,9,9,1,2,4,2,1],[1,2,5,4,2,1,6,3,9],[2,9,3,1,3,8,5,2,1],[3,1,1,9,4,4,5,8,1]])
parseCave :: [[Int]] -> Cave
parseCave (x:xs) = let up = tail x
                       left = tail $ map head xs
                       rest = map tail xs
                    in (up, left, rest)

-- | Find the lowest-risk path through the cave
-- >>> lowestRiskPath ([],[9],[])
-- [9]
-- >>> lowestRiskPath ([6],[8],[[9]])
-- [6,9]
-- >>> lowestRiskPath ([2,3],[4,7],[[5,6],[8,9]])
-- [2,3,6,9]
lowestRiskPath :: Cave -> [Risk]
lowestRiskPath (_,[x],[]) = [x]
lowestRiskPath ([x],_,[]) = [x]
lowestRiskPath (_,_,[[x]]) = [x]
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
goRight :: Cave -> Cave
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
goDown :: Cave -> Cave
goDown (_,_,[]) = ([],[],[])
goDown (up,left,rest) = (
  head rest
  ,tail left
  ,tail rest)
