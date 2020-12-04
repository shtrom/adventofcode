import System.IO

type CoordX = Int
type CoordY = Int
data Slope = Slope CoordX CoordY deriving Show

type Square = Char
type Row = [Square]
type Area  = [Row]
type Count = Int

main = do
  input <- getContents
  let area = lines input
  -- Part 1
  print $ countTrees (Slope 3 1) area
  -- Part 2
  print $ foldl (\acc s -> acc * (countTrees s area)) 1 [ (Slope 1 1)
                                                         , (Slope 3 1)
                                                         , (Slope 5 1)
                                                         , (Slope 7 1)
                                                         , (Slope 1 2)
                                                         ]

countTrees :: Slope -> Area -> Count
countTrees s@(Slope x y) a = countRemainingTrees s 0 0  a

countRemainingTrees :: Slope -> CoordX -> CoordY -> Area -> Count
countRemainingTrees _ _ _ [] = 0
countRemainingTrees (Slope 0 _) curX _ (a:_) = countTreeAt curX a
countRemainingTrees s@(Slope x y) curX 0 aa@(a:as) = countTreeAt curX a
  + countRemainingTrees s (curX+x) (y-1) as -- !!! If we use as, we've already consumed 1 y shift for the next step
countRemainingTrees s curX nextY (a:as) = countRemainingTrees s curX (nextY-1) as

countTreeAt :: CoordX -> Row -> Count
-- countTreeAt x a = countTreeAtRepeat x a a
countTreeAt x a = countTreeAtMod (x `mod` (length a)) a

-- slower
countTreeAtRepeat :: CoordX -> Row -> Row -> Count
countTreeAtRepeat 0 (s:_) _ = countTree s
countTreeAtRepeat 0 [] (s:ss) = countTree s
countTreeAtRepeat x [] sq@(s:ss) = countTreeAtRepeat (x-1) ss sq
countTreeAtRepeat x (s:ss) sq = countTreeAtRepeat (x-1) ss sq

-- faster
countTreeAtMod :: CoordX -> Row -> Count
countTreeAtMod 0 (s:_) = countTree s
countTreeAtMod x (_:ss) = countTreeAtMod (x-1) ss

countTree :: Square -> Count
countTree '.' = 0
countTree '#' = 1
