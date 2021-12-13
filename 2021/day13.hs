import System.IO
import AoCUtils

data Direction = X | Y deriving Show
data Fold = Fold Direction Int deriving Show

type Dot = (Int, Int)

main = do input <- getContents
          let (d:f:[]) = map lines $ lines2 input
              dots = map parseDots d
              folds = map parseFolds f
          print $ day131 (head folds) dots
          putStr $ day132 folds dots

-- | Parse dot coordinates
-- >>> parseDots "6,10"
-- (6,10)
parseDots :: String -> Dot
parseDots s = let (x:y:_) = map toInt $ splitStringAt (==',') s
               in (x,y)

-- | Parse fold instructions
-- >>> parseFolds "fold along y=7"
-- Fold Y 7
-- >>> parseFolds "fold along x=5"
-- Fold X 5
parseFolds :: String -> Fold
parseFolds s = let (d:i:_) = splitStringAt (== '=') $ last $ words s
                   direction = case d of
                                 "x" -> X
                                 "y" -> Y
                  in Fold direction (toInt i)

-- | Day 13 part 1
-- >>> day131 (Fold Y 7) [(6,10),(0,14),(9,10),(0,3),(10,4),(4,11),(6,0),(6,12),(4,1),(0,13),(10,12),(3,4),(3,0),(8,4),(1,10),(2,14),(8,10),(9,0)]
-- 17
day131 :: Fold -> [Dot] -> Int
day131 f d = length $ uniq $ sort $ map (foldDot f) d

-- | Day 13 part 2
day132 :: [Fold] -> [Dot] -> String
day132 f d = makeLines $ map swapXY $ uniq $ sort $ map swapXY $ applyFolds f d

swapXY :: (a,b) -> (b,a)
swapXY (x,y) = (y,x)

-- | Render the dots
-- XXX: Does not support more than one newline at a time
-- >>> makeLines [(1,0),(3,0),(2,1),(0,2)]
-- "\n.#.#\n..#\n#"
makeLines :: [Dot] -> String
makeLines [] = []
-- makeLines s@((x,y):_) = (take (y) $ repeat '\n')
--                         ++ (take (x) $ repeat '.') ++ makeLine s
makeLines s@((x,y):_) = "\n"
                        ++ (take (x) $ repeat '.') ++ makeLine s
makeLine :: [Dot] -> String
makeLine [] = []
makeLine [x] = "#"
makeLine (d:d':ds) = let (x,y) = d
                         (x',y') = d'
                       in case y==y' of
                            True -> "#" ++ (take (x'-x-1) $ repeat '.')
                                    ++ makeLine (d':ds)
                            False -> "#" ++ makeLines (d':ds)

-- | Deduplicate sorted list
-- >>> uniq [1,1,2,3,4,4,5]
-- [1,2,3,4,5]
uniq :: Eq a => [a] -> [a]
uniq [x] = [x]
uniq (x:x':xs) = case x==x' of
                   True -> uniq (x':xs)
                   False -> x : uniq (x':xs)

-- | Apply folds to li-t of dots
-- >>> applyFolds [Fold X 5,Fold Y 5] [(10,10)]
-- [(0,0)]
-- >>> applyFolds [Fold X 7,Fold X 10] [(10,10)]
-- [(4,10)]
-- >>> applyFolds [Fold X 10,Fold X 7] [(10,10)]
-- [(4,10)]
applyFolds :: [Fold] -> [Dot] -> [Dot]
applyFolds [] d = d
applyFolds (f:fs) d = applyFolds fs $ map (foldDot f) d

-- | Fold one dot
-- >>> foldDot (Fold X 10) (11,1)
-- (9,1)
-- >>> foldDot (Fold Y 10) (1,11)
-- (1,9)
-- >>> foldDot (Fold X 10) (9,1)
-- (9,1)
-- >>> foldDot (Fold Y 10) (1,9)
-- (1,9)
foldDot :: Fold -> Dot -> Dot
foldDot (Fold d i) (x,y) = case d of
                           X -> case x>i of
                                  True -> (2*i-x,y)
                                  False -> (x,y)
                           Y -> case y>i of
                                  True-> (x,2*i-y)
                                  False -> (x,y)
