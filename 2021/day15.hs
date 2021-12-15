import System.IO
import AoCUtils

type Position = Int
type Cave = ([Position], [Position], [[Position]])

main = do input <- getContents
          print $ map (map (\x -> read [x] ::Int)) $ lines input

-- | Parse a cave into top, left and rest components
-- >>> parseCave [1163751742,1381373672,2136511328,3694931569,7463417111,1319128137,1359912421,3125421639,1293138521,2311944581]
-- parseCave :: [Int] -> Cave
-- parseCave (x:xs) = let top = tail 
