import System.IO
import AoCUtils

type Coords = (Int, Int)
type Segment = (Coords, Coords)

main = do line <- getContents
          print $ parseSegments $ lines line

-- | Parse segments coordinates from a string
-- >>> parseSegments ["0,9 -> 5,9", "8,0 -> 0,8"]
-- [((0,9),(5,9)),((8,0),(0,8))]
-- >>> parseSegments ["0,9 -> 5,9, 8,0 -> 0,8"]
-- [*** Exception: malformed line 0,9 -> 5,9, 8,0 -> 0,8
-- ...
parseSegments :: [String] -> [Segment]
parseSegments = map parseSegment

-- | Parse segment coordinates from a string
-- >>> parseSegment "0,9 -> 5,9"
-- ((0,9),(5,9))
-- >>> parseSegment "0,9 -> 5,9, 8,0 -> 0,8"
-- *** Exception: malformed line 0,9 -> 5,9, 8,0 -> 0,8
-- ...
parseSegment :: String -> Segment
parseSegment s = let (ss:es:cruft) = splitStringAt (=='-') $ filter (not. flip elem ['>', ' ']) s
                     sc = parseCoords ss
                     ec = parseCoords es
                 in case cruft of
                      [] -> (sc, ec)
                      otherwise -> error ("malformed line " ++ s)

-- | Parse point coordinates from a string
-- >>> parseCoords "0,9"
-- (0,9)
-- >>> parseCoords "5,9"
-- (5,9)
parseCoords :: String -> Coords
parseCoords s = let (x:y:[]) = map toInt $ splitStringAt (==',') s
                in (x, y)

-- | Determine if two segments intersect
-- >>> segmentIntersect ((0,0),(0,1)) ((5,0),(5,1))
-- False
-- >>> segmentIntersect ((5,3),(5,6)) ((3,5),(6,5))
-- True
-- >>> segmentIntersect ((0,0),(5,5)) ((0,5),(5,0))
-- True
segmentIntersect :: Segment -> Segment -> Bool
segmentIntersect ((x11, y11),(x12,y12)) ((x21, y21),(x22,y22)) = False
-- https://algotree.org/algorithms/computational_geometry/line_segment_intersection/
-- segmentIntersect ((x11, y11),(x12,y12)) ((x21, y21),(x22,y22)) = let or11 = orientation ((x11, y11),(x12,y12)) ((x12,y12),(x21, y21))
--                                                                      or12 = orientation ((x11, y11),(x12,y12)) ((x12,y12),(x22, y22))
--                                                                      or21 = orientation ((x11, y11),(x12,y12)) ((x12,y12),(x21, y21))
--                                                                      or22 = orientation ((x11, y11),(x12,y12)) ((x12,y12),(x22, y22))
--                                                                 in or11 /= or12
--                                                                 && or21 /= or22

orientation s1 s2 = let alpha = slope s1
                        beta = slope s2
                     in alpha - beta

slope :: Segment -> Int
slope ((x1, y1),(x2,y2)) = div (y2-y1) (x2-x1)
