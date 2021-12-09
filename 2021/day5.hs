import System.IO
import AoCUtils

type Coords = (Int, Int)
type Segment = (Coords, Coords)

main = do line <- getContents
          let segments = parseSegments $ lines line
          print $ day51 $ filter horizontalOrVertical segments

-- | Count the number of intersections
-- >>> day51 [((0,9),(5,9)),((9,4),(3,4)),((2,2),(2,1)),((7,0),(7,4)),((0,9),(2,9)),((3,4),(1,4))]
-- 5
-- day51:: [Segment] -> Int
-- day51 = countIntersections
day51 l = [ [s1,s2] | s1 <- l, s2 <- l, segmentIntersect s1 s2]

countIntersections :: [Segment] -> Int
countIntersections (xs:[]) = 0
countIntersections (x:xs) = (length $ filter (segmentIntersect x) xs) + countIntersections xs

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

horizontalOrVertical :: Segment -> Bool
horizontalOrVertical s = any (==True) [horizontal s, vertical s]

horizontal :: Segment -> Bool
horizontal ((x1,y1),(x2,y2)) = x1==x2

vertical :: Segment -> Bool
vertical ((x1,y1),(x2,y2)) = y1==y2

-- | Determine if two segments intersect
-- https://algotree.org/algorithms/computational_geometry/line_segment_intersection/
-- >>> segmentIntersect ((0,0),(0,1)) ((5,0),(5,1))
-- False
-- >>> segmentIntersect ((5,3),(5,6)) ((3,5),(6,5))
-- True
-- >>> segmentIntersect ((0,3),(5,3)) ((3,0),(3,5))
-- True
-- >>> segmentIntersect ((0,3),(5,3)) ((3,0),(3,5))
-- True
-- >>> segmentIntersect ((9,4),(3,4)) ((3,4),(1,4))
-- True
-- >>> segmentIntersect ((0,9),(2,9)) ((0,9),(5,9))
-- True
-- >>> segmentIntersect ((0,9),(2,9)) ((2,2),(2,1))
-- False
-- >>> segmentIntersect ((0,9),(5,9)) ((2,2),(2,1))
-- False
-- >>> segmentIntersect ((7,0),(7,4)) ((9,4),(3,4))
-- True
-- >>> segmentIntersect ((7,0),(7,4)) ((3,4),(1,4))
-- False
--
-- ((2,2),(2,1)),
-- ((9,4),(3,4)),
-- ((3,4),(1,4))]
-- ((7,0),(7,4)),
-- ((0,9),(2,9)),
-- ((0,9),(5,9)),
segmentIntersect :: Segment -> Segment -> Bool
segmentIntersect ((x11, y11),(x12,y12)) ((x21, y21),(x22,y22)) = let p1 = (x11, y11)
                                                                     q1 = (x12,y12)
                                                                     p2 = (x12,y12)
                                                                     q2 =(x22, y22)
                                                                     or11 = orientation p1 q1 p2
                                                                     or12 = orientation p1 q1 q2
                                                                     or21 = orientation p2 q2 p1
                                                                     or22 = orientation p2 q2 q1
                                                        in or11 /= or12 && or21 /= or22

orientation p1 p2 p3 = let alpha = slope (p1,p2)
                           beta = slope (p2,p3)
                        in alpha - beta

slope :: Segment -> Int
slope ((x1, y1),(x2,y2))
  | x1 == x2 = 0 -- dodgy
  | otherwise = div (y2-y1) (x2-x1)
