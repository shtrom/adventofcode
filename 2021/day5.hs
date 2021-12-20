import System.IO
import AoCUtils

type Coords = (Int, Int)
type Segment = (Coords, Coords)

main = do line <- getContents
          let segments = parseSegments $ lines line
          print $ day51 $ filter horizontalOrVertical segments
          print $ day51 segments

-- | Count the number of intersections
-- >>> day51 [((0,9),(5,9)),((3,4),(9,4)),((2,1),(2,1)),((7,0),(7,4)),((0,9),(2,9)),((1,4),(3,4))]
-- 5
day51:: [Segment] -> Int
day51 = length . (filter (\(c,v) -> c>1)) . aggregateOverlaps . sort . (map expandSegment)
-- day51 l = [ [s1,s2] | s1 <- l, s2 <- l, segmentIntersect s1 s2]

countIntersections :: [Segment] -> Int
countIntersections (xs:[]) = 0
countIntersections (x:xs) = (length $ filter (segmentIntersect x) xs) + countIntersections xs

-- | Aggregate overlapping bits between segments
-- XXX: The count of overlaps is incorrect as segments are always found to
-- overlap in pairs.
-- >>> aggregateOverlaps [[(0,1),(0,2)],[(0,2),(1,2)]]
-- [(1,(0,1)),(2,(0,2)),(1,(1,2))]
aggregateOverlaps :: [[Coords]] -> [Aggregate Coords]
aggregateOverlaps x = aggregate $ sort $ foldl (++) [] x

-- | Check overlapping bits of segments
-- >>> checkOverlaps ((0,0),(0,4)) ((0,2),(0,4))
-- [(2,(0,2)),(2,(0,3)),(2,(0,4))]
-- >>> checkOverlaps ((1,0),(1,4)) ((0,2),(0,4))
-- []
checkOverlaps :: Segment -> Segment -> [Aggregate Coords]
checkOverlaps s1 s2
        | segmentIntersect s1 s2 = let es1 = expandSegment s1
                                       es2 = expandSegment s2
                                       agg = aggregate $ sort $ es1 ++ es2
                                    in filter (\(c,p) -> c>1) agg
        | otherwise = []

-- | Measure overlapping bits of segments
-- >>> measureOverlaps ((0,0),(0,4)) ((0,2),(0,4))
-- 3
-- >>> measureOverlaps ((1,0),(1,4)) ((0,2),(0,4))
-- 0
measureOverlaps :: Segment -> Segment -> Int
measureOverlaps s1 s2 = length $ checkOverlaps s1 s2

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

-- | Expand a segment to all its points
-- >>> expandSegment ((0,0), (4,0))
-- [(0,0),(1,0),(2,0),(3,0),(4,0)]
-- >>> expandSegment ((0,0), (0,4))
-- [(0,0),(0,1),(0,2),(0,3),(0,4)]
-- >>> expandSegment ((0,4), (0,0))
-- [(0,4),(0,3),(0,2),(0,1),(0,0)]
-- >>> expandSegment ((0,0), (3,3))
-- [(0,0),(1,1),(2,2),(3,3)]
expandSegment :: Segment -> [Coords]
expandSegment s@((x,y),(x',y'))
  | x==x' && y==y' = [(x',y')]
  | otherwise = [(x,y)] ++ expandSegment ((x+signum (x'-x),y+signum (y'-y)),(x',y'))

horizontalOrVertical :: Segment -> Bool
horizontalOrVertical s = any (==True) [horizontal s, vertical s]

horizontal :: Segment -> Bool
horizontal ((x1,y1),(x2,y2)) = x1==x2

vertical :: Segment -> Bool
vertical ((x1,y1),(x2,y2)) = y1==y2

-- | Determine if two segments intersect
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
segmentIntersect :: Segment -> Segment -> Bool
--
-- ((2,2),(2,1)),
-- ((9,4),(3,4)),
-- ((3,4),(1,4))]
-- ((7,0),(7,4)),
-- ((0,9),(2,9)),
-- ((0,9),(5,9)),
-- https://algotree.org/algorithms/computational_geometry/line_segment_intersection/
-- segmentIntersect ((x11, y11),(x12,y12)) ((x21, y21),(x22,y22)) = let p1 = (x11, y11)
--                                                                      q1 = (x12,y12)
--                                                                      p2 = (x12,y12)
--                                                                      q2 =(x22, y22)
--                                                                      or11 = orientation p1 q1 p2
--                                                                      or12 = orientation p1 q1 q2
--                                                                      or21 = orientation p2 q2 p1
--                                                                      or22 = orientation p2 q2 q1
--                                                         in or11 /= or12 && or21 /= or22

-- orientation p1 p2 p3 = let alpha = slope (p1,p2)
--                            beta = slope (p2,p3)
--                         in alpha - beta

-- slope :: Segment -> Int
-- slope ((x1, y1),(x2,y2))
--   | x1 == x2 = 0 -- dodgy
--   | otherwise = div (y2-y1) (x2-x1)

-- https://martin-thoma.com/how-to-check-if-two-line-segments-intersect/
segmentIntersect s1 s2 =
        let s1' = sortSegment s1
            s2' = sortSegment s2
         in boundingBoxCollide s1' s2'
            && touchesOrCrossesLine s1' s2'
            && touchesOrCrossesLine s2' s1'

-- | Swap segment coordinates to have lowest X first
-- >>> sortSegment ((3,4),(9,4))
-- ((3,4),(9,4))
-- >>> sortSegment ((9,4),(3,4))
-- ((3,4),(9,4))
-- >>> sortSegment ((2,2),(2,1))
-- ((2,1),(2,2))
sortSegment :: Segment -> Segment
sortSegment ((xa1,ya1),(xa2,ya2))
  | xa2 < xa1 = ((xa2,ya2),(xa1,ya1))
  | ya2 < ya1 = ((xa2,ya2),(xa1,ya1))
  | otherwise =  ((xa1,ya1),(xa2,ya2))

-- | Check if the bounding boxes of two segment collide
-- >>> boundingBoxCollide ((2,4),(6,10)) ((3,3),(7,3))
-- False
-- >>> boundingBoxCollide ((2,4),(6,10)) ((3,1),(7,5))
-- True
-- >>> boundingBoxCollide ((3,4),(9,4)) ((1,4),(3,4))
-- True
boundingBoxCollide :: Segment -> Segment -> Bool
boundingBoxCollide ((xa1,ya1),(xa2,ya2)) ((xb1,yb1),(xb2,yb2)) =
        xa1 <= xb2
        && xa2 >= xb1
        && ya1 <= yb2
        && ya2 >= yb1

touchesOrCrossesLine :: Segment -> Segment -> Bool
touchesOrCrossesLine ((xa1,ya1),(xa2,ya2)) ((xb1,yb1),(xb2,yb2)) =
        pointOnLine ((xa1,ya1),(xa2,ya2)) (xb1,yb1)
        || pointOnLine ((xa1,ya1),(xa2,ya2)) (xb2,yb2)
        || (xor
                (pointRightOfLine ((xa1,ya1),(xa2,ya2)) (xb1,yb1))
                (pointRightOfLine ((xa1,ya1),(xa2,ya2)) (xb2,yb2))
           )

xor :: Bool -> Bool -> Bool
xor False False = False
xor False True = True
xor True False = True
xor True True = False

pointOnLine :: Segment -> Coords -> Bool
pointOnLine s p =
        0 == abs (translatedCrossProduct s p)

pointRightOfLine :: Segment -> Coords -> Bool
pointRightOfLine s p =
        0 > translatedCrossProduct s p

translatedCrossProduct :: Segment -> Coords -> Int
translatedCrossProduct ((x1,y1),(x2,y2)) (x,y) =
        let (x2',y2') = translateTo (x1,y1) (x2,y2)
            (x',y') = translateTo (x1,y1) (x,y)
         in crossProduct (x2',y2') (x',y')

translateTo :: Coords -> Coords -> Coords
translateTo (x1,y1) (x2,y2) = (x2-x1,y2-y1)

crossProduct :: Coords -> Coords -> Int
crossProduct (x1,y1) (x2,y2) = x1 * y2 + x2 * y1
