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
