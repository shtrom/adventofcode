import System.IO

data BoardingPass = BoardingPass {row :: Int
                                 , column :: Int
                                 } deriving (Show)
type BinarySymbol = Char
type HighSymbol = BinarySymbol
type LowSymbol = BinarySymbol
data BinaryPair = BinaryPair LowSymbol HighSymbol deriving (Show)
type BinaryString = String

main = do
  input <- getContents
  let passes = lines input
      bps = [makeBoardingPass passString | passString <- passes]

  print bps


-- | `makeBoardingPass` creates a boarding pass recordset from a string
-- >>> makeBoardingPass "BFFFBBFRRR"
--BoardingPass {row = 70, column = 7}
-- >>> makeBoardingPass "FFFBBBFRRR"
--BoardingPass {row = 14, column = 7}
-- >>> makeBoardingPass "BBFFBBFRLL"
--BoardingPass {row = 102, column = 4}
--
makeBoardingPass :: BinaryString -> BoardingPass
makeBoardingPass bps = let (row, column) = splitAt 7 bps
                        in BoardingPass { row = toInt (BinaryPair 'F' 'B') row
                                        , column = toInt (BinaryPair 'L' 'R') column
                                        }

-- | `rowId` calculates the row ID from a Boarding Pass
-- >>> rowId $ BoardingPass {row = 70, column = 7}
-- 567
-- >>> rowId $ BoardingPass {row = 14, column = 7}
-- 119
-- >>> rowId $ BoardingPass {row = 102, column = 4}
-- 820
rowId :: BoardingPass -> Int
rowId bp = (row bp * 8) + (column bp)

-- | `toInt` converts a BinaryString encoding using a BinaryPair MSB first to an Int
-- >>> toInt (BinaryPair '0' '1') "0"
-- 0
-- >>> toInt (BinaryPair '0' '1') "1"
-- 1
-- >>> toInt (BinaryPair '0' '1') "10"
-- 2
toInt :: BinaryPair -> BinaryString -> Int
toInt bp s = toIntLSB bp $ reverse s

-- | `toIntLSB` converts a BinaryString encoding using a BinaryPair LSB first to an Int
-- >>> toIntLSB (BinaryPair '0' '1') "0"
-- 0
-- >>> toIntLSB (BinaryPair '0' '1') "1"
-- 1
-- >>> toIntLSB (BinaryPair '0' '1') "10"
-- 1
-- >>> toIntLSB (BinaryPair '0' '1') "01"
-- 2
toIntLSB :: BinaryPair -> BinaryString -> Int
toIntLSB _ [] = 0
toIntLSB bp (x:xs) = (toInt1 bp x) + 2 * (toIntLSB bp xs)

-- | `toInt1` converts one BinarySymbol encoded using BinaryPair to an Int
-- >>> toInt1 ((BinaryPair '0' '1')) '0'
-- 0
-- >>> toInt1 ((BinaryPair '0' '1')) '1'
-- 1
-- >>> toInt1 ((BinaryPair '0' '1')) 'b'
-- *** Exception: 'b' is not from BinaryPair '0' '1'
-- ...
toInt1 :: BinaryPair -> BinarySymbol -> Int
toInt1 bp@(BinaryPair l h) c = (if c == l then 0
                                          else if c == h then 1
                                          else error ((show c) ++ " is not from " ++ (show bp)))

-- | `splitAt'` splits a list at the nth element;
-- Also, there is a standard function with exactly that signature doing exactly
-- that...
-- >>> splitAt' 2 [1,2,3,4]
-- ([1,2],[3,4])
-- >>> splitAt' 0 [1,2,3,4]
-- ([],[1,2,3,4])
-- >>> splitAt' 10 [1,2,3,4]
-- ([1,2,3,4],[])
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] = ([], [])
splitAt' 0 xs = ([], xs)
splitAt' n (x:xs) = let (xx, xxs) = splitAt' (n-1) xs
                    in (x:xx, xxs)
