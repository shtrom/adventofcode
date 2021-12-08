import System.IO
import AoCUtils

type Segment = Char
type Digit = [Segment]
type Digits = [Digit]
type Pattern = Digit
type Patterns = [Pattern]
type DigitMap = (Int, [Segment])

main = do
        input <- getContents
        let notes = splitInput input
        print $ day81 $ foldl (++) [] $ map snd notes
        print $ day82 notes

-- | Split the data input
-- >>> splitInput "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nf"
-- [(["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"],["fdgacbe","cefdb","cefbgd","gcbe"]),(["edbfga","begcd","cbg","gc","gcadebf","fbgde","acbgfd","abcde","gfcbed","gfec"],["fcgedb","cgb","dgebacf","gc"])]
splitInput :: String -> [(Patterns, Digits)]
splitInput l = [
                        (patterns, digits) | (patterns:digits:[]) <- map (map words . (splitStringAt (=='|'))) $ lines l
               ]

day81 :: Digits -> Int
day81 digits = length $ filter simpleDigit digits

day82 :: [(Patterns, Digits)] -> Int
day82 l = sum $ map readDigits l

-- | Read Pattern and Digits, and determine value
-- >>> readDigits (["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"],["fdgacbe","cefdb","cefbgd","gcbe"])
-- 8394
readDigits :: (Patterns, Digits) -> Int
readDigits (ps, d) = readDigits' ps (reverse d)

readDigits' :: Patterns -> Digits -> Int
readDigits' _ [] = 0
readDigits' ps (x:xs) = digitValue ps x + 10 * readDigits' ps xs

-- | Determine the digit value
-- >>> digitValue ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"] "edb"
-- 7
-- >>> ps = ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"]
-- >>> map (digitValue ps) ps
-- [1,8,9,6,4,5,0,3,2,7]
digitValue :: Patterns -> Digit -> Int
digitValue ps dg = digitValueFromMap (makeSegmentMap ps) dg

digitValueFromMap :: [Segment] -> Digit -> Int
digitValueFromMap (a:b:c:d:e:f:g:[]) dg
        | all (flip elem dg) (a:b:c:d:e:f:g:[]) = 8
        | all (flip elem dg) (a:b:d:e:f:g:[]) = 6
        | all (flip elem dg) (a:b:c:d:f:g:[]) = 9
        | all (flip elem dg) (a:b:c:e:f:g:[]) = 0
        | all (flip elem dg) (a:c:d:e:g:[]) = 2
        | all (flip elem dg) (a:c:d:f:g:[]) = 3
        | all (flip elem dg) (a:b:d:f:g:[]) = 5
        | all (flip elem dg) (b:c:d:f:[]) = 4
        | all (flip elem dg) (a:c:f:[]) = 7
        | all (flip elem dg) (c:f:[]) = 1
        | otherwise = error ("gobbledygook " ++ dg)

-- | Determine the segment map
-- >>> makeSegmentMap ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"]
-- "dgbcaef"
makeSegmentMap :: Patterns -> [Segment]
makeSegmentMap ps = [
                    guessA ps
                    ,guessB ps
                    ,guessC ps
                    ,guessD ps
                    ,guessE ps
                    ,guessF ps
                    ,guessG ps
                    ]

-- | Guess the A segment based on the recorded patterns
-- Of the simple digits, A is only used in 7, which is the only difference from
-- 1
-- >>> guessA ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"]
-- 'd'
guessA :: Patterns -> Segment
guessA ps = let simples = filter simpleDigit ps
                simplesMap = makeDigitMap simples
                allAs@(theA:xs) = [ a
                  | a <- (getMapAt 7 simplesMap), not $ elem a (getMapAt 1 simplesMap)
                               ]
                in theA

-- | Guess the B segment based on the recorded patterns
-- B is the only segment appearing 6 times
-- >>> guessB ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"]
-- 'g'
guessB :: Patterns -> Segment
guessB ps = getSegmentByOccurence 6 ps

-- | Guess the C segment based on the recorded patterns
-- B is the only segment appearing 8 times, that is not A
-- >>> guessC ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"]
-- 'b'
guessC :: Patterns -> Segment
guessC ps = getSegmentByOccurence 8 $ map (filter (/= (guessA ps))) ps

-- | Guess the D segment based on the recorded patterns
-- Of the simple digits, D is only used in 4, and is neither B, C or F
-- >>> guessD ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"]
-- 'c'
guessD :: Patterns -> Segment
guessD ps = let simples = filter simpleDigit ps
                simplesMap = makeDigitMap simples
                allDs@(theD:xs) = [ d
                  | d <- (getMapAt 4 simplesMap), not $ elem d [guessB ps, guessC ps, guessF ps]
                               ]
                in theD

-- | Guess the E segment based on the recorded patterns
-- E is the only segment appearing 4 times
-- >>> guessE ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"]
-- 'a'
guessE :: Patterns -> Segment
guessE ps = getSegmentByOccurence 4 ps

-- | Guess the F segment based on the recorded patterns
-- F is the only segment appearing 9 times
-- >>> guessF ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"]
-- 'e'
guessF :: Patterns -> Segment
guessF ps = getSegmentByOccurence 9 ps

-- | Guess the G segment based on the recorded patterns
-- G is the only segment appearing 7 times, that is not D
-- >>> guessG ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"]
-- 'f'
guessG :: Patterns -> Segment
guessG ps = getSegmentByOccurence 7 $ map (filter (/= (guessD ps))) ps

-- | Get an arbitrary segment by occurence
-- XXX: only works correctly for segments that have a unique occurence count
-- >>> getSegmentByOccurence 6 ["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"]
-- 'g'
getSegmentByOccurence :: Int -> Patterns -> Segment
getSegmentByOccurence n ps = getMapAt n $ aggregate $ sort $ foldl (++) [] ps

-- | Guess digits based on length
-- >>> simpleDigit "aa"
-- True
-- >>> simpleDigit "aaaa"
-- True
-- >>> simpleDigit "aaa"
-- True
-- >>> simpleDigit "aaaaaaa"
-- True
-- >>> simpleDigit "aaaaaaaa"
-- False
simpleDigit :: Pattern -> Bool
simpleDigit s = any (==length(s)) [2,3,4,7]

makeDigitMap :: [Digit] -> [DigitMap]
makeDigitMap ds = zip (map guessDigit ds) (map sort ds)

-- | Guess digits based on length
-- >>> guessDigit "aa"
-- 1
-- >>> guessDigit "aaaa"
-- 4
-- >>> guessDigit "aaa"
-- 7
-- >>> guessDigit "aaaaaaa"
-- 8
guessDigit :: Digit -> Int
guessDigit s
  | length(s) == 2 = 1
  | length(s) == 4 = 4
  | length(s) == 3 = 7
  | length(s) == 7 = 8
  | otherwise = error $ "cannot guess digit " ++ s

-- | Find the digit associated to a number
-- >>> getMapAt 7 [(1,"be"),(8,"abcdefg"),(4,"bceg"),(7,"bde")]
-- "bde"
getMapAt :: Int -> [(Int, a)] -> a
getMapAt _ [] = error "unmapped value"
getMapAt n (x:xs)
  | (fst x) == n = snd x
  | otherwise = getMapAt n xs
