import System.IO
import AoCUtils

main = do
        input <- getContents
        let notes = splitInput input
        print $ day81 $ foldl (++) [] $ map snd notes

-- | Split the data input
-- >>> splitInput "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nf"
-- [(["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"],["fdgacbe","cefdb","cefbgd","gcbe"]),(["edbfga","begcd","cbg","gc","gcadebf","fbgde","acbgfd","abcde","gfcbed","gfec"],["fcgedb","cgb","dgebacf","gc"])]
splitInput :: String -> [([String], [String])]
splitInput l = [
                        (patterns, digits) | (patterns:digits:[]) <- map (map words . (splitStringAt (=='|'))) $ lines l 
               ]

day81 :: [String] -> Int
day81 digits = length $ filter simpleDigit digits

-- | Guess digits based on length
-- >>> guessDigit "aa"
-- 1
-- >>> guessDigit "aaaa"
-- 4
-- >>> guessDigit "aaa"
-- 7
-- >>> guessDigit "aaaaaaa"
-- 8
guessDigit :: String -> Int
guessDigit s
  | length(s) == 2 = 1
  | length(s) == 4 = 4
  | length(s) == 3 = 7
  | length(s) == 7 = 8

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
simpleDigit :: String -> Bool
simpleDigit s = any (==length(s)) [2,3,4,7]
