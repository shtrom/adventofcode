import System.IO

type PassportField = String

main = do
  input <- getContents
  let passports = map (makePassport) $ lines2 input
  print $ length $ filter (isValidPassport) passports

-- | 'makePassport' take a string description of a passport, and returns a Passport
--
-- >>> makePassport "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"
-- ["ecl:gry","pid:860033327","eyr:2020","hcl:#fffffd","byr:1937","iyr:2017","cid:147","hgt:183cm"]
--
-- >>> makePassport "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929"
-- ["iyr:2013","ecl:amb","cid:350","eyr:2023","pid:028048884","hcl:#cfa07d","byr:1929"]
--
-- >>> makePassport "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm"
-- ["hcl:#ae17e1","iyr:2013","eyr:2024","ecl:brn","pid:760753108","byr:1931","hgt:179cm"]
--
-- >>> makePassport "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"
-- ["hcl:#cfa07d","eyr:2025","pid:166559648","iyr:2011","ecl:brn","hgt:59in"]
--
makePassport :: String -> [PassportField]
makePassport s = words $ unlines $ lines s

-- | 'isValidPassport' verifies that a passport is valid, in that it contains
-- only (?)
-- - byr (Birth Year)
-- - iyr (Issue Year)
-- - eyr (Expiration Year)
-- - hgt (Height)
-- - hcl (Hair Color)
-- - ecl (Eye Color)
-- - pid (Passport ID)
-- - cid (Country ID)
-- with cid being optional
--
-- >>> isValidPassport ["ecl:gry","pid:860033327","eyr:2020","hcl:#fffffd","byr:1937","iyr:2017","cid:147","hgt:183cm"]
-- True
--
-- >>> isValidPassport ["iyr:2013","ecl:amb","cid:350","eyr:2023","pid:028048884","hcl:#cfa07d","byr:1929"]
-- False
--
-- Treat missing cid as valid
-- >>> isValidPassport ["hcl:#ae17e1","iyr:2013","eyr:2024","ecl:brn","pid:760753108","byr:1931","hgt:179cm"]
-- True
--
-- >>> isValidPassport ["hcl:#cfa07d","eyr:2025","pid:166559648","iyr:2011","ecl:brn","hgt:59in"]
-- False
--
isValidPassport :: [PassportField] -> Bool
isValidPassport p = (length (filter (isValidPassportField) p)) == 7

-- | 'isValidPassportField' verifies that a passport field is valid, in that it starts with
-- either
-- - byr (Birth Year)
-- - iyr (Issue Year)
-- - eyr (Expiration Year)
-- - hgt (Height)
-- - hcl (Hair Color)
-- - ecl (Eye Color)
-- - pid (Passport ID)
-- with cid being optional
isValidPassportField :: PassportField -> Bool
isValidPassportField pf = case take 4 pf of "byr:" -> True
                                            "iyr:" -> True
                                            "eyr:" -> True
                                            "hgt:" -> True
                                            "hcl:" -> True
                                            "ecl:" -> True
                                            "pid:" -> True
                                            -- "cid:" -> True
                                            otherwise -> False


-- | 'lines2' breaks a string up into a list of strings at newline
-- characters.  The resulting strings do not contain newlines2.
--
-- Note that after splitting the string at newline characters, the
-- last part of the string is considered a line even if it doesn't end
-- with a newline. For example,
--
-- >>> lines2 ""
-- []
--
-- >>> lines2 "\n"
-- ["\n"]
--
-- >>> lines2 "one"
-- ["one"]
--
-- >>> lines2 "one\n"
-- ["one\n"]
--
-- >>> lines2 "one\n\n"
-- ["one"]
--
-- >>> lines2 "one\ntwo"
-- ["one\ntwo"]
--
-- >>> lines2 "one\ntwo\n"
-- ["one\ntwo\n"]
--
-- >>> lines2 "one\n\ntwo"
-- ["one","two"]
--
-- >>> lines2 "one\n\ntwo\n"
-- ["one","two\n"]
--
-- >>> lines2 "one\n\ntwo\n\n"
-- ["one","two"]
--
-- Thus @'lines2' s@ contains at least as many elements as newlines in @s@.
lines2                   :: String -> [String]
lines2 ""                =  []
-- Somehow GHC doesn't detect the selector thunks in the below code,
-- so s' keeps a reference to the first line via the pair and we have
-- a space leak (cf. #4334).
-- So we need to make GHC see the selector thunks with a trick.
lines2 s                 =  cons (case break2 (== '\n') s of
                                    (l, s') -> (l, case s' of
                                                    []      -> []
                                                    _:_:s''   -> lines2 s''))
  where
    cons ~(h, t)        =  h : t

-- | Break a list with at two consecutive matches
--
-- >>> break2 (< 9) []
-- ([],[])
-- >>> break2 (< 9) [1]
-- ([1],[])
-- >>> break2 (< 9) [1,1]
-- ([],[1,1])
-- >>> break2 (> 9) [1,1]
-- ([1,1],[])
-- >>> break2 (< 9) [1,2,3]
-- ([],[1,2,3])
-- >>> break2 (> 9) [1,2,3]
-- ([1,2,3],[])
-- >>> break2 (> 3) [1,2,3,4,1,2,3,4]
-- ([1,2,3,4,1,2,3,4],[])
-- >>> break2 (> 3) [1,2,3,4,4,1,2,3,4]
-- ([1,2,3],[4,4,1,2,3,4])
--
break2 :: (a -> Bool) -> [a] -> ([a], [a])
break2 _ xs@[]           = (xs, xs)
break2 p xs@(x:[]) = (xs, [])
break2 p xs@(x1:x2:[])
  | p x1 && p x2 =  ([],xs)
  | otherwise = (xs, [])
break2 p xs@(x1:x2:xs')
  | p x1 && p x2 =  ([],xs)
  | not (p x1) && p x2 =  let (ys,zs) = break2 p (x2:xs') in (x1:ys,zs)
  | otherwise    =  let (ys,zs) = break2 p (xs') in (x1:x2:ys,zs)
