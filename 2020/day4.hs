import System.IO
import AoCUtils

type PassportField = String

main = do
  input <- getContents
  let passports = map (makePassport) $ lines2 input
  print $ length $ filter (isValidPassport) passports
  print $ length $ filter (isValidPassportData) passports

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
isValidPassportField pf = case splitPasswordField pf of ("byr", _) -> True
                                                        ("iyr", _) -> True
                                                        ("eyr", _) -> True
                                                        ("hgt", _) -> True
                                                        ("hcl", _) -> True
                                                        ("ecl", _) -> True
                                                        ("pid", _) -> True
                                                        -- ("cid:", ) -> True
                                                        otherwise -> False

-- | 'isValidPassportData' validate that the data in passport fields are correct
--
-- >>> isValidPassportData $ makePassport "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
-- False
-- >>> isValidPassportData $ makePassport "iyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946"
-- False
-- >>> isValidPassportData $ makePassport "hcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
-- False
-- >>> isValidPassportData $ makePassport "hgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007"
-- False
--
-- >>> isValidPassportData $ makePassport "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f"
-- True
-- >>> isValidPassportData $ makePassport "eyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
-- True
-- >>> isValidPassportData $ makePassport "hcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022"
-- True
-- >>> isValidPassportData $ makePassport "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
-- True
isValidPassportData :: [PassportField] -> Bool
isValidPassportData p = isValidPassport p && (length (filter (isValidPassportFieldValue) p)) == 7

-- | 'isValidPassportFieldValue' verifies that a passport field value is correct
-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
--     If cm, the number must be at least 150 and at most 193.
--     If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.
--
-- >>> isValidPassportFieldValue "byr:2002"
-- True
-- >>> isValidPassportFieldValue "byr:2003"
-- False
-- >>> isValidPassportFieldValue "byr:1919"
-- False
--
-- >>> isValidPassportFieldValue "hgt:60in"
-- True
-- >>> isValidPassportFieldValue "hgt:190cm"
-- True
-- >>> isValidPassportFieldValue "hgt:190in"
-- False
-- >>> isValidPassportFieldValue "hgt:190"
-- False
--
-- >>> isValidPassportFieldValue "hcl:#123abc"
-- True
-- >>> isValidPassportFieldValue "hcl:#123abz"
-- False
-- >>> isValidPassportFieldValue "hcl:123abc"
-- False
-- >>> isValidPassportFieldValue "hcl:dab227"
-- False
--
-- >>> isValidPassportFieldValue "ecl:brn"
-- True
-- >>> isValidPassportFieldValue "ecl:wat"
-- False
--
-- >>> isValidPassportFieldValue "pid:000000001"
-- True
-- >>> isValidPassportFieldValue "pid:0123456789"
-- False
-- >>> isValidPassportFieldValue "pid:00000000s"
-- False
isValidPassportFieldValue :: PassportField -> Bool
isValidPassportFieldValue pf = case (splitPasswordField pf) of ("byr", byr) -> ((length byr == 4)
                                                                             && allHold [(>=1920), (<=2002)] (read byr :: Int))
                                                               ("iyr", iyr) -> ((length iyr == 4)
                                                                              && allHold [(>=2010), (<=2020)] (read iyr :: Int))
                                                               ("eyr", eyr) -> ((length eyr == 4)
                                                                              && allHold [(>=2020), (<=2030)] (read eyr :: Int))
                                                               ("hgt", hgt) -> (case (break (allHold [(>='a'), (<='z')]) hgt) of (cm, "cm") -> allHold [(>=150), (<=193)] (read cm :: Int)
                                                                                                                                 (inches, "in") -> allHold [(>=59), (<=76)] (read inches :: Int)
                                                                                                                                 otherwise -> False
                                                                               )
                                                               ("hcl", (hash:hcl)) -> ((length hcl == 6)
                                                                                    && (hash == '#')
                                                                                    && (foldl (&&) True (
                                                                                        map (eitherHolds [(allHold [(>='a'), (<='f')])
                                                                                                         ,(allHold [(>='0'), (<='9')])
                                                                                                         ]) hcl
                                                                                                        )
                                                                                       )
                                                                                     )
                                                               ("ecl", ecl) -> eitherHolds [(== "amb")
                                                                                           ,(== "blu")
                                                                                           ,(== "brn")
                                                                                           ,(== "gry")
                                                                                           ,(== "grn")
                                                                                           ,(== "hzl")
                                                                                           ,(== "oth")
                                                                                           ] ecl
                                                               ("pid", pid) -> ((length pid == 9)
                                                                                    && (foldl (&&) True (
                                                                                        map ((allHold [(>='0'), (<='9')])) pid
                                                                                                        )
                                                                                       )
                                                                                     )
                                                               ("cid", _) -> False -- we just never want to count it
                                                               otherwise -> False

splitPasswordField :: PassportField-> (String, String)
splitPasswordField = toTuple2 . wordsWhen (==':')
