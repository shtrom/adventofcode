import System.IO
import AoCUtils

type Rule = (String, String)

main = do input <- getContents
          let (template, rules) = parseInput input
          print $ day141 rules template

-- | Parse input
-- >>> parseInput "NNCB\n\nCH -> B\nHH -> N\n"
-- ("NNCB",[("CH","B"),("HH","N")])
parseInput :: String -> (String, [Rule])
parseInput s = let [t, r] = lines2 s
                   rr = [(pair, insert) | (pair:insert:_) <-  map (splitStringAt (=='-') . filter (not . flip elem ['>', ' '])) $ lines r]
                in (t,rr)

-- |> Iterate template 10 times, and subtract the quantity of least and most
-- common elements
-- >>> day141 [("CH","B"),("HH","N"),("CB","H"),("NH","C"),("HB","C"),("HC","B"),("HN","C"),("NN","C"),("BH","H"),("NC","B"),("NB","B"),("BN","B"),("BB","N"),("BC","B"),("CC","N"),("CN","C")] "NNCB"
-- 1588
day141 :: [Rule] -> String -> Int
day141 r t = let p = repeatProcess 10 r t
                 agg = sort $ aggregate $ sort p
              in (fst $ last agg) - (fst $ head agg)

-- | Repeatedly apply rules
-- >>> repeatProcess 2 [("NN","C"),("NC","B")] "NNC"
-- "NBCNBC"
repeatProcess :: Int -> [Rule] -> String -> String
repeatProcess 0 _ t = t
repeatProcess n r t = repeatProcess (n-1) r $ processRules r t

-- | Process one full update
-- >>> processRules [("NN","C"),("NC","B")] "NNC"
-- "NCNBC"
processRules :: [Rule] -> String -> String
processRules _ [t] = [t]
processRules rr (t:t':ts) = (init $ applyRules rr [t,t'])
                            ++ (processRules rr (t':ts))

-- | Apply all rules to one pair
-- >>> applyRules [("NN","C"),("NC","B")] "NN"
-- "NCN"
-- >>> applyRules [("NN","C"),("NC","B")] "NC"
-- "NBC"
applyRules :: [Rule] -> String -> String
applyRules [] t = t
applyRules (r:rs) t = applyRules rs $ applyRule r t

-- | Apply one rule to one pair
-- >>> applyRule ("NN","C") "NN"
-- "NCN"
-- >>> applyRule ("NC","B") "NN"
-- "NN"
-- >>> applyRule ("NC","B") "NCN"
-- "NCN"
applyRule :: Rule -> String -> String
applyRule _ t@(a:b:c:_) = t
applyRule (pair, insert) t@[f,l] = case t == pair of
                               True -> [f] ++ insert ++ [l]  -- XXX: should return early
                               False -> t
