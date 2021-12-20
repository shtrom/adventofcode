import System.IO
import AoCUtils

type Template = String
type Element = Char
type Pair = String
type Rule = (String, String)

main = do input <- getContents
          let (template, rules) = parseInput input
              pairs = sortAggregates . aggregate  $ splitTemplate template
              firstElem = head template
              lastElem = last template
          -- print $ day141' rules template
          -- print $ day142 rules template
          print $ day14 10 rules pairs firstElem lastElem
          print $ day14 40 rules pairs firstElem lastElem

-- | Parse input
-- >>> parseInput "NNCB\n\nCH -> B\nHH -> N\n"
-- ("NNCB",[("CH","B"),("HH","N")])
parseInput :: String -> (String, [Rule])
parseInput s = let [t, r] = lines2 s
                   rr = [(pair, insert) | (pair:insert:_) <-  map (splitStringAt (=='-') . filter (not . flip elem ['>', ' '])) $ lines r]
                in (t,rr)

-- | Split all input into subsequent pairs
-- >>> splitTemplate "NNCB"
-- ["CB","NC","NN"]
splitTemplate :: String -> [Pair]
splitTemplate [_] = []
splitTemplate (x:x':xs) = sort $ [[x,x']] ++ splitTemplate (x':xs)

-- | Iterate template on aggregate pairs the requested number of times
-- >>> day14 10 [("CH","B"),("HH","N"),("CB","H"),("NH","C"),("HB","C"),("HC","B"),("HN","C"),("NN","C"),("BH","H"),("NC","B"),("NB","B"),("BN","B"),("BB","N"),("BC","B"),("CC","N"),("CN","C")] [(1,"NN"),(1,"NC"),(1,"CB")] 'N' 'B'
-- 1588
-- >>> day14 40 [("CH","B"),("HH","N"),("CB","H"),("NH","C"),("HB","C"),("HC","B"),("HN","C"),("NN","C"),("BH","H"),("NC","B"),("NB","B"),("BN","B"),("BB","N"),("BC","B"),("CC","N"),("CN","C")] [(1,"NN"),(1,"NC"),(1,"CB")] 'N' 'B'
-- 2188189693529
day14 :: Int -> [Rule] -> [Aggregate Pair] -> Element -> Element -> Int
day14 n rr p f l = let agg = splitPairs $ repeatProcessAggregates n rr p
                       doubleAgg = sort $ mergeAggregates agg $ sortAggregates $ aggregate [f,l] -- each single letter is in two pair, except for the first and last of the template
                    in div ((fst $ last doubleAgg) - (fst $ head doubleAgg)) 2
                    -- in agg
                    -- in doubleAgg
                    -- in map (\ (c,v) -> (div c 2,v)) doubleAgg

-- | Slit an aggregate of pairs into an aggregate of constituent element
-- >>> splitPairs [(2,"BC"),(1,"CN"),(2,"NB"),(1,"NN")]
-- [(4,'B'),(3,'C'),(5,'N')]
splitPairs :: [Aggregate Pair] -> [Aggregate Element]
splitPairs [] = []
splitPairs ((c,v):ps) = mergeAggregates (let f = head v
                                             l = last v
                                          in case f==l of
                                               True -> [(2*c,f)]
                                               False -> sortAggregates  [(c,head v),(c,last v)])
                                               (sortAggregates $ splitPairs ps)

-- | Repeatedly apply rules on aggregates
-- >>> repeatProcessAggregates 1 [("NN","C"),("NC","B")] [(1,"NN"),(1,"NC")]
-- [(1,"BC"),(1,"CN"),(1,"NB"),(1,"NC")]
-- >>> repeatProcessAggregates 1 [("NN","C"),("NC","B")] [(1,"BC"),(1,"CN"),(1,"NB"),(1,"NC")]
-- [(2,"BC"),(1,"CN"),(2,"NB")]
-- >>> repeatProcessAggregates 2 [("NN","C"),("NC","B")] [(1,"NN"),(1,"NC")]
-- [(2,"BC"),(1,"CN"),(2,"NB")]
--
-- | Sample; every letter is doubled as present in two pairs, except for the
-- first and last of the template, so 161*2 = 322, 1749*2 = 3498, but B is a last letter of the template
-- >>> sort $ splitPairs $ repeatProcessAggregates 10 [("CH","B"),("HH","N"),("CB","H"),("NH","C"),("HB","C"),("HC","B"),("HN","C"),("NN","C"),("BH","H"),("NC","B"),("NB","B"),("BN","B"),("BB","N"),("BC","B"),("CC","N"),("CN","C")] [(1,"NN"),(1,"NC"),(1,"CB")]
-- [(322,'H'),...,(3497,'B')]
repeatProcessAggregates :: Int -> [Rule] -> [Aggregate Pair] -> [Aggregate Pair]
repeatProcessAggregates 0 _ p = p
repeatProcessAggregates n rr p = repeatProcessAggregates (n-1) rr $ processAggregates rr p

-- | Process one full update
-- >>> processAggregates [("NN","C"),("NC","B")] [(2,"NN"),(3,"NC")]
-- [(3,"BC"),(2,"CN"),(3,"NB"),(2,"NC")]
-- >>> processAggregates [("NN","C"),("NC","B")] [(1,"BC"),(1,"CN"),(1,"NB"),(1,"NC")]
-- [(2,"BC"),(1,"CN"),(2,"NB")]
--
-- >>> processAggregates [("CH","B"),("HH","N"),("CB","H"),("NH","C"),("HB","C"),("HC","B"),("HN","C"),("NN","C"),("BH","H"),("NC","B"),("NB","B"),("BN","B"),("BB","N"),("BC","B"),("CC","N"),("CN","C")] [(1,"NN"),(1,"NC"),(1,"CB")]
-- [(1,"BC"),(1,"CH"),(1,"CN"),(1,"HB"),(1,"NB"),(1,"NC")]
-- >>> processAggregates [("CH","B"),("HH","N"),("CB","H"),("NH","C"),("HB","C"),("HC","B"),("HN","C"),("NN","C"),("BH","H"),("NC","B"),("NB","B"),("BN","B"),("BB","N"),("BC","B"),("CC","N"),("CN","C")] [(1,"BC"),(1,"CH"),(1,"CN"),(1,"HB"),(1,"NB"),(1,"NC")]
-- [(2,"BB"),(2,"BC"),(1,"BH"),(2,"CB"),(1,"CC"),(1,"CN"),(1,"HC"),(2,"NB")]
-- >>> processAggregates [("CH","B"),("HH","N"),("CB","H"),("NH","C"),("HB","C"),("HC","B"),("HN","C"),("NN","C"),("BH","H"),("NC","B"),("NB","B"),("BN","B"),("BB","N"),("BC","B"),("CC","N"),("CN","C")] [(2,"BB"),(2,"BC"),(1,"BH"),(2,"CB"),(1,"CC"),(1,"CN"),(1,"HC"),(2,"NB")]
-- [(4,"BB"),(3,"BC"),(1,"BH"),(2,"BN"),(1,"CC"),(2,"CH"),(2,"CN"),(3,"HB"),(1,"HH"),(4,"NB"),(1,"NC")]
-- >>> processAggregates [("CH","B"),("HH","N"),("CB","H"),("NH","C"),("HB","C"),("HC","B"),("HN","C"),("NN","C"),("BH","H"),("NC","B"),("NB","B"),("BN","B"),("BB","N"),("BC","B"),("CC","N"),("CN","C")] [(4,"BB"),(3,"BC"),(1,"BH"),(2,"BN"),(1,"CC"),(2,"CH"),(2,"CN"),(3,"HB"),(1,"HH"),(4,"NB"),(1,"NC")]
-- [(9,"BB"),(4,"BC"),(3,"BH"),(6,"BN"),(5,"CB"),(2,"CC"),(3,"CN"),(3,"HC"),(1,"HH"),(1,"HN"),(9,"NB"),(1,"NC"),(1,"NH")]
processAggregates :: [Rule] -> [Aggregate Pair] -> [Aggregate Pair]
processAggregates _ [] = []
processAggregates rr (p:ps) = let (c,v) = p in
                                  mergeAggregates
                                        (sortAggregates . map (multAggregate c) $ aggregate $ splitTemplate $ applyRules rr v)
                                        (sortAggregates $ processAggregates rr ps)
                                  -- (sortAggregates . map (multAggregate c) $ aggregate $ splitTemplate $ applyRules rr v)
                                  -- ++ (sortAggregates $ processAggregates rr ps)

-- |> Iterate template 10 times, and subtract the quantity of least and most
-- common elements
-- >>> day141 [("CH","B"),("HH","N"),("CB","H"),("NH","C"),("HB","C"),("HC","B"),("HN","C"),("NN","C"),("BH","H"),("NC","B"),("NB","B"),("BN","B"),("BB","N"),("BC","B"),("CC","N"),("CN","C")] "NNCB"
-- 1588
day141 :: [Rule] -> Template -> Int
day141 r t = let p = repeatProcess 10 r t
                 agg = sort $ aggregate $ sort p
              in (fst $ last agg) - (fst $ head agg)

-- |> Iterate template 40 times, and subtract the quantity of least and most
-- common elements
-- [skipped] day141 [("CH","B"),("HH","N"),("CB","H"),("NH","C"),("HB","C"),("HC","B"),("HN","C"),("NN","C"),("BH","H"),("NC","B"),("NB","B"),("BN","B"),("BB","N"),("BC","B"),("CC","N"),("CN","C")] "NNCB"
-- 2188189693529
day142 :: [Rule] -> String -> Int
day142 r t = let p = repeatProcess 40 r t
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
-- >>> applyRules [("NN","C"),("NC","B")] "CN"
-- "CN"
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
