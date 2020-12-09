module AoCUtils
(
toTuple2
, lines2
, break2
, wordsWhen
, allHold
, eitherHolds
) where

-- | 'toTuple2' makes a tuple out of a two-element list
--
-- >>> toTuple2 [1, 2]
-- (1,2)
toTuple2 :: [a] -> (a, a)
toTuple2 (x1:x2:[]) = (x1, x2)

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

-- From https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- | Test that all predicates hold on the given value
--
-- >>> allHold [(>1), (>2)] 3
-- True
--
-- >>> allHold [(>1), (>2)] 1
-- False
--
-- >>> allHold [(>2), (>1)] 1
-- False
allHold :: [a -> Bool] -> a -> Bool
allHold ps x = foldl (&&) True $ [ p x | p <- ps ]

-- | Test that any predicates hold on the given value
--
-- >>> eitherHolds [(>1), (>2)] 3
-- True
--
-- >>> eitherHolds [(>1), (>2)] 2
-- True
--
-- >>> eitherHolds [(>2), (>1)] 1
-- False
eitherHolds :: [a -> Bool] -> a -> Bool
eitherHolds ps x = foldl (||) False $ [ p x | p <- ps ]
