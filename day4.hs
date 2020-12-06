import System.IO

main = do
  input <- getContents
  print input

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
