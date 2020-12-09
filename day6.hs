import System.IO
import Data.List
--  import Data.List.Utils -- cabal install MissingH
import AoCUtils

main = do
  input <- getContents
  let responses = lines2 input
  print $ foldl1 (+) [ length $ uniq (sort (filter (/= '\n') r)) | r <- responses ]
