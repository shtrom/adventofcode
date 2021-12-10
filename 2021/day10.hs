import System.IO
import AoCUtils

main = do input<- getContents
          print $ day101 $ lines input
          print $ day102 $ lines input

-- | Find and score corrupted lines
-- >>> day101 ["[({(<(())[]>[[{[]{<()<>>","[(()[<>])]({[<{<<[]>>(","{([(<{}[<>[]}>{[]{[(<()>","(((({<>}<{<{<>}{[]{[]{}","[[<[([]))<([[{}[[()]]]","[{[{({}]{}}([{[{{{}}([]","{<[[]]>}<{[{[{[]{()[[[]","[<(<(<(<{}))><([]([]()","<{([([[(<>()){}]>(<<{{","<{([{{}}[<[[[<>{}]]]>[]]"]
-- 26397
day101 :: [String] -> Int
day101 l = sum $ map scoreCorrupted
        $ [x | (x:xs) <- map (checkChunks []) l, isClose x]

-- | Find and score completion for incomplete lines
-- >>> day102 ["[({(<(())[]>[[{[]{<()<>>","[(()[<>])]({[<{<<[]>>(","{([(<{}[<>[]}>{[]{[(<()>","(((({<>}<{<{<>}{[]{[]{}","[[<[([]))<([[{}[[()]]]","[{[{({}]{}}([{[{{{}}([]","{<[[]]>}<{[{[{[]{()[[[]","[<(<(<(<{}))><([]([]()","<{([([[(<>()){}]>(<<{{","<{([{{}}[<[[[<>{}]]]>[]]"]
-- 288957
--
-- day102 :: [String] -> Int
-- day102 _ = 0
day102 l = median $ sort
  $ [ scoreCompletion x
  | x <- map (checkChunks []) l
  , (not . isClose) $ head x]

-- | Check a line for corruption, and return the first corrupted character
-- >>> checkChunks [] "{([(<{}[<>[]}>{[]{[(<()>"
-- "}..."
-- >>> checkChunks [] "[[<[([]))<([[{}[[()]]]"
-- ")..."
-- >>> checkChunks [] "[{[{({}]{}}([{[{{{}}([]"
-- "]..."
-- >>> checkChunks [] "[<(<(<(<{}))><([]([]()"
-- ")..."
-- >>> checkChunks [] "<{([([[(<>()){}]>(<<{{"
-- ">..."
-- >>> checkChunks "(" ")"
-- ""
-- >>> checkChunks [] "()"
-- ""
-- >>> checkChunks [] "("
-- "("
-- >>> map closing $ checkChunks [] "("
-- ")"
-- >>> map closing $ checkChunks "[(" ""
-- "])"
-- >>> map closing $ checkChunks "(" "["
-- "])"
-- >>> map closing $ checkChunks [] "([{<"
-- ">}])"
-- >>> map closing $ checkChunks [] "[({(<(())[]>[[{[]{<()<>>"
-- "}}]])})]"
-- >>> map closing $ checkChunks [] "[(()[<>])]({[<{<<[]>>("
-- ")}>]})"
-- >>> map closing $ checkChunks [] "(((({<>}<{<{<>}{[]{[]{}"
-- "}}>}>))))"
-- >>> map closing $ checkChunks [] "{<[[]]>}<{[{[{[]{()[[[]"
-- "]]}}]}]}>"
-- >>> map closing $ checkChunks [] "<{([{{}}[<[[[<>{}]]]>[]]"
-- "])}>"
checkChunks :: String -> String -> String
checkChunks [] [] = []
checkChunks y [] = y
checkChunks [] (x:xs) = case isOpen x of
                        True -> checkChunks [x] xs
                        False -> [x]
checkChunks (y:ys) (x:xs) = case isOpen x of
                        True -> checkChunks (x:y:ys) xs
                        False -> case closeMatchesOpen y x of
                                True -> checkChunks ys xs
                                False -> (x:y:ys)


pairs = ["()", "[]", "{}", "<>"]

-- | Determine whether a character can open a chunk
-- >>> isOpen '('
-- True
-- >>> isOpen '['
-- True
-- >>> isOpen '{'
-- True
-- >>> isOpen '<'
-- True
-- >>> isOpen ')'
-- False
-- >>> isOpen ']'
-- False
-- >>> isOpen '}'
-- False
-- >>> isOpen '>'
-- False
-- >>> isOpen 't'
-- False
isOpen :: Char -> Bool
isOpen = flip elem $ map head pairs

-- | Determine whether a character can close a chunk
-- >>> isClose '('
-- False
-- >>> isClose '['
-- False
-- >>> isClose '{'
-- False
-- >>> isClose '<'
-- False
-- >>> isClose ')'
-- True
-- >>> isClose ']'
-- True
-- >>> isClose '}'
-- True
-- >>> isClose '>'
-- True
-- >>> isClose 't'
-- False
isClose :: Char -> Bool
isClose = flip elem $ map last pairs

-- | Determine whether a closing character matches an opening one
-- >>> closeMatchesOpen '(' ')'
-- True
-- >>> closeMatchesOpen '[' ']'
-- True
-- >>> closeMatchesOpen '{' '}'
-- True
-- >>> closeMatchesOpen '<' '>'
-- True
-- >>> closeMatchesOpen '(' ']'
-- False
-- >>> closeMatchesOpen '[' '>'
-- False
-- >>> closeMatchesOpen '{' ')'
-- False
-- >>> closeMatchesOpen '<' '}'
-- False
closeMatchesOpen :: Char -> Char -> Bool
closeMatchesOpen open close = close == closing open

-- | Return the matching closing character
-- >>> closing '('
-- ')'
-- >>> closing '['
-- ']'
-- >>> closing '{'
-- '}'
-- >>> closing '<'
-- '>'
closing :: Char -> Char
closing open = let (pair:[]) = [x | x <- pairs, open == head x]
                               in last pair

scoreCorrupted :: Char -> Int
scoreCorrupted ')' = 3
scoreCorrupted ']' = 57
scoreCorrupted '}' = 1197
scoreCorrupted '>' = 25137

scoreClose :: Char -> Int
scoreClose ')' = 1
scoreClose ']' = 2
scoreClose '}' = 3
scoreClose '>' = 4
-- trick to avoid having to find closing character:
-- just score the opening ones the same way
scoreClose '(' = 1
scoreClose '[' = 2
scoreClose '{' = 3
scoreClose '<' = 4

-- | Score completion
-- >>> scoreCompletion "])}>"
-- 294
-- >>> scoreCompletion "}}]])})]"
-- 288957
-- >>> scoreCompletion ")}>]})"
-- 5566
-- >>> scoreCompletion "}}>}>))))"
-- 1480781
-- >>> scoreCompletion "]]}}]}]}>"
-- 995444
scoreCompletion :: String -> Int
scoreCompletion s = foldl (\acc -> \c -> 5 * acc + scoreClose c) 0 s
