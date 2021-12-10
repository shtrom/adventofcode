import System.IO

main = do input<- getContents
          print $ day10 $ lines input

-- | Find and score corrupted lines
-- >>> day10 ["[({(<(())[]>[[{[]{<()<>>","[(()[<>])]({[<{<<[]>>(","{([(<{}[<>[]}>{[]{[(<()>","(((({<>}<{<{<>}{[]{[]{}","[[<[([]))<([[{}[[()]]]","[{[{({}]{}}([{[{{{}}([]","{<[[]]>}<{[{[{[]{()[[[]","[<(<(<(<{}))><([]([]()","<{([([[(<>()){}]>(<<{{","<{([{{}}[<[[[<>{}]]]>[]]"]
-- 26397
day10 :: [String] -> Int
day10 l = sum $ map score $ [x | (x:xs) <- map (checkChunks []) l, isClose x]


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
checkChunks :: String -> String -> String
checkChunks [] [] = []
checkChunks y [] = y
checkChunks [] (x:xs) = case isOpen x of
                        True -> checkChunks [x] xs
                        False -> [x]
checkChunks (y:ys) (x:xs) = case isOpen x of
                        True -> checkChunks (x:y:ys) xs ++ (y:ys)
                        False -> case closeMatchesOpen y x of
                                True -> checkChunks ys xs ++ (ys)
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
closeMatchesOpen open close = let (pair:[]) = [x | x <- pairs, open == head x]
                               in close == last pair

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score 'v' = 0
score 'i' = 0
