import System.IO

type Command = String
type Displacement = Int
data Instruction = Instruction Command Displacement deriving Show

type HPos = Int
type Depth = Int
data Movement = Movement HPos Depth deriving Show

type Aim = Int
data AimedPosition = AimedPosition HPos Depth Aim deriving Show

main = do
        input <- getContents
        print $ day21 $ lines input
        print $ day22 $ lines input

day21 :: [String] -> Int
day21 a = summariseMovement
        $ foldl applyMovement (Movement 0 0)
                $ ( map (runInstruction . makeInstruction . splitCommandLine) a)

-- | Split a command line into the command and its value
-- >>> splitCommandLine "aa 1"
-- ("aa"," 1")
splitCommandLine :: String -> (String, String)
splitCommandLine = break (==' ')

-- | Create an instruction from a tuple of strings
-- >>> makeInstruction ("1", "2")
-- Instruction "1" 2
makeInstruction :: (String, String) -> Instruction
makeInstruction (a,b) = Instruction a (read b :: Int)

-- | Return the (x,y) displacement of an instruction
-- >>> runInstruction $ Instruction "forward" 1
-- Movement 1 0
-- >>> runInstruction $ Instruction "down" 2
-- Movement 0 2
-- >>> runInstruction $ Instruction "up" 3
-- Movement 0 (-3)
runInstruction :: Instruction -> Movement
runInstruction i = case i of
                      Instruction "forward" i -> Movement i 0
                      Instruction "down" i -> Movement 0 i
                      Instruction "up" i -> Movement 0 (-i)

-- | Combine two movements into one
-- >>> applyMovement (Movement 1 2) (Movement 3 (-4))
-- Movement 4 (-2)
applyMovement :: Movement -> Movement -> Movement
applyMovement (Movement h1 d1) (Movement h2 d2) = Movement (h1+h2) (d1+d2)

-- | Summarise a movemement by multiplying both its components
-- >>> summariseMovement $ Movement 2 10
-- 20
summariseMovement :: Movement -> Int
summariseMovement (Movement u d) = u * d

-- | Day 2 part 2
-- >>> day22 ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
-- 900
day22 :: [String] -> Int
day22 a = summariseAimedPosition
        $ foldl applyAimedMovement (AimedPosition 0 0 0)
                $ ( map (runInstruction . makeInstruction . splitCommandLine) a)

-- | Combine two movements into one
-- >>> applyAimedMovement (AimedPosition 0 0 0) (Movement 5 0)
-- AimedPosition 5 0 0
-- >>> applyAimedMovement (AimedPosition 5 0 0) (Movement 0 5)
-- AimedPosition 5 0 5
-- >>> applyAimedMovement (AimedPosition 5 0 5) (Movement 8 0)
-- AimedPosition 13 40 5
applyAimedMovement:: AimedPosition -> Movement -> AimedPosition
applyAimedMovement (AimedPosition h d a) (Movement dh da) = AimedPosition (h+dh) (d+dh*a) (a+da)

-- | Summarise an aimed movemement by multiplying both its components
-- >>> summariseAimedPosition $ AimedPosition 2 10 0
-- 20
summariseAimedPosition :: AimedPosition -> Int
summariseAimedPosition (AimedPosition u d _) = u * d
