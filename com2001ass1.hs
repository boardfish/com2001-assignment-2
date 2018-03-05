{-
     COM2001 Spring Assignment 1
     Haskell Template
     (c) 2018 Mike Stannett
     Email: m.stannett@sheffield.ac.uk
-}

-- If you would like to add your name and/or registration number
-- to this file, please do so here:
-- Simon Fish
-- 160153503
-- sgfish1@sheffield.ac.uk

type Input  = Int
type Output = Int
 
-- A program is something that tells a computer how to
-- move from one configuration to another, how to
-- recognize when a configuration represents a valid
-- accept state, and so on.

class (Eq cfg) => ProgrammableComputer cfg where
  initialise   :: Program -> [Input] -> cfg
  getOutput    :: cfg -> Output
  getBoxes    :: cfg -> [Int]
  acceptState  :: Program -> cfg -> Bool
  doNextMove   :: Program -> cfg -> cfg
  runFrom      :: Program -> cfg -> cfg
  runProgram   :: Program -> [Input] -> cfg
  -- Default implementation
  runProgram p is = runFrom p (initialise p is)


  
-- The BATcomputer has just 3 types of instruction
-- CLR b        == empty box b
-- INC b        == add a token to box b
-- JEQ b1 b2 t  == if boxes b1 and b2 contain the same 
--                 number of tokens, jump to instruction t
--
data Instruction
  = CLR {box :: Int} 
  | INC {box :: Int}
  | JEQ {box1   :: Int, 
         box2   :: Int,
         target :: Int}
  deriving (Eq, Show)
   
type Program = [Instruction]



-- PROBLEM 1. 
-- --------------------------
-- Each instruction in a program refers to one or
-- more boxes.  What is the highest box number used
-- anywhere in the program?
insMaxBoxNum :: Instruction -> Int
insMaxBoxNum (CLR box) = box
insMaxBoxNum (INC box) = box
insMaxBoxNum (JEQ box1 box2 _) = max box1 box2
maxBoxNum :: Program -> Int
maxBoxNum p = maximum (map insMaxBoxNum p)


-- The configuration of a BATcomputer is given once
-- you know how many tokens are in each box, and
-- which instruction should be executed next
data BATConfig = BATConfig {
    boxes   :: [Int],
    counter :: Int
    } deriving (Eq)

    
-- PROBLEM 2. 
-- --------------------------
-- Make BATConfig an instance of the class Show,
-- so that configurations look like this when 
-- displayed on the screen
-- boxes = <list of box values>; 
-- counter = <counter value

instance Show BATConfig where
    show (BATConfig boxes counter) = "boxes = " ++ show boxes ++ ";\ncounter = " ++ show counter

clr :: Int -> [Int] -> [Int]
clr n boxes = let (xs, (y:ys)) = splitAt n boxes
                    in xs ++ (0:ys)
inc :: Int -> [Int] -> [Int]
inc n boxes = let (xs, (y:ys)) = splitAt n boxes
                    in xs ++ ((y+1):ys)
jeq :: Int -> Int -> Int -> [Int] -> Int -> Int
jeq n1 n2 t boxes counter
  | boxes!!n1 == boxes!!n2 = t
  | otherwise = counter + 1
doMove :: Instruction -> BATConfig -> BATConfig
doMove (CLR box) (BATConfig { boxes = b, counter = c }) = (BATConfig (clr box b) (c + 1))
doMove (INC box) (BATConfig { boxes = b, counter = c }) = (BATConfig (inc box b) (c + 1))
doMove (JEQ box box2 target) (BATConfig { boxes = b, counter = c }) = (BATConfig b (jeq box box2 target b c))
-- IMPLEMENTING THE BATComputer
-- ============================
-- User inputs run from Box 1 onwards. Output is what ends up in Box 1.
-- Box 0 can be used by programs for calculations.
instance ProgrammableComputer BATConfig  where
    -- PROBLEM 3: 
    -- initialise   :: Program -> [Input] -> BATConfig
    initialise program inputs = let emptyBoxes = ((maxBoxNum program) - (length inputs))
                                    boxes   = (0:inputs) ++ (replicate emptyBoxes 0)
                                    counter = 0
                                 in (BATConfig boxes counter)
    -- PROBLEM 4: 
    -- acceptState  :: Program -> cfg -> Bool
    acceptState program (BATConfig { boxes = b, counter = c }) = (length program) <= c
    -- PROBLEM 5: 
    -- doNextMove   :: Program -> cfg -> cfg
    doNextMove p (BATConfig { boxes = b, counter = c }) = doMove (p!!c) (BATConfig b c)
    -- PROBLEM 6: 
    -- runFrom      :: Program -> cfg -> cfg
    runFrom p (BATConfig { boxes = b, counter = c }) 
      | acceptState p (BATConfig { boxes = b, counter = c}) = (BATConfig { boxes = b, counter = c})
      | otherwise = runFrom p (doNextMove p (BATConfig b c))
    -- PROBLEM 7: getOutput    :: cfg -> Output
    getOutput (BATConfig { boxes = (b0:b1:_) }) = b1
    getBoxes (BATConfig { boxes = b }) = b


-- This function is included to help with testing. Running
-- "execute p xs" should show the output generated when
-- running program p with user input(s) xs  

execute :: Program -> [Input] -> Output
execute p ins = getOutput ((runProgram p ins) :: BATConfig)
executeDebug :: Program -> [Input] -> [Int]
executeDebug p ins = getBoxes ((runProgram p ins) :: BATConfig)


-- PROBLEM 8. 
-- ---------------------------
-- start a program at instruction n instead of 0.  In other
-- words, change Jump instructions from (J x y t) to (J x y (t+n))
-- and leave all other instructions unchanged.

transposeInstruction :: Int -> Instruction -> Instruction
transposeInstruction n (JEQ x y t) = (JEQ x y (t+n))
transposeInstruction n i = i

transpose :: Int -> Program -> Program
transpose n (i:ps) = ((transposeInstruction n i):(transpose n ps))
transpose _ [] = []


-- PROBLEM 9. 
-- ---------------------------
-- join two programs together, so as to run one
-- after the other

(*->*) :: Program -> Program -> Program
p1 *->* [] = p1
[] *->* p2 = p2
p1 *->* p2 = foldr (:) p2 p1 


-- PROBLEM 10. 
-- ---------------------------
-- program to compute B1 = B1 + B2

adder :: Program
adder = [(CLR 3), (JEQ 0 2 6), (INC 0), (INC 1), (INC 3), (JEQ 0 3 1)]
    

-- PROBLEM 11.
-- ---------------------------
-- create a program to copy the contents of box m to box n (leave box m unchanged)

copyBox :: Int -> Int -> Program
copyBox m n = [(JEQ m n 6), (CLR n), (JEQ m n 6), (INC n), (INC 0), (JEQ n 0 2)]


-- PROBLEM 12.
-- ---------------------------
-- program to compute B1 = Bx + By

-- addXY :: Int -> Int -> Program
-- addXY x y = ...


-- END OF ASSIGNMENT
