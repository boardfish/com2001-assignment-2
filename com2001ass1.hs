{-
     COM2001 Spring Assignment 1
     Haskell Template
     (c) 2018 Mike Stannett
     Email: m.stannett@sheffield.ac.uk
-}

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
  -- Additional debug method that displays the contents of all boxes rather
  -- than just 1.
  getBoxes     :: cfg -> [Int]
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
-- maxBoxNum uses the above insMaxBoxNum helper to find the highest box number
-- for each instruction in the program, then finds the largest of these.
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
    show (BATConfig boxes counter) = "boxes = " ++ show boxes 
      ++ ";\ncounter = " ++ show counter

-- clr is quite self-explanatory - it uses the given index n to replace the
-- contents of the box with 0.
clr :: Int -> [Int] -> [Int]
clr n boxes = let (xs, (y:ys)) = splitAt n boxes
                    in xs ++ (0:ys)

-- inc uses the same method to increase the box contents by 1.
inc :: Int -> [Int] -> [Int]
inc n boxes = let (xs, (y:ys)) = splitAt n boxes
                    in xs ++ ((y+1):ys)

-- jeq makes an assumption that the given indices n1 and n2 are valid boxes,
-- which is supported during initialisation by the maxBoxNum method.
jeq :: Int -> Int -> Int -> [Int] -> Int -> Int
jeq n1 n2 t boxes counter
  | boxes!!n1 == boxes!!n2 = t
  | otherwise = counter + 1

-- doMove uses the above methods to modify the boxes or counter depending on
-- the type of instruction executed.
doMove :: Instruction -> BATConfig -> BATConfig
doMove (CLR box) (BATConfig { boxes = b, counter = c }) = 
    (BATConfig (clr box b) (c + 1))
doMove (INC box) (BATConfig { boxes = b, counter = c }) = 
    (BATConfig (inc box b) (c + 1))
doMove (JEQ box box2 target) (BATConfig { boxes = b, counter = c }) = 
    (BATConfig b (jeq box box2 target b c))

-- IMPLEMENTING THE BATComputer
-- ============================
-- User inputs run from Box 1 onwards. Output is what ends up in Box 1.
-- Box 0 can be used by programs for calculations.
instance ProgrammableComputer BATConfig  where
    -- PROBLEM 3: 
    -- initialise   :: Program -> [Input] -> BATConfig
    -- initialise uses maxBoxNum to figure out whether any additional boxes are
    -- necessary, and initialises them to empty if so.
    -- If an empty program is supplied, an accept state is assumed.
    initialise [] inputs = (BATConfig inputs (-1))
    initialise program inputs = let emptyBoxes = 
                                        ((maxBoxNum program) - (length inputs))
                                    boxes   = 
                                        (0:inputs) ++ (replicate emptyBoxes 0)
                                 in (BATConfig boxes 0)
    -- PROBLEM 4: 
    -- acceptState  :: Program -> cfg -> Bool
    -- Programs terminate based on a final instruction that asserts that the
    -- necessary condition is fulfilled. This sets the counter to -1 to signal
    -- that an accept state has been reached.
    acceptState program (BATConfig { boxes = b, counter = c }) = c < 0
    -- PROBLEM 5: 
    -- doNextMove   :: Program -> cfg -> cfg
    -- doNextMove executes the instruction at the current counter value in the
    -- BATComputer.
    doNextMove p (BATConfig { boxes = b, counter = c }) = 
        doMove (p!!c) (BATConfig b c)
    -- PROBLEM 6: 
    -- runFrom      :: Program -> cfg -> cfg
    -- If the BATComputer is in an accept state, return its configuration. Else,
    -- execute the next move.
    runFrom p (BATConfig { boxes = b, counter = c }) 
      | acceptState p (BATConfig { boxes = b, counter = c}) = 
          (BATConfig { boxes = b, counter = c})
      | otherwise = runFrom p (doNextMove p (BATConfig b c))
    -- PROBLEM 7: getOutput    :: cfg -> Output
    -- getOutput returns the contents of box 1.
    -- getBoxes is a method used for debug which returns the contents of all
    -- boxes including 0.
    getOutput (BATConfig { boxes = (b0:b1:_) }) = b1
    getBoxes (BATConfig { boxes = b }) = b


-- This function is included to help with testing. Running
-- "execute p xs" should show the output generated when
-- running program p with user input(s) xs  

execute :: Program -> [Input] -> Output
execute p ins = getOutput ((runProgram p ins) :: BATConfig)

-- executeDebug is an equivalent that outputs the contents of  all 
-- available boxes.

executeDebug :: Program -> [Input] -> [Int]
executeDebug p ins = getBoxes ((runProgram p ins) :: BATConfig)

-- PROBLEM 8. 
-- ---------------------------
-- start a program at instruction n instead of 0.  In other
-- words, change Jump instructions from (J x y t) to (J x y (t+n))
-- and leave all other instructions unchanged.

transposeInstruction :: Int -> Instruction -> Instruction
transposeInstruction n (JEQ x y t) 
  | t<0 = (JEQ x y t)
  | otherwise = (JEQ x y (t+n))
transposeInstruction n i = i

-- transposeInstructionWithKey works similarly to transpose, but instructions
-- that set the counter to -1 are changed to the provided key value, which
-- should mark the first instruction in the next program when it is used in the
-- *->* function.
transposeInstructionWithKey :: Int -> Int -> Instruction -> Instruction
transposeInstructionWithKey key n (JEQ x y t) 
  | t<0 = (JEQ x y key)
  | otherwise = (JEQ x y (t+n))
transposeInstructionWithKey key n i = i

-- transpose is a simple recursion on transposeInstruction.
-- TODO: use a mapping function instead.
transpose :: Int -> Program -> Program
transpose n (i:ps) = ((transposeInstruction n i):(transpose n ps))
transpose _ [] = []

-- ditto for transposeInstructionWithKey.
-- TODO: use a mapping function instead.
transposeWithKey :: Int -> Int -> Program -> Program
transposeWithKey key n (i:ps) = ((transposeInstructionWithKey key n i):(transposeWithKey key n ps))
transposeWithKey _ _ [] = []


-- PROBLEM 9. 
-- ---------------------------
-- join two programs together, so as to run one
-- after the other

(*->*) :: Program -> Program -> Program
p1 *->* [] = p1
[] *->* p2 = p2
p1 *->* p2 = let l = length p1
              in foldr (:) (transpose l p2) (transposeWithKey l 0 p1)


-- PROBLEM 10. 
-- ---------------------------
-- program to compute B1 = B1 + B2

adder :: Program
adder = [(CLR 0),       -- clear box 0, used to keep track of what's added to 1
         (JEQ 0 2 (-1)),-- if box 1 has been incremented box 2 times, stop 
         (INC 0),       -- increment box 0
         (INC 1),       -- increment box 1
         (JEQ 0 0 1)    -- test for equality again
        ] 

-- PROBLEM 11.
-- ---------------------------
-- create a program to copy the contents of box m to box n (leave box m unchanged)

copyBox :: Int -> Int -> Program
copyBox m n
  | m == n = []         -- cannot copy value back into its own box
  | otherwise = 
      [(CLR n),         -- clear target box
       (JEQ m n (-1)),  -- check for equality
       (INC n),         -- increment target box
       (JEQ n n 1)      -- loop back to 1 to test for equality again
      ]

-- PROBLEM 12.
-- ---------------------------
-- program to compute B1 = Bx + By

-- addXY :: Int -> Int -> Program
-- TODO: Please fix!
addXY 1 2 = adder -- adding boxes 1 and 2 reduces to adder function
addXY 2 1 = adder -- ditto
addXY x 1 = copyBox x 2 *->* adder
addXY 2 y = copyBox y 1 *->* adder
addXY x y = (copyBox x 1 *->* copyBox y 2) *->* adder

-- END OF ASSIGNMENT
