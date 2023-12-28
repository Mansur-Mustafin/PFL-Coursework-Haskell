-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

import Pilha
import Map 

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]
type State = (Map String (Either Bool Integer))
type Stack = (Pilha (Either Bool Integer))


createEmptyStack :: Stack
createEmptyStack = Pilha.empty

createEmptyState :: State
createEmptyState = Map.empty


showEither :: (Show a, Show b) => Either a b -> String
showEither (Left bool) = show bool
showEither (Right int) = show int


stack2Str :: Stack -> String
stack2Str pilha
 | Pilha.isEmpty pilha = ""
 | Pilha.isEmpty $ pop pilha = showEither (top pilha)
 | otherwise = showEither (top pilha) ++ "," ++ stack2Str (pop pilha)


state2Str :: State -> String
state2Str state 
 | Map.isEmpty state = "" 
 | otherwise = init . concat $ map showPair (map2List state)
 where 
  showPair (k, val) = k ++ "=" ++ showEither val ++ ","


run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, storage) = ([], stack, storage) -- base case

run (Push int:code, stack, storage) = run (code, push (Right int) stack, storage)

run (Add:code, stack, storage) = run (code, push (Right (v2+v1)) newStack, storage)
 where 
  (v1, v2, newStack) = get2RightValues stack

run (Mult:code, stack, storage) = run (code, push (Right (v2*v1)) newStack, storage)
 where 
  (v1, v2, newStack) = get2RightValues stack

run (Sub:code, stack, storage) = run (code, push (Right (v1-v2)) newStack, storage)
 where 
  (v1, v2, newStack) = get2RightValues stack

run (Tru:code, stack, storage) = run (code, push (Left True) stack, storage)

run (Fals:code, stack, storage) = run (code, push (Left False) stack, storage)

run (Equ:code, stack, storage) =
  case (top stack, top . pop $ stack) of
    (Left v1, Left v2)   -> run (code, push (Left (v1 == v2)) (pop . pop $ stack), storage)
    (Right v1, Right v2) -> run (code, push (Left (v1 == v2)) (pop . pop $ stack), storage)
    _                    -> error "Run-time error"

run (Le:code, stack, storage)
 | v1 <= v2 = run (code, push (Left True) newStack, storage)
 | otherwise = run (code, push (Left False) newStack, storage)
 where (v1, v2, newStack) = get2RightValues stack 

run (And:code, stack, storage)
 | v1 && v2 = run (code, push (Left True) newStack, storage)
 | otherwise = run (code, push (Left False) newStack, storage)
 where (v1, v2, newStack) = get2LeftValues stack

run (Neg:code, stack, storage) =
  case top stack of
    Left bool -> run (code, push (Left (not bool)) (pop stack), storage)
    _         -> error "Run-time error"

run (Fetch key:code, stack, storage) = 
  case find key storage of 
    Just value -> run (code, push value stack, storage)
    Nothing    -> error "Run-time error"

run (Store key:code, stack, storage) 
 = run (code, pop stack, insert key (top stack) storage)

run (Noop:code, stack, storage) = run (code, stack, storage)

run (Branch c1 c2:code, stack, storage) =
  case top stack of
    Left True  -> run (c1++code, pop stack, storage)
    Left False -> run (c2++code, pop stack, storage)
    _          -> error "Run-time error"

run (Loop c1 c2:code, stack, storage) 
 = run(c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, storage)

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
--  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")


-- ####################################################################################################################
-- #                                                                                                                  #
-- #                                                      Tests                                                       #
-- #                                                                                                                  #
-- ####################################################################################################################

-- Examples:
-- testAssembler [Push 1,Push 2,And]                     "Run-time error"
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]      "Run-time error"
-- testAssembler [Push 10,Push 2,Branch [Add] [Sub]]     "Run-time error"
-- testAssembler [Tru,Fals,Neg,Add]                      "Run-time error"
-- testAssembler [Tru,Push 2,Equ]                        "Run-time error"

testCasesCompile :: [([Inst], (String, String))]
testCasesCompile = [
    ([Push 10,Push 4,Push 3,Sub,Mult], ("-10","")),
    ([Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"], ("","a=3,someVar=False,var=True")),
    ([Fals,Store "var",Fetch "var"], ("False","var=False")),
    ([Push (-20),Tru,Fals], ("False,True,-20","")),
    ([Push (-20),Tru,Tru,Neg], ("False,True,-20","")),
    ([Push (-20),Tru,Tru,Neg,Equ], ("False,-20","")),
    ([Push (-20),Push (-20),Equ], ("True","")),
    ([Push (-20),Push (10),Equ], ("False","")),
    ([Push (-20),Push (-21), Le], ("True","")),
    ([Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"], ("","x=4")),
    ([Push 10, Store "i", Push 1, Store "fact", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "fact", Mult, Store "fact", Push 1, Fetch "i", Sub, Store "i"]], ("","fact=3628800,i=1")),
    ([Push 10,Push 2,Tru,Branch [Add] [Sub]], ("12","")),
    ([Push 10,Push 2,Fals,Branch [Add] [Sub]], ("-8",""))
    ]

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

runTest :: (Int, ([Inst], (String, String))) -> IO ()
runTest (index, (input, expected)) 
 | result == expected = putStrLn $ "Test " ++ show index ++ " passed."
 | otherwise = putStrLn $ "Test " ++ show index ++ " failed. Expected " ++ show expected ++ " but got " ++ show result
 where result = testAssembler input

runAllTests :: [(Int, ([Inst], (String, String)))] -> IO ()
runAllTests [] = return ()  
runAllTests (test:rest) = do
    runTest test 
    runAllTests rest 

main :: IO ()
main = do
    putStrLn "Starting tests..."
    runAllTests $ zip [1..] testCasesCompile 
    putStrLn "All tests completed."
