-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

import Data.Char (isDigit, isLetter, isSpace, isLower, isUpper)
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Maybe (isJust, fromJust)
import Pilha
import Map 

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]
type State = (Map String (Either String Integer))
type Stack = (Pilha (Either String Integer))


createEmptyStack :: Stack
createEmptyStack = Pilha.empty

createEmptyState :: State
createEmptyState = Map.empty


stack2Str :: Stack -> String
stack2Str pilha
 | Pilha.isEmpty pilha = ""
 | Pilha.isEmpty (pop pilha) = showEither (top pilha)
 | otherwise = showEither (top pilha) ++ "," ++ stack2Str (pop pilha)
 where 
    showEither (Left "tt") = "True"
    showEither (Left "ff") = "False"
    showEither (Right int) = show int 
    showEither _ = error "Run-time error"


state2Str :: State -> String
state2Str state = if (Map.isEmpty state) then "" 
                  else init . concat $ map showPair (map2List state)
 where 
  showPair (k, Left "tt") = k ++ "=True,"
  showPair (k, Left "ff") = k ++ "=False,"
  showPair (k, Right int) = k ++ "=" ++ show int ++ ","


run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, storage) = ([], stack, storage) -- base case

run (Push int:code, stack, storage) = run (code, push (Right int) stack, storage)

run (Add:code, stack, storage) = run (code, push (Right (v2+v1)) (pop . pop $ stack), storage)
 where 
  (v1, v2) = get2RightInt stack

run (Mult:code, stack, storage) = run (code, push (Right (v2*v1)) (pop . pop $ stack), storage)
 where 
  (v1, v2) = get2RightInt stack

run (Sub:code, stack, storage) = run (code, push (Right (v1-v2)) (pop . pop $ stack), storage)
 where 
  (v1, v2) = get2RightInt stack

run (Tru:code, stack, storage) = run (code, push (Left "tt") stack, storage)

run (Fals:code, stack, storage) = run (code, push (Left "ff") stack, storage)

-- TODO simplify this 
run (Equ:code, stack, storage)
 | isSameType && v1 == v2 = run (code, push (Left "tt") (pop . pop $ stack), storage)
 | isSameType = run (code, push (Left "ff") (pop . pop $ stack), storage)
 | otherwise = error "Run-time error"
 where 
  v1 = top stack 
  v2 = top (pop stack)
  isSameType = (isLeft v1 && isLeft v2) || (isRight v1 && isRight v2)

run (Le:code, stack, storage)
 | v1 <= v2 = run (code, push (Left "tt") (pop . pop $ stack), storage)
 | otherwise = run (code, push (Left "ff") (pop . pop $ stack), storage)
 where (v1, v2) = get2RightInt stack 

run (And:code, stack, storage)
 | v1 == "tt" && v2 == "tt" = run (code, push (Left "tt") (pop . pop $ stack), storage)
 | otherwise = run (code, push (Left "ff") (pop . pop $ stack), storage)
 where (v1, v2) = get2LeftString stack

run (Neg:code, stack, storage)
 | top stack == Left "ff" = run (code, push (Left "tt") (pop stack), storage)
 | top stack == Left "tt" = run (code, push (Left "ff") (pop stack), storage)
 | otherwise = error "Run-time error"

run (Fetch key:code, stack, storage)
 | isJust value = run (code, push (fromJust value) stack, storage)
 | otherwise = error "Run-time error"
 where value = find key storage

run (Store key:code, stack, storage) = run (code, pop stack, insert key (top stack) storage)

run (Noop:code, stack, storage) = run (code, stack, storage)

run (Branch c1 c2:code, stack, storage)
 | top stack == Left "tt" = run (c1++code, pop stack, storage)
 | top stack == Left "ff" = run (c2++code, pop stack, storage)
 | otherwise = error "Run-time error"

run (Loop c1 c2:code, stack, storage) = run(c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code , stack, storage)


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

lexer :: String -> [String]
lexer "" = []
lexer (curr1:curr2:rest) 
  | elem (curr1:[curr2]) ["<=", ":=", "=="] = (curr1:[curr2]) : lexer rest
lexer (curr:rest)
  | elem curr ['(', ')', '+', '-', '*', ';', '='] = [curr] : lexer rest
  | isDigit curr = let restStr = dropWhile isDigit rest
                       nextChar = head restStr
                   in if not (nextChar == '_' || isLetter nextChar)
                      then takeWhile isDigit (curr:rest) : lexer (dropWhile isDigit rest)
                      else error "Variable names cannot start with digits"
  | isLower curr = let variableFilter char = char == '_' || isLetter char || isDigit char
                   in takeWhile variableFilter (curr:rest) : lexer (dropWhile variableFilter rest)
  | isUpper curr = let str = takeWhile isLower rest
                   in if curr:str == "True" || curr:str == "False"
                      then (curr:str) : lexer (dropWhile isLower rest)
                      else error "Variable names cannot start with uppercase characters"
  | isSpace curr = lexer (dropWhile isSpace rest)
  | otherwise = error "Invalid token"


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
-- testAssembler [Push 1,Push 2,And]:                     "Run-time error"
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]:      "Run-time error"
-- testAssembler [Push 10,Push 2,Branch [Add] [Sub]] :    "Run-time error"
testCasesCompile :: [([Inst], (String, String))]
testCasesCompile = [
    ([Push 10,Push 4,Push 3,Sub,Mult], ("-10","")),
    ([Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"], ("","a=3,someVar=False,var=True")),
    ([Fals,Store "var",Fetch "var"], ("False","var=False")),
    ([Push (-20),Tru,Fals], ("False,True,-20","")),
    ([Push (-20),Tru,Tru,Neg], ("False,True,-20","")),
    ([Push (-20),Tru,Tru,Neg,Equ], ("False,-20","")),
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
