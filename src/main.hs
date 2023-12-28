-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

import Data.Char (isDigit, isLetter, isSpace, isLower, isUpper, digitToInt)
import Data.List (span)
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Maybe (isJust, fromJust)
import Pilha
import Map 

-- Part 1

-- Do not modify our definition of Inst and Code
data Token
  = PlusTok | MinusTok | TimesTok
  | OpenTok | CloseTok | SemiColonTok
  | AndTok | BoolEqTok | IntEqTok | LeTok | NotTok
  | AssignTok
  | WhileTok | DoTok
  | IfTok | ThenTok | ElseTok
  | IntTok Int | VarTok String | BoolTok Bool
  deriving (Show) 

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
state2Str state = if Map.isEmpty state then "" 
                  else init $ concatMap showPair (map2List state)
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


data Aexp
  = IntLit Integer
  | VarLitA String
  | AddExp Aexp Aexp
  | SubExp Aexp Aexp
  | MultExp Aexp Aexp

data Bexp
  = BoolLit Bool
  | VarLitB String
  | AndExp Bexp Bexp
  | NegExp Bexp
  | EquExpInt Aexp Aexp
  | EquExpBool Bexp Bexp
  | LeExp Aexp Aexp

data Stm
  = StoreStmA String Aexp
  | StoreStmB String Bexp
  | ParenthStm [Stm]    -- Check if this is necessary when we start doing parsing
  | IfStm Bexp Stm Stm 
  | WhileStm Bexp Stm

type Program = [Stm]

compA :: Aexp -> Code
compA (IntLit n) = [Push n]
compA (VarLitA var) = [Fetch var]
compA (AddExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Add]
compA (SubExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Sub]
compA (MultExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Mult]

compB :: Bexp -> Code
compB (BoolLit val) = if val then [Tru] else [Fals]
compB (VarLitB var) = [Fetch var]
compB (AndExp bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [And]
compB (NegExp bexp) = compB bexp ++ [Neg]
compB (EquExpInt aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Equ]
compB (EquExpBool bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [Equ]
compB (LeExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]


compile :: Program -> Code
compile prog = concat $ map compileStm prog

compileStm :: Stm -> Code
compileStm (StoreStmA var aexp) = compA aexp ++ [Store var]
compileStm (StoreStmB var bexp) = compB bexp ++ [Store var]
compileStm (ParenthStm stms) = concat $ map compileStm stms
compileStm (IfStm bexp stm1 stm2) = compB bexp ++ [Branch (compileStm stm1) (compileStm stm2)]
compileStm (WhileStm bexp stm) = [Loop (compB bexp) (compileStm stm)]

-- The parser needs to take the precedence of operators into account
-- parse :: String -> Program
parse = undefined -- TODO

lexer :: String -> [Token]
lexer [] = []
lexer ('+':restStr) = PlusTok : lexer restStr
lexer ('-':restStr) = MinusTok : lexer restStr
lexer ('*':restStr) = TimesTok : lexer restStr
lexer ('(':restStr) = OpenTok : lexer restStr
lexer (')':restStr) = CloseTok : lexer restStr
lexer (';':restStr) = SemiColonTok : lexer restStr
lexer ('a':'n':'d':' ':restStr) = AndTok : lexer restStr
lexer ('=':'=':restStr) = IntEqTok : lexer restStr
lexer ('=':restStr) = BoolEqTok : lexer restStr
lexer ('<':'=':restStr) = LeTok : lexer restStr
lexer ('n':'o':'t':' ':restStr) = NotTok : lexer restStr
lexer (':':'=':restStr) = AssignTok : lexer restStr
lexer ('w':'h':'i':'l':'e':' ':restStr) = WhileTok : lexer restStr
lexer ('d':'o':' ':restStr) = DoTok : lexer restStr
lexer ('i':'f':' ':restStr) = IfTok : lexer restStr
lexer ('t':'h':'e':'n':' ':restStr) = ThenTok : lexer restStr
lexer ('e':'l':'s':'e':' ':restStr) = ElseTok : lexer restStr
lexer ('T':'r':'u':'e':' ':restStr) = BoolTok True : lexer restStr
lexer ('F':'a':'l':'s':'e':' ':restStr) = BoolTok False : lexer restStr

lexer str@(char:restStr)
 | isSpace char = lexer restStr
 | isDigit char = let (digStr, restStr) = span isDigit str
                      stringToInt = foldl (\acc chr->10*acc+digitToInt chr) 0
                  in if null restStr || not (head restStr == '_' || isLetter (head restStr))
                      then IntTok (stringToInt digStr) : lexer restStr
                     else error "Syntax error: Variables cannot start with a digit"
 | isLower char = let (varStr, restStr) = span (\x -> isLetter x || isDigit x || x == '_') str
                   in VarTok varStr : lexer restStr
 | otherwise = error "Syntax error: Invalid symbol"


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

-- Compiler A
-- (AddExp (IntLit 1) (VarLitA "x"), [Fetch "x", Push 1, Add])
-- (SubExp (IntLit 1) (VarLitA "x"), [Fetch "x", Push 1, Sub])
-- (MultExp (IntLit 1) (VarLitA "x"), [Fetch "x", Push 1, Mult])
-- (AddExp (IntLit 2) (MultExp (IntLit 7) (IntLit 13)), [Push 13, Push 7, Mult, Push 2, Add])
-- (AddExp (IntLit 2) (MultExp (VarLitA "x") (VarLitA "y")), [Fetch "y", Fetch "x", Mult, Push 2, Add])

-- Compiler B
-- (NegExp (BoolLit True), [Tru, Neg])
-- (AndExp (BoolLit True) (BoolLit False), [Fals, Tru, And])
-- (AndExp (EquExpInt (AddExp (IntLit 1) (IntLit 2)) (IntLit 2)) (BoolLit False), [Fals, Push 2, Push 2, Push 1, Add, Equ, And])
-- (EquExpBool (LeExp (IntLit 2) (IntLit 1)) (BoolLit False), [Fals, Push 1, Push 2, Le, Equ])

-- Compiler Global
-- ([StoreStmA "x" (AddExp (IntLit 1) (IntLit 2))], [Push 2, Push 1, Add, Store "x"])
-- ([StoreStmA "x" (AddExp (IntLit 1) (IntLit 2)), StoreStmA "x" (AddExp (IntLit 1) (IntLit 2))], [Push 2, Push 1, Add, Store "x", Push 2, Push 1, Add, Store "x"])
-- ([WhileStm (BoolLit True) (ParenthStm [StoreStmA "x" (IntLit 1), StoreStmB "y" (BoolLit True), IfStm (BoolLit False) (StoreStmA "z" (IntLit 2)) (StoreStmA "z" (IntLit 3))])], [Loop [Tru] [Push 1,Store "x",Tru,Store "y",Fals,Branch [Push 2,Store "z"] [Push 3,Store "z"]]])




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
