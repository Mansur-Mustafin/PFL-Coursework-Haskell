-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

import Data.List (span)
import Pilha
import Map 
import Lexer
import Compile 

-- Part 1

-- Do not modify our definition of Inst and Code
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

-- The parser needs to take the precedence of operators into account

parse :: String -> Program
parse = undefined -- TODO

buildData :: [Token] -> Program
buildData [] = []
buildData list = [stm] ++ buildData restTok
 where (stm, restTok) = getStatement list


getStatement :: [Token] -> (Stm, [Token])
getStatement list@(WhileTok:rst) = getWhileStatement list 
getStatement list@(IfTok:rst) = getIfStatement list 


getWhileStatement :: [Token] -> (Stm, [Token])
getWhileStatement = undefined

getIfStatement :: [Token] -> (Stm, [Token])
getIfStatement = undefined



--------------------------------------------------------------------------------------------------------------
-- We should define the levels of operations: 
-- Sum / Sub -> Mult -> IntLit / VarLit / Pars

getAexp :: [Token] -> (Aexp, [Token])
getAexp tokens =
  case parseSumSub tokens of
    Just (aExp, tokens) -> (aExp, tokens)
    _                   -> error "Syntax error: Algebric expression."

-- parse Sum and Substruction 
parseSumSub :: [Token] ->  Maybe (Aexp, [Token])
parseSumSub tokens =
  case parseProd tokens of 
    Just (exp1, (PlusTok:restTokens1)) ->
      case parseSumSub restTokens1 of
        Just (exp2, restTokens2) -> Just (AddExp exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    Just (exp1, (MinusTok: restTokens1)) ->
      case parseSumSub restTokens1 of
        Just (exp2, restTokens2) -> Just (SubExp exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    result -> result

-- parse Product
parseProd :: [Token] ->  Maybe (Aexp, [Token])
parseProd tokens =
  case parseIntVarPar tokens of 
    Just (exp1, (TimesTok: restTokens1)) ->
      case parseProd restTokens1 of
        Just (exp2, restTokens2) -> Just (MultExp exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    other -> other

-- parse the Integer values or Variables
parseIntVarPar :: [Token] -> Maybe (Aexp, [Token])
parseIntVarPar (IntTok n: restTokens) = Just (IntLit n, restTokens)
parseIntVarPar (VarTok v: restTokens) = Just (VarLitA v, restTokens)
parseIntVarPar (OpenTok:restTokens1) =
  case parseSumSub restTokens1 of
    Just (exp1, (CloseTok:restTokens2)) -> Just (exp1, restTokens2)
    _ -> Nothing
parseIntVarPar _ = Nothing

--------------------------------------------------------------------------------------------------------------
-- We should define the levels of operations: 
-- AndTok -> BoolEqTok -> NotTok -> IntEqTok -> LeTok -> BoolLit/VarLit/Pars

getBexp :: [Token] -> (Bexp, [Token])
getBexp tokens =   
  case parseAnd tokens of
    Just (bExp, tokens) -> (bExp, tokens)
    _                   -> error "Syntax error: Boolean expression."


-- parse And
parseAnd :: [Token] ->  Maybe (Bexp, [Token])
parseAnd tokens =
  case parseBoolEq tokens of 
    Just (exp1, (AndTok:restTokens1)) ->
      case parseAnd restTokens1 of
        Just (exp2, restTokens2) -> Just (AndExp exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    result -> result


-- parse Boolean equality
parseBoolEq :: [Token] ->  Maybe (Bexp, [Token])
parseBoolEq tokens =
  case parseNot tokens of 
    Just (exp1, (BoolEqTok:restTokens1)) ->
      case parseBoolEq restTokens1 of
        Just (exp2, restTokens2) -> Just (EquExpBool exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    result -> result


-- parse Integer equality
parseNot :: [Token] ->  Maybe (Bexp, [Token])
parseNot (NotTok:restTokens) = 
  case parseNot restTokens of
    Just (exp1, restTokens1) -> Just (NegExp exp1, restTokens1)
    result -> result

parseNot tokens = parseLowerNot tokens

parseLowerNot :: [Token] ->  Maybe (Bexp, [Token])
parseLowerNot tokens = 
  case parseIntEqLe tokens of
    Just (exp1, restTokens1) -> Just (exp1, restTokens1)
    Nothing -> case parseBoolVarPars tokens of
      Just (exp2, restTokens2) -> Just (exp2, restTokens2)
      Nothing -> Nothing


-- parse Boolean equality
parseIntEqLe :: [Token] ->  Maybe (Bexp, [Token])
parseIntEqLe tokens =
  case parseSumSub tokens of 
    Just (exp1, (IntEqTok:restTokens1)) ->
      case parseSumSub restTokens1 of
        Just (exp2, restTokens2) -> Just (EquExpInt exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    Just (exp1, (LeTok:restTokens1)) ->
      case parseSumSub restTokens1 of
        Just (exp2, restTokens2) -> Just (LeExp exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    _ -> Nothing


-- parse the Integer values or Variables
parseBoolVarPars :: [Token] -> Maybe (Bexp, [Token])
parseBoolVarPars (BoolTok n: restTokens) = Just (BoolLit n, restTokens)
parseBoolVarPars (VarTok v: restTokens) = Just (VarLitB v, restTokens)
parseBoolVarPars (OpenTok:restTokens1) =
  case parseAnd restTokens1 of
    Just (exp1, (CloseTok:restTokens2)) -> Just (exp1, restTokens2)
    _ -> Nothing
parseBoolVarPars _ = Nothing



-- ####################################################################################################################
-- #                                                                                                                  #
-- #                                                      Tests                                                       #
-- #                                                                                                                  #
-- ####################################################################################################################
-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
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



-- Examples:
-- testAssembler [Push 1,Push 2,And]:                     "Run-time error"
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]:      "Run-time error"
-- testAssembler [Push 10,Push 2,Branch [Add] [Sub]] :    "Run-time error"

testCasesCompile1 :: [([Inst], (String, String))]
testCasesCompile1 = [
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

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


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
    runAllTests $ zip [1..] testCasesCompile1
    -- Aexp


-- testAssembler $ compA $ fst $ getAexp $ lexer "((2 + 3) * (4 - 1)) + 5" deve dar ("20","")
-- testAssembler $ compB $ fst $ getBexp $ lexer "not True and 2 <= 5 = 3 == 4" deve dar False
