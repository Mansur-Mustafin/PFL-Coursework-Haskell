{-# OPTIONS_GHC -Wno-incomplete-patterns #-}  -- TODO: delete this
{-|
  Module      : Main
  Description : Holds the main functionality of the project
  Copyright   : Daniel Ferreira and Mansur Mustafin, 2023
  Maintainer  : unidsferreira2003@gmail.com and mustafin.mansur02@gmail.com

  This module holds the primary functions used for the whole program, that is, the functions responsible for
  parsing a string containing the program's input and running a list of instructions built from the parser.
-}

-- PFL 2023/24 - Haskell practical assignment
module Main where

import Pilha
--import qualified AVLMap as Map
import Map
import Lexer
import Compile
import Test

-- Part 1

{-|
    Defines the storage of the program, where the values of variables are kept.
    A variable can hold a boolean or integer value
-}
type State = (Map.Map String (Either Bool Integer))

{-|
    Defines the evaluation stack of the program
-}
type Stack = (Pilha (Either Bool Integer))

{-| 
    Builds an empty evaluation stack to be used throughout the program
-}
createEmptyStack :: Stack
createEmptyStack = Pilha.empty

{-|
    Builds an empty map to be used as the storage throughout the program
-}
createEmptyState :: State
createEmptyState = Map.empty

{-|
    Prints the left or right value associated with an Either data variable
-}
showEither :: (Show a, Show b) => Either a b -> String
showEither (Left bool) = show bool
showEither (Right int) = show int

{-|
    Builds a string that represents the contents of the evaluation stack that is given as input.
    The leftmost value of the string represents the value at the top of the evaluation stack,
    while the rightmost value represents the value at the bottom of the stack.
-}
stack2Str :: Stack -> String
stack2Str pilha
 | Pilha.isEmpty pilha = ""
 | Pilha.isEmpty $ pop pilha = showEither (top pilha)
 | otherwise = showEither (top pilha) ++ "," ++ stack2Str (pop pilha)

{-|
    Builds a string that represents the contents of the program's storage.
    Its values are variable-value pairs that are organized in alphabetical order of the variable name.
-}
state2Str :: State -> String
state2Str state
 | Map.isEmpty state = ""
 | otherwise = init $ concatMap showPair (Map.map2List state)
 where
  showPair (k, val) = k ++ "=" ++ showEither val ++ ","

{-|
    Evaluates the instruction at the head of the list of instructions, which must be a valid instruction from the
    'Compile.Inst' data type. The instruction updates the evaluation stack and the program's storage accordingly so that
    it can be used by the next instruction. 
-}
run :: (Code, Stack, State) -> (Code, Stack, State)

run ([], stack, storage) = ([], stack, storage)

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
  if last key == '$' 
    then case top stack of
      Right index -> fetchValue (key ++ show index) (pop stack)
      _ -> error "Run-time error"
  else fetchValue key stack
  where fetchValue k stk = case Map.find k storage of
                            Just value -> run (code, push value stk, storage)
                            Nothing -> error "Run-time error"

run (Store key:code, stack, storage) =
  if last key == '$'
    then case top stack of
      Right index -> run (code, pop . pop $ stack, Map.insert (key++show index) (top . pop $ stack) storage)
      _ -> error "Run-time error"
  else run (code, pop stack, Map.insert key (top stack) storage)

run (Noop:code, stack, storage) = run (code, stack, storage)

run (Branch c1 c2:code, stack, storage) =
  case top stack of
    Left True  -> run (c1++code, pop stack, storage)
    Left False -> run (c2++code, pop stack, storage)
    _          -> error "Run-time error"

run (Loop c1 c2:code, stack, storage)
 = run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, storage)


{-|
    Parses a string that represents a program, tokenizing it and translating it into
    the correct statements from the 'Compile.Stm', 'Compile.Aexp' and 'Compile.Bexp' data types
-}
parse :: String -> Program
parse =  buildData . lexer

{-|
    Translates a list of token to the correct statements from the 'Compile.Stm', 'Compile.Aexp' and 'Compile.Bexp' data types
-}
buildData :: [Token] -> Program
buildData [] = []
buildData list = stm : buildData restTok
 where (stm, restTok) = getStatement list

{-|
    Builds a valid statement from the 'Compile.Stm' data type with the next tokens that appear in the token list given as input
-}
getStatement :: [Token] -> (Stm, [Token])
getStatement list@(WhileTok:rest) = getWhileStatement list                            -- while
getStatement list@(IfTok:rest) = getIfStatement list                                  -- if 
getStatement list@(VarTok var:AssignTok:ListTok:(IntTok _):SemiColonTok:rest) = getStoreDefaultList list -- v := list 4
getStatement list@(VarTok var:AssignTok:OpenSqTok:rest) = getStoreListStatement list  -- v := [
getStatement list@(VarTok var:AssignTok:rest) = getStoreStatement list                -- x := 
getStatement list@(VarTok var:DollarTok:rest) = getUpdateVectorStatement list         -- x$
getStatement list@(VarTok var:AssignPlusTok:rest) = getIncrPlusStatement list     -- x +=  
getStatement list@(VarTok var:AssignSubTok:rest) = getIncrMinusStatement list   -- x -= 
getStatement list@(VarTok var:AssignProdTok:rest) = getIncrMultStatement list    -- x *= 
getStatement list@(ForTok:rest) = getForStatement list                                -- for 
getStatement list@(OpenTok:rest) = (stm, rest)                                        -- (
  where (stm, SemiColonTok:rest) = getParenthStatement list                           -- TODO se nao tiver ; ele vai dar erro de haskell
getStatement _ = error "Invalid token used as the beginning of new statement"

{-|
    Builds a valid 'Compile.WhileStm' with the next tokens that appear in the token list given as input

    This function handles the casa where the 'Compile.WhileStm' is built from a 'Lexer.ForTok'
-}
getForStatement :: [Token] -> (Stm, [Token])
getForStatement (ForTok:OpenTok:rest) = (ParenthStm [storeStm, WhileStm bexp (ParenthStm [stms, stepStm])], restTokens)
  where (storeStm, restTokens1) = getStoreStatement rest
        (bexp, SemiColonTok:restTokens2) = getBexp restTokens1
        (stepStm, CloseTok:DoTok:restTokens3) = case restTokens2 of
            (VarTok var:AssignTok:rest) -> getStoreStatement restTokens2
            (VarTok var:AssignPlusTok:rest) -> getIncrPlusStatement restTokens2
            (VarTok var:AssignSubTok:rest) -> getIncrMinusStatement restTokens2
            (VarTok var:AssignProdTok:rest) ->  getIncrMultStatement restTokens2
        (stms, restTokens) = getStatement restTokens3

{-|
    Builds a valid 'Compile.ParenthStm' with the next tokens that appear in the token list given as input,
    verifying that the next token that is parsed in a 'Lexer.OpenTok'
-}
getParenthStatement :: [Token] -> (Stm, [Token])
getParenthStatement (OpenTok:rest) = (ParenthStm stms, restTokens)
 where (stms, restTokens) = getStatement' rest

{-|
    Builds a list of valid statements with the next tokens that appear in the token list given as input, until
    a 'Lexer.CloseTok' is found. This fetches all statements that are between a pair of correctly matched parentheses.
-}
getStatement'  :: [Token] -> ([Stm], [Token])
getStatement' l@(CloseTok:rest) = ([], rest)
getStatement' [] = error "Syntax error: unmatch parentheses."
getStatement' l = (fst (getStatement l) : stms , rest)
 where (stms, rest) =  getStatement' (snd (getStatement l))

{-|
    Builds a valid 'Compile.StoreStmA' or 'Compile.StoreStmB' with the next tokens that appear in the token list given as input.
    The parser identifies if the statement assigns a boolean or integer value to the chosen variable.
-}
getStoreStatement :: [Token] -> (Stm, [Token])
getStoreStatement (VarTok var:AssignTok:rest) =
  if isBool rest then (case getBexp rest of
           (bexp, SemiColonTok:restTokens) -> (StoreStmB var bexp, restTokens)) else (case getAexp rest of
           (aexp, SemiColonTok:restTokens) -> (StoreStmA var aexp, restTokens)
           _ -> error "Syntax error: Assign value.")

{-|
    Builds a valid 'Compile.StoreStmA' with the next tokens that appear in the token list given as input.
    This effectively stores the sum between the value of the variable of the next token in the list and an 
    arithmetic expression.
-}
getIncrPlusStatement :: [Token] -> (Stm, [Token])
getIncrPlusStatement list@(VarTok var:AssignPlusTok:rest) = 
  case getAexp rest of
    (aexp, SemiColonTok:restTokens) -> (StoreStmA var (AddExp (VarLitA var) aexp), restTokens)

{-|
    Builds a valid 'Compile.StoreStmA' with the next tokens that appear in the token list given as input.
    This effectively stores the subtraction between the value of the variable of the next token in the list 
    and an arithmetic expression.
-}
getIncrMultStatement :: [Token] -> (Stm, [Token])
getIncrMultStatement list@(VarTok var:AssignProdTok:rest) = 
  case getAexp rest of
    (aexp, SemiColonTok:restTokens) -> (StoreStmA var (MultExp (VarLitA var) aexp), restTokens)

{-|
    Builds a valid 'Compile.StoreStmA' with the next tokens that appear in the token list given as input.
    This effectively stores the product between the value of the variable of the next token in the list and 
    an arithmetic expression.
-}
getIncrMinusStatement :: [Token] -> (Stm, [Token])
getIncrMinusStatement list@(VarTok var:AssignSubTok:rest) = 
  case getAexp rest of
    (aexp, SemiColonTok:restTokens) -> (StoreStmA var (SubExp (VarLitA var) aexp), restTokens)


{-|
    Verifies if a token that represents a boolean constant or a boolean operator is found before the next
    'Lexer.SemiColonTok' is found in the next tokens that appear in the token list given as input. This function
    is used by 'getStoreStatement' to verify if a store statement assign a boolean or integer value to a certain variable.
-}
isBool :: [Token] -> Bool
isBool (SemiColonTok:r) = False
isBool (x:xs) = elem x [BoolTok True, BoolTok False, AndTok, BoolEqTok, IntEqTok, LeTok, NotTok] || isBool xs

{-|
    Builds a valid 'Compile.ParenthStm' with the next tokens that appear in the token list given as input,
    containing a sequence of statements to store a list full of zeros in a variable.
-}
getStoreDefaultList :: [Token] -> (Stm, [Token])
getStoreDefaultList (VarTok var:AssignTok:ListTok:(IntTok size):SemiColonTok:rest) = (ParenthStm (buildDefaultList var size), rest)

{-|
    Builds the needed statements to store a list with a certain length given as input in a variable whose name
    is also specified in the input.
-}
buildDefaultList :: String -> Integer -> [Stm]
buildDefaultList _ 0 = []
buildDefaultList prefix index = StoreStmA varName (IntLit 0) : buildDefaultList prefix (index - 1)
  where varName = prefix ++ "$" ++ show (index - 1)

{-|
    Builds a valid 'Compile.ParenthStm' with the next tokens that appear in the token list given as input,
    containing a sequence of statements to store a list with integer values in a variable.
-}
getStoreListStatement :: [Token] -> (Stm, [Token])
getStoreListStatement list@(VarTok var:AssignTok:OpenSqTok:rest) = (ParenthStm stms, restTokens)
 where
  (stms, restTokens) = buildStoreStatements rest var 0

{-|
    Builds the needed statements to store a list with arithmetic expression in a variable whose name
    is given as input.
-}
buildStoreStatements :: [Token] -> String -> Int -> ([Stm],[Token])
buildStoreStatements tokens prefix index = 
  case getAexp tokens of
    (exp1, CloseSqTok:SemiColonTok:restTokens) -> ([StoreStmA varName exp1], restTokens)
    (exp1, CommaTok:restTokens1) -> let (stms, restTokens) = buildStoreStatements restTokens1 prefix (index + 1)
                                    in (StoreStmA varName exp1 : stms , restTokens)
    _ -> error "Invalid list syntax"
  where
    varName = prefix ++ "$" ++ show index

{-|
    Builds a valid 'Compile.IfStm' with the next tokens that appear in the token list given as input.
-}
getIfStatement :: [Token] -> (Stm, [Token])
getIfStatement (IfTok:rest) = (IfStm bexp thenStm elseStm, restTokens)
 where
  (bexp, ThenTok:restTokens1) = getBexp rest
  (thenStm, ElseTok:restTokens2) = if head restTokens1 == OpenTok
                                    then getParenthStatement restTokens1
                                   else getStatement restTokens1
  (elseStm, restTokens) = getStatement restTokens2

{-|
    Builds a valid 'Compile.WhileStm' with the next tokens that appear in the token list given as input.

    This function handles the casa where the 'Compile.WhileStm' is built from a 'Lexer.WhileTok'.
-}
getWhileStatement :: [Token] -> (Stm, [Token])
getWhileStatement (WhileTok:rest) = (WhileStm bexp stm, restTokens)
 where
  (bexp, DoTok:restTokens1) = getBexp rest
  (stm, restTokens) = getStatement restTokens1

{-|
    Builds a valid 'Compile.VarlitAVecorStore' to update the contents of a list with the next
    tokens that appear in the token list given as input
-}
getUpdateVectorStatement :: [Token] -> (Stm, [Token])
getUpdateVectorStatement list@(VarTok var:DollarTok:rest) = 
  case getAexp rest of 
    (index, AssignTok : restTokens1) -> 
      case getAexp restTokens1 of
        (valor, SemiColonTok: restTokens) -> (VarlitAVecorStore valor index (var++"$"), restTokens)

{-|
    Builds a valid 'Compile.Aexp' with the next tokens that appear in the token list given as input.

    This translations takes the precedence of parenthesis and of the arithmetic operators used in the expression into account.
-}
getAexp :: [Token] -> (Aexp, [Token])
getAexp tokens =
  case parseSumSub tokens of
    Just (aExp, tokens) -> (aExp, tokens)
    _                   -> error "Syntax error: Invalid arithmetic expression"

{-|
    Builds a valid 'Compile.AddExp' or 'Compile.SubExp' with the next tokens that appear in the token list given as input.
-}
parseSumSub :: [Token] ->  Maybe (Aexp, [Token])
parseSumSub tokens =
  case parseProd tokens of
    Just (exp1, PlusTok:restTokens1) ->
      case parseSumSub restTokens1 of
        Just (exp2, restTokens2) -> Just (AddExp exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    Just (exp1, MinusTok: restTokens1) ->
      case parseSumSub restTokens1 of
        Just (exp2, restTokens2) -> Just (SubExp exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    result -> result

{-|
    Builds a valid 'Compile.MultExp' with the next tokens that appear in the token list given as input.
-}
parseProd :: [Token] ->  Maybe (Aexp, [Token])
parseProd tokens =
  case parseIntVarPar tokens of
    Just (exp1, TimesTok: restTokens1) ->
      case parseProd restTokens1 of
        Just (exp2, restTokens2) -> Just (MultExp exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    other -> other

{-|
    Builds the correct statement to either fetch a value from a list, or parse a variable or an integer

    This functions also handles the use of parenthesis in arithmetic expressions.
-}
parseIntVarPar :: [Token] -> Maybe (Aexp, [Token])
parseIntVarPar (IntTok n: restTokens) = Just (IntLit n, restTokens)

parseIntVarPar (VarTok v: DollarTok : VarTok index:tokens) = 
  Just (VarlitAVecorFetch (VarLitA index) (v ++ "$"), tokens)

parseIntVarPar (VarTok v: DollarTok : IntTok index:tokens) = 
  Just (VarlitAVecorFetch (IntLit index) (v ++ "$"), tokens)

parseIntVarPar (VarTok v: DollarTok : OpenTok : tokens) = case getAexp tokens of 
  (aexp, CloseTok : restTokens) -> Just (VarlitAVecorFetch aexp (v ++ "$"), restTokens)
  _ -> Nothing

parseIntVarPar (VarTok v: tokens) = Just (VarLitA v, tokens)
parseIntVarPar (OpenTok:tokens) =
  case parseSumSub tokens of
    Just (exp1, CloseTok:restTokens1) -> Just (exp1, restTokens1)
    _ -> Nothing
parseIntVarPar _ = Nothing

{-|
    Builds a valid 'Compile.Bexp' with the next tokens that appear in the token list given as input.

    This translations takes the precedence of parenthesis and of the boolean operators used in the expression into account.
-}
getBexp :: [Token] -> (Bexp, [Token])
getBexp tokens =
  case parseAnd tokens of
    Just (bExp, tokens) -> (bExp, tokens)
    _                   -> error "Syntax error: Boolean expression."


{-|
    Builds a valid 'Compile.AndExp' with the next tokens that appear in the token list given as input.
-}
parseAnd :: [Token] ->  Maybe (Bexp, [Token])
parseAnd tokens =
  case parseBoolEq tokens of
    Just (exp1, AndTok:restTokens1) ->
      case parseAnd restTokens1 of
        Just (exp2, restTokens2) -> Just (AndExp exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    result -> result


{-|
    Builds a valid 'Compile.EquExpBool' with the next tokens that appear in the token list given as input.
-}
parseBoolEq :: [Token] ->  Maybe (Bexp, [Token])
parseBoolEq tokens =
  case parseNot tokens of
    Just (exp1, BoolEqTok:restTokens1) ->
      case parseBoolEq restTokens1 of
        Just (exp2, restTokens2) -> Just (EquExpBool exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    result -> result

{-|
    Builds a valid 'Compile.NegExp' with the next tokens that appear in the token list given as input.

    This function allows the chaining of "not" statements without the use of parenthesis
    such as "not not True" which evaluates to True.
-}
parseNot :: [Token] ->  Maybe (Bexp, [Token])
parseNot (NotTok:restTokens) =
  case parseNot restTokens of
    Just (exp1, restTokens1) -> Just (NegExp exp1, restTokens1)
    result -> result

parseNot tokens =
  case parseIntEqLe tokens of
    Just (exp1, restTokens1) -> Just (exp1, restTokens1)
    Nothing -> case parseBoolVarPars tokens of
      Just (exp2, restTokens2) -> Just (exp2, restTokens2)
      Nothing -> Nothing

{-|
    Builds a valid 'Compile.EquExpInt' or 'Compile.LeExp' with the next tokens that appear in the token list given as input.
-}
parseIntEqLe :: [Token] ->  Maybe (Bexp, [Token])
parseIntEqLe tokens =
  case parseSumSub tokens of
    Just (exp1, IntEqTok:restTokens1) ->
      case parseSumSub restTokens1 of
        Just (exp2, restTokens2) -> Just (EquExpInt exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    Just (exp1, LeTok:restTokens1) ->
      case parseSumSub restTokens1 of
        Just (exp2, restTokens2) -> Just (LeExp exp1 exp2, restTokens2)
        Nothing                  -> Nothing
    _ -> Nothing


{-|
    Builds a valid 'Compile.BoolLit' or 'Compile.VarLitB' with the next tokens that appear in the token list given as input.

    This functions also handles the use of parenthesis in boolean expressions.
-}
parseBoolVarPars :: [Token] -> Maybe (Bexp, [Token])
parseBoolVarPars (BoolTok n: restTokens) = Just (BoolLit n, restTokens)
parseBoolVarPars (VarTok v: restTokens) = Just (VarLitB v, restTokens)
parseBoolVarPars (OpenTok:restTokens1) =
  case parseAnd restTokens1 of
    Just (exp1, CloseTok:restTokens2) -> Just (exp1, restTokens2)
    _ -> Nothing
parseBoolVarPars _ = Nothing


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


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)


main :: IO ()
main = do
    runTests (zip [1..] testCasesAssembler) testAssembler
    runTests (zip [1..] testCasesParser) testParser
