{-|
  Module      : Compile
  Description : Set of functions responsible for translating a list of statements into a list of valid instructions, acting as the program's compiler
  Copyright   : Daniel Ferreira and Mansur Mustafin, 2023
  Maintainer  : unidsferreira2003@gmail.com and mustafin.mansur02@gmail.com

  This module acts as the program's compiler holding all functions necessary to transform any statement,
  arithmetic expression or boolean expression into the respective valid set of instructions
-}

module Compile where

-- | Data type that holds all valid instructions
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

-- | Data type that holds all possible representations of an arithmetic expression
data Aexp
  = IntLit Integer -- ^ Represents an integer literal, e.g. 1
  | VarLitA String -- ^ Represents a variable in an arithmetic expression, e.g. 1 + **x**
  | AddExp Aexp Aexp -- ^ Represents an addition between two arithmetic expressions, e.g. 1 + 2
  | SubExp Aexp Aexp -- ^ Represents a subtraction between two arithmetic expressions, e.g. 1 - 2
  | MultExp Aexp Aexp -- ^ Represents a product between two arithmetic expressions, e.g. 1 * 2
  deriving (Show) 

-- | Data type that holds all possible representations of an boolean expression
data Bexp
  = BoolLit Bool -- ^ Represents a boolean literal, e.g. True
  | VarLitB String -- ^ Represents a variable in a boolean expression, e.g. 1 and **x**
  | AndExp Bexp Bexp -- ^ Represents the **and** logical operation between two boolean expressions, e.g. True and False
  | NegExp Bexp -- ^ Represents the **not** logical operation between two boolean expressions, e.g. neg True
  | EquExpBool Bexp Bexp -- ^ Represents the equality between two boolean expression, e.g. True == False
  | EquExpInt Aexp Aexp -- ^ Represents the equality between two arithmetic expression, e.g. 1 + 2 == 3
  | LeExp Aexp Aexp -- ^ Represents the **less than** operation between two arithmetic expressions, e.g. 2 <= 3
  deriving (Show) 

-- | Data type that holds all valid program statements
data Stm
  = StoreStmA String Aexp -- ^ Represents an integer assignment, e.g. x := 1 + 2
  | StoreStmB String Bexp -- ^ Represents a boolean assignment, e.g. x := True and False
  | ParenthStm [Stm] -- ^ Represents a chain of statements between parentheses, e.g. (x := 2; y := True and False)
  | IfStm Bexp Stm Stm -- ^ Represents an if-then-else statement, e.g. if True then x := 2 else x := 3
  | WhileStm Bexp Stm -- ^ Represents a while statement, e.g. while True do x := x + 1
  deriving (Show) 

-- Defines a program as a list of statements
type Program = [Stm]

-- Defines the program's code as a list of instructions
type Code = [Inst]

-- Translates a list of statements into the respective valid list of instructions
compile :: Program -> Code
compile prog = concatMap compileStm prog

-- Translates a statement from 'Stm' into the respective valid list of instructions
compileStm :: Stm -> Code
compileStm (StoreStmA var aexp) = compA aexp ++ [Store var]
compileStm (StoreStmB var bexp) = compB bexp ++ [Store var]
compileStm (ParenthStm stms) = compile stms
compileStm (IfStm bexp stm1 stm2) = compB bexp ++ [Branch (compileStm stm1) (compileStm stm2)]
compileStm (WhileStm bexp stm) = [Loop (compB bexp) (compileStm stm)]

-- Translates an arithmetic expression from 'Aexp' into the respective valid list of instructions
compA :: Aexp -> Code
compA (IntLit n) = [Push n]
compA (VarLitA var) = [Fetch var]
compA (AddExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Add]
compA (SubExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Sub]
compA (MultExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Mult]

-- Translates a boolean expression from 'Bexp' into the respective valid list of instructions
compB :: Bexp -> Code
compB (BoolLit val) = if val then [Tru] else [Fals]
compB (VarLitB var) = [Fetch var]
compB (AndExp bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [And]
compB (NegExp bexp) = compB bexp ++ [Neg]
compB (EquExpInt aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Equ]
compB (EquExpBool bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [Equ]
compB (LeExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]

