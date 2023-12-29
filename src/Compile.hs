module Compile where

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

data Aexp
  = IntLit Integer
  | VarLitA String
  | AddExp Aexp Aexp
  | SubExp Aexp Aexp
  | MultExp Aexp Aexp
  deriving (Show) 

data Bexp
  = BoolLit Bool
  | VarLitB String
  | AndExp Bexp Bexp
  | NegExp Bexp
  | EquExpBool Bexp Bexp
  | EquExpInt Aexp Aexp
  | LeExp Aexp Aexp
  deriving (Show) 

data Stm
  = StoreStmA String Aexp
  | ParenthStm [Stm]    -- Check if this is necessary when we start doing parsing
  | IfStm Bexp Stm Stm 
  | WhileStm Bexp Stm
  deriving (Show) 

type Program = [Stm]
type Code = [Inst]

-- TODO: Define the types Aexp, Bexp, Stm and Program
compile :: Program -> Code
compile prog = concatMap compileStm prog

compileStm :: Stm -> Code
compileStm (StoreStmA var aexp) = compA aexp ++ [Store var]
compileStm (ParenthStm stms) = concatMap compileStm stms
compileStm (IfStm bexp stm1 stm2) = compB bexp ++ [Branch (compileStm stm1) (compileStm stm2)]
compileStm (WhileStm bexp stm) = [Loop (compB bexp) (compileStm stm)]

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

