module Test where

import Compile
import GHC.Arr (done)

testCasesAssembler :: [(Code, (String, String))]
testCasesAssembler = [
    ([Push 10,Push 4,Push 3,Sub,Mult], ("-10","")),
    ([Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"], ("","a=3,someVar=False,var=True")),
    ([Fals,Store "var",Fetch "var"], ("False","var=False")),
    ([Push (-20),Tru,Fals], ("False,True,-20","")),
    ([Push (-20),Tru,Tru,Neg], ("False,True,-20","")),
    ([Push (-20),Tru,Tru,Neg,Equ], ("False,-20","")),
    ([Push (-20),Push (-20),Equ], ("True","")),
    ([Push (-20),Push 10,Equ], ("False","")),
    ([Push (-20),Push (-21), Le], ("True","")),
    ([Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"], ("","x=4")),
    ([Push 10, Store "i", Push 1, Store "fact", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "fact", Mult, Store "fact", Push 1, Fetch "i", Sub, Store "i"]], ("","fact=3628800,i=1")),
    ([Push 10,Push 2,Tru,Branch [Add] [Sub]], ("12","")),
    ([Push 10,Push 2,Fals,Branch [Add] [Sub]], ("-8","")),
    ([Push 123, Store "vector$2", Push 2, Fetch "vector$"], ("123","vector$2=123"))
    ]

testCasesParser :: [(String, (String, String))]
testCasesParser = [
    ("x := 5; x := x - 1;" , ("","x=4")),
    ( "x := 0 - 2;" , ("","x=-2")),
    ( "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" , ("","y=2")),
    ( "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" , ("","x=1")),
    ( "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" , ("","x=2")),
    ( "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" , ("","x=2,z=4")),
    ( "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" , ("","x=34,y=68")),
    ( "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" , ("","x=34")),
    ( "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" , ("","x=1")),
    ( "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" , ("","x=2")),
    ( "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" , ("","x=2,y=-10,z=6")),
    ( "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" , ("","fact=3628800,i=1")),
    ( "x := 12; y := 10; if (True) then ( x := 5; if (True) then y := 5; else z := 10;) else z := 13;", ("","x=5,y=5")),
    ( "if not not True then x :=1; else x:=2;", ("", "x=1")),
    ( "if not (not True) then x :=1; else x:=2;", ("", "x=1")),
    ( "x := 2; x += 2 + 3 * 2;", ("", "x=10")),
    ( "x := 2; x -= 2 + 3 * 2;", ("", "x=-6")),
    ( "x := 2; x *= 2 + 3 * 2;", ("", "x=16"))
    ]

runTest :: (Show b, Eq b) => (Int, (c, b)) -> (c -> b) -> IO ()
runTest (index, (input, expected)) testFunction
 | result == expected = return ()
 | otherwise = putStrLn $ "Test " ++ show index ++ " failed. Expected " ++ show expected ++ " but got " ++ show result
 where result = testFunction input


runTests :: (Show b, Eq b) => [(Int, (c, b))] -> (c -> b) -> IO ()
runTests [] _ = putStrLn "Tests passed."
runTests (test:rest) testFunction = do
    runTest test testFunction
    runTests rest testFunction


-- Examples:
-- testAssembler [Push 1,Push 2,And]                      "Run-time error"
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]       "Run-time error"
-- testAssembler [Push 10,Push 2,Branch [Add] [Sub]]      "Run-time error"
-- testAssembler [Tru,Fals,Neg,Add]                       "Run-time error"
-- testAssembler [Tru,Push 2,Equ]                         "Run-time error"




program1 :: String
program1 = " list := [1,2,3,4,5,6,7,8,9,10];                    \
          \  sum := 0;                                          \
          \  for (index := 0; index <= 9; index += 1;)          \
          \                                                     \
          \  do(                                                \
          \      sum += list$index;                             \
          \  );"

