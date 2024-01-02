# PFL-Practical Assignment 2

## Group : T01_G11
* Members:
  * [Daniel dos Santos Ferreira](https://github.com/dsantosferreira) - up202108771 (50%)
  * [Mansur Mustafin](https://github.com/Mansur-Mustafin) - up202102355 (50%)

## Problem description

This project was divided in two parts:

- **Part 1 (low-level machine)** - our goal was to implement a low-level machine with a configuration of the form (*c*, *e*, *s*), where *c* is a list of defined instructions to run, *e* is the evaluation stack which holds integer and boolean values to be evaluated, and *s* is the machine's storage which stores integer and boolean values which are identified by the variable's name;
- **Part 2 (parser and compiler)** - the objective of this part was to implement a parser and compiler capable of translating strings containing a program written in a small imperative programming language that followed a defined set of rules, into a set of instructions that could be run by the low-level machine developed in the first part of the project.

## Our solution

In this section we will describe the decisions we made to define the data types and functions necessary to develop both parts of the project

### Part 1: Low-level machine

#### Data types

- The machine's stack **Stack**: to define the machine's stack we used the stack data structure, which holds integer and boolean values, with the following definition: `type Stack = (Pilha (Either Bool Integer))`. It supports the usual stack operations (popping and pushing a value from/into the stack, inspecting the top of the stack, creating an empty stack and verifying if the stack is empty). This data structure was the obvious choice as it perfectly mimics a machine's stack;

- The machine's state **State**: to define the machine's state we used a self-balancing binary search tree data structure, which stores pairs containing a string (the variable's name) and either an integer or a boolean value (the variable's value), with the following definition: `type State = (Map.Map String (Either Bool Integer))`. The pairs are ordered by the pair's string so that the value of a certain variable, given its name, can be found efficiently. Our implementation supports build an empty tree, checking if the tree is empty and finding and inserting a value in the tree. It doesn't support removing elements from the tree, since the low-level machine that was implemented doesn't support that action.

#### Main functions

- **createEmptyStack** and **createEmptyState**: in these functions we had to be able to create an empty stack and state for the machine. For that we simply had to call the `empty` function from the respective data structures that we implemented;

- **stack2Str** and **state2Str**: in these functions we had to represent the contents of the machine's stack and state in a string. The contents of the stack had to be ordered with the leftmost value representing the top of the stack while the contents of the state had to be ordered in alphabetical order of the variable name. The implementation of **stack2Str** was simple since appending the value from the top of the stack to the final string as the stack is being emptied gives us the correct order. For **state2Str** we performed an inorder traversal of the binary search tree which gives us the variables ordered in alphabetical order;

- **run**: for this function we had to actually run the machine instructions that are held in a list of instructions. To do so we used pattern matching to identify the next instruction and updated the machine's stack and state as described in the project's specification for each instruction. Notably, for `Fetch` and `Store` instructions, special attention is given to variables marked with a **$** symbol. This symbol indicates the involvement of list indexing, wherein the instruction anticipates the topmost stack element to be the index of a list item that the program needs to access or modify.

To test our implementation we ran all of the tests that were given to us in the template file.

### Part 2: Parser and compiler

#### Data types

- Arithmetic expressions **Aexp**: we defined an arithmetic expression as either an integer, a variable storing an integer in the machine's state, the addition of two arithmetic expressions, the subtraction of two arithmetic expressions or the product of two arithmetic expressions. Integers and variables storing integers are considered the terminal symbols of this grammar. The precedence of operators is handled by the parser and not by the compiler which will already receive the arithmetic statements correctly ordered in regards to its precedence. As an example, the expression `2 + x * 5` will be represented (after parsing) as `AddExp (IntLit 2) (Mult (VarLitA "x") (IntLit 5))`.

- Boolean expressions **Bexp**: we defined a boolean expression as either a boolean constant (True or False), a variable storing a boolean in the machine's state, the *and* logical operation between two boolean expressions, the *not* logical operation between two boolean expressions, the equality between two boolean expressions, the equality between two arithmetic expressions and the *less than* operation between two arithmetic expressions. Boolean constants and variables storing boolean values are considered the terminal symbols of this grammar. The precedence of operators is handled by the parser and not by the compiler which will already receive the boolean statements correctly ordered in regards to its precedence. As an example, the expression `2 <= 4 and True = False` will be represented (after parsing) as `AndExp (LeExp (IntLit 2) (IntLit 4)) (EquExpBool (BoolLit True) (BoolLit False))`.

- Statements **Stm**: we defined a program statement as either the storage of the result of an arithmetic or boolean expression in a variable, the *if-then-else* statement which needs a boolean expression to check which of the statements the machine should execute (either the *then* or *else* block), the *while-do* statement which also needs a boolean expression to check if the statement in the *do* block should be executed and finally a sequence of statements inside parentheses which we named **ParenthStm**. The store statements are considered the terminal symbols of this grammar. As an example, the expression `if True then x := 2 else x := 3;` will be represented (after parsing) as `IfStm (BoolLit True) (StoreStmA "x" (IntLit 2)) (StoreStmA "x" (IntLit 3))`.

#### Main functions

- **compile**, **compA** and **compB**: these functions are the ones responsible for translating the statements described above into machine instructions that can be interpreted by the low-level machine we implemented in the first part. We handled arithmetic and boolean statements translation in **compA** and **compB** respectively. The statements were handled in **compile** where **compA** and **compB** were called as auxiliary functions. To implement this function we followed the same strategy as the **run** function, that is, we used pattern matching to indentify the next statement to be translated and built the intruction list according to the specification of the first part of the project. That is, the instructions to push the necessary values into the evaluation stack appear before the operation itself.

- **parse**: this function handles the translation of a string containing a program into a list of valid statements, so that the compiler can translate those statements into the final list of instructions. To do so we divided this functionality in two seperate functions and logical parts:
  - The first one, the **lexer**, translates the string into a list of tokens, such as a token of the '+' sign or an integer, removing all white space between tokens. If an invalid token is found, such as a variable name that starts with an uppercased letter "Variable", it also throws an error. As an example, the string `"if True then (x := 2 + 3; y := 2;) else z := 1;"` produces the following list of tokens `[IfTok,BoolTok True,ThenTok,OpenTok,VarTok "x",AssignTok,IntTok 2,PlusTok,IntTok 3,SemiColonTok,VarTok "y",AssignTok,IntTok 2,SemiColonTok,CloseTok,ElseTok,VarTok "z",AssignTok,IntTok 1,SemiColonTok]`. The name of each token starts with the name of the symbol or operator followed by the suffix "Tok".
  - The second part - **buildData** uses the list of tokens mentioned above and builds a list of statements to be used by the compiler. It also takes the precedence of operators into account so that the compiler doens't have to. To implement this we firstly created several helper functions to parse arithmetic and boolean expressions, taking the precedence of operators into account. That is, when trying to parse a sum, we firstly check if there is an operation with higher precedence to the left and to the right of the '+' sign, so that those operations are parsed before the sum. That way, operators with higher precedence and parentheses are parsed before operators with lower precedence. With this, for each token we find, we try to match it with one of the possible statements from the Stm data type and parse the following tokens depending on the statement that is expected. As an example, the list of tokens above would produce the following list of statements `[IfStm (BoolLit True) (ParenthStm [StoreStmA "x" (AddExp (IntLit 2) (IntLit 3)),StoreStmA "y" (IntLit 2)]) (StoreStmA "z" (IntLit 1))]`.

## Extra features

In this project, we decided to implement a set of extra features which can be found in the enhanced_src folder:

- **AVL tree**: instead of using the tree in the /src folder, which can become unbalanced pretty easily, we implemented an AVL tree so the storage of variables becomes more efficient;
- **For cycles**: we implemented the use of for cycles so that programs with the following syntax `z := 0; for (i := 0; i <= 2; i := i + 1) do z := z + 1;` can be run on our machine. In this example the evaluation stack and state will have the following contents `("","i=3,z=3")`. To implement this we simply worked on the parser to identify the syntax we gave to *for loops* and translated those tokens into a while statement;
- **List syntax**: we also allow users to use lists with integers in our program `a := [1, 1+1, 3, 4]; b := a$1 + a$2 + 5;`. For example, the first element in the list is "a$0". In this example the evaluation stack will have the following contents `("","a$0=1,a$1=2,a$2=3,a$3=4,b=10")`. We represented a list in the machine's storage as different variables with the following name "variable_name$index". A value from the list can be accessed by using the variable's name followed by a dollar sigh '$' and the index of the value, where index could be any aritmetic expression. In addition to that the user can create a list full of 0s with a certain length with the following syntax `a := list 4`, which will store a list of 4 zeros `("","a$0=0,a$1=0,a$2=0,a$3=0")`;
- **New assignment operations**: we implemented the addition assignment "+=", subtraction assignment "-=" and product assignment "*=" operators to achieve the following syntax `"z := 0; for (i := 0; i <= 2; i += 1) do z := z + 1;"`. However, these operators don't work with a list element on left side.

## Program examples:

* Sum of the list:
```haskell
program1 :: String
program1 = " my_list := [1,2,3,4,5,6,7,8,9,10];                 \
          \  sum := 0;                                          \
          \  for (index := 0; index <= 9; index += 1;)          \
          \                                                     \
          \  do(                                                \
          \      sum += my_list$index;                          \
          \  );"
```
Output : `sum=55`

* Check if list is sorted:
```haskell
program2 :: String
program2 = " my_list := [1,2,3,6,4,5];                          \
          \                                                     \
          \  isOrdered := True;                                 \
          \  for (index := 0; index <= 4; index += 1;)          \
          \                                                     \
          \  do(                                                \
          \      if (not my_list$index <= my_list$(index + 1))  \
          \       then isOrdered := False;                      \
          \      else ();                                       \
          \  );"
```
Output : `isOrdered=False`


## Documentation

Documentation can be found on top of every function and data type throughout the project. We also generated documentation that can be visualized in a web browser using **Haddock**. To do so, open the file **index.html** in the **docs** folder in a web browser. You can also manually input the url with the following structure `file://path/to/index.html`.
The web documentation provided features the documentation of the version with extra features.