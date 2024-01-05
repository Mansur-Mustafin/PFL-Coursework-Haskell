{-|
Module      : Pilha
Description : Stack Implementation in Haskell
Copyright   : Daniel Ferreira and Mansur Mustafin, 2023
Maintainer  : unidsferreira2003@gmail.com and mustafin.mansur02@gmail.com

This module 'Pilha' provides a generic stack (LIFO) data structure.
It allows users to perform basic operations.

The module also includes specialized functions for handling stacks containing 'Either' values. 
'get2RightValues' and 'get2LeftValues'.
-}

module Pilha (
      -- * Type
      Pilha,
      -- * Construction
      empty,
      -- * Operations
      push, pop, top, get2RightValues, get2LeftValues,
      -- * Query Functions
      isEmpty
) where

{-|
    Type 'Pilha' represents a stack. It is a wrapper around a list.
-}
data Pilha a = Stk [a] 
               deriving Show


{-|
    Creates an empty stack.
-}
empty :: Pilha a
empty = Stk []


{-|
    Checks if a stack is empty.
-}
isEmpty :: Pilha a -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False


{-|
    Adds a new element to the top of the stack.
-}
push :: a -> Pilha a -> Pilha a 
push x (Stk xs) = Stk (x:xs)


{-|
    Removes the top element from the stack.

    __Throws:__ 'error' if the stack is empty.
-}
pop :: Pilha a -> Pilha a
pop (Stk (_:xs)) = Stk xs
pop (Stk []) = error "Pilha.pop: empty stack"


{-|
    Returns the top element of the stack.
-}
top :: Pilha a -> a
top (Stk (x:_)) = x
top (Stk _) = error "Pilha.top: empty stack"


{-|
    Retrieves two consecutive 'Right' values from the top of the stack if available.

    Returns a tuple of the two values and the remaining stack.

    __Throws:__ 'error' if there aren't two 'Right' values at the top of the stack.
-}
get2RightValues :: Pilha (Either a b) -> (b, b, Pilha (Either a b))
get2RightValues (Stk (Right v1:Right v2:xs)) = (v1, v2, Stk xs)
get2RightValues (Stk _) = error "Run-time error"


{-|
    Retrieves two consecutive 'Left' values from the top of the stack if available.

    Returns a tuple of the two values and the remaining stack.
    
    __Throws:__ 'error' if there aren't two 'Left' values at the top of the stack.
-}
get2LeftValues :: Pilha (Either a b) -> (a, a, Pilha (Either a b))
get2LeftValues (Stk (Left v1:Left v2:xs)) = (v1, v2, Stk xs)
get2LeftValues (Stk _) = error "Run-time error"
