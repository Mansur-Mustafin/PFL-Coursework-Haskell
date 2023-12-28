{-|
Module      : Pilha
Description : Stack Implementation in Haskell
Copyright   : Who?
Maintainer  : who@gmail.com

This module 'Pilha' provides a generic stack (LIFO) data structure.
It allows users to perform basic operations.

The module also includes specialized functions for handling stacks containing 'Either' values. 
'get2RightValues' and 'get2LeftValues'.
-}

module Pilha (
      -- Export of type
      Pilha, 
      -- Export of operations
      push, pop, top, get2RightValues, get2LeftValues,
      empty, isEmpty) where

{-|
    'Pilha' type is represents a Stack. Is a wrapper around a list.
-}
data Pilha a = Stk [a] deriving Show

{-|
    'empty'
    Description: Yields a empty stack
-}
empty :: Pilha a
empty = Stk []

{-|
    'isEmpty' 
    Description: checks if a stack is empty. 
-}
isEmpty :: Pilha a -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False

{-|
    'push'
    Description: adds a new element to the top of the stack.
-}
push :: a -> Pilha a -> Pilha a 
push x (Stk xs) = Stk (x:xs)

{-|
    'pop'
    Description: removes the top element from the stack.
-}
pop :: Pilha a -> Pilha a
pop (Stk (_:xs)) = Stk xs
pop (Stk []) = error "Pilha.pop: empty stack"

{-|
    'top'
    Description: returns the top element of the stack.
-}
top :: Pilha a -> a
top (Stk (x:_)) = x
top (Stk _) = error "Pilha.top: empty stack"

{-|
    'get2RightValues'
    Description: retrieves two consecutive 'Right' values from 
    the top of the stack if available.
    Return: tuple of the two values and the remaining stack.
-}
get2RightValues :: Pilha (Either a b) -> (b, b, Pilha (Either a b))
get2RightValues (Stk (Right v1:Right v2:xs)) = (v1, v2, Stk xs)
get2RightValues (Stk _) = error "Run-time error"

{-|
    'get2LeftValues'
    Description: retrieves two consecutive 'Left' values from 
    the top of the stack if available.
    Return: tuple of the two values and the remaining stack.
-}
get2LeftValues :: Pilha (Either a b) -> (a, a, Pilha (Either a b))
get2LeftValues (Stk (Left v1:Left v2:xs)) = (v1, v2, Stk xs)
get2LeftValues (Stk _) = error "Run-time error"