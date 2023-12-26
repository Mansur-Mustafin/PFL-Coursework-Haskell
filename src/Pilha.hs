module Pilha (Pilha,                                         -- exportar o tipo
              push, pop, top, get2RightInt, get2LeftString,  -- e as operações
              empty, isEmpty, topIsLeft, topIsRight) where

import Data.Either (isLeft, isRight)

data Pilha a = Stk [a] deriving Show

push :: a -> Pilha a -> Pilha a 
push x (Stk xs) = Stk (x:xs)

pop :: Pilha a -> Pilha a
pop (Stk (_:xs)) = Stk xs
pop (Stk []) = error "Pilha.pop: empty stack"

top :: Pilha a -> a
top (Stk (x:_)) = x
top _ = error "Pilha.top: empty stack"

empty :: Pilha a
empty = Stk []

isEmpty :: Pilha a -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False

-- aux functions
get2RightInt :: Pilha (Either String Integer) -> (Integer, Integer)
get2RightInt pilha
 | topIsRight pilha = (v1, v2)
 | otherwise = error "Run-time error" 
 where 
  Right v1 = top pilha
  Right v2 = top (pop pilha)


get2LeftString :: Pilha (Either String Integer) -> (String, String)
get2LeftString pilha
 | topIsLeft pilha = (v1, v2)
 | otherwise = error "Run-time error" 
 where 
  Left v1 = top pilha
  Left v2 = top (pop pilha)


topIsLeft :: Pilha (Either String Integer) -> Bool
topIsLeft pilha = isLeft (top pilha) && isLeft (top (pop pilha))

topIsRight :: Pilha (Either String Integer) -> Bool
topIsRight pilha = isRight (top pilha) && isRight (top (pop pilha))
