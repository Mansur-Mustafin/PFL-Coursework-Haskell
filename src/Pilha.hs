module Pilha (Pilha,            -- exportar o tipo
              push, pop, top,   -- e as operações
              empty, isEmpty) where

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
isEmpty (Stk [])= True
isEmpty (Stk _) = False