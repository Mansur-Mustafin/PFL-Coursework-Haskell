module Pilha (Pilha,                                         -- exportar o tipo
              push, pop, top, get2RightValues, get2LeftValues,  -- e as operações
              empty, isEmpty) where

data Pilha a = Stk [a] deriving Show

empty :: Pilha a
empty = Stk []

isEmpty :: Pilha a -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False

push :: a -> Pilha a -> Pilha a 
push x (Stk xs) = Stk (x:xs)

pop :: Pilha a -> Pilha a
pop (Stk (_:xs)) = Stk xs
pop (Stk []) = error "Pilha.pop: empty stack"

top :: Pilha a -> a
top (Stk (x:_)) = x
top (Stk _) = error "Pilha.top: empty stack"

get2RightValues :: Pilha (Either a b) -> (b, b, Pilha (Either a b))
get2RightValues (Stk (Right v1:Right v2:xs)) = (v1, v2, Stk xs)
get2RightValues (Stk _) = error "Run-time error"

get2LeftValues :: Pilha (Either a b) -> (a, a, Pilha (Either a b))
get2LeftValues (Stk (Left v1:Left v2:xs)) = (v1, v2, Stk xs)
get2LeftValues (Stk _) = error "Run-time error"


-- Old versions of functions:

{-
    get2RightValues :: Pilha (Either a b) -> (b, b, Pilha (Either a b))
    get2RightValues pilha
     | topIsRight pilha = (v1, v2, pop . pop $ pilha)
     | otherwise = error "Run-time error" 
     where 
      Right v1 = top pilha
      Right v2 = top (pop pilha)

    topIsRight :: Pilha (Either String Integer) -> Bool
    topIsRight pilha = isRight (top pilha) && isRight (top (pop pilha))
-}

-- One version with top pop ...
{-
get2RightValues :: Pilha (Either a b) -> (b, b, Pilha (Either a b))
get2RightValues pilha = 
  case (top pilha, top . pop $ pilha) of
    (Right v1, Right v2) -> (v1, v2, pop . pop $ pilha)
    _                    -> error "Run-time error"
-}