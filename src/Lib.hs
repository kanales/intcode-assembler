{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}
module Lib
    ( Param(..)
    , Opcode(..)
    , repr
    )
where

data Param = Pos Integer | Imm Integer | Rel Integer deriving (Show)

get :: Param -> Integer
get (Pos a) = a
get (Imm a) = a
get (Rel a) = a

numericParam :: Param -> Integer
numericParam (Pos _) = 0
numericParam (Imm _) = 1
numericParam (Rel _) = 2

data Opcode a = Add a a a
            | Mul a a a
            | Inp a
            | Out a
            | Jnz a a
            | Jz a a
            | Lt a a a
            | Equ a a a
            | Rbs a
            | Hlt
            deriving (Show, Functor, Foldable)

code :: Opcode a -> (Integer, Integer)
code (Add _ _ _) = (0, 1)
code (Mul _ _ _) = (0, 2)
code (Inp _    ) = (0, 3)
code (Out _    ) = (0, 4)
code (Jnz _ _  ) = (0, 5)
code (Jz  _ _  ) = (0, 6)
code (Lt  _ _ _) = (0, 7)
code (Equ _ _ _) = (0, 8)
code (Rbs _    ) = (0, 9)
code Hlt         = (9, 9)

numericOpcode :: Opcode Param -> Integer
numericOpcode c =
    let pars   = foldl (\a x -> numericParam x : a) [] c
        (f, s) = code c
        ds     = pars ++ [f, s]
    in  foldl (\a d -> a * 10 + d) 0 ds

repr :: Opcode Param -> [Integer]
repr c = numericOpcode c : foldr (:) [] (get <$> c)

