module Bits where

import Control.Applicative
import Data.Monoid
import ArgumentBase

data Bit = Zero | One
   deriving (Eq, Show)

not' :: Bit -> Bit
not' Zero = One
not' _ = Zero

and', or', xor', nand', nor', xnor' :: Bit -> Bit -> Bit
and' One One = One
and' _ _     = Zero

or' Zero Zero   = Zero
or' _ _         = One

xor' One One    = Zero
xor' Zero Zero  = Zero
xor' _ _        = One

nand' x y = not' (and' x y)
nor' x y = not' (or' x y)
xnor' x y = not' (xor' x y)

notB :: [[Bit]] -> [[Bit]]
notB (x:xs) = map not' x : []

liftA2' :: (a -> b -> c) -> [a] -> [b] -> [c]
liftA2' f xs ys = map (\(x, y) -> x `f` y) (zip xs ys)

andB, nandB, orB, norB, xorB, xnorB :: [[Bit]] -> [[Bit]]
andB (x:xs) = foldr (liftA2' and') x xs : []
nandB (x:xs) = foldr (liftA2' nand') x xs : []
orB (x:xs) = foldr (liftA2' or') x xs : []
norB (x:xs) = foldr (liftA2' nor') x xs : []
xorB (x:xs) = foldr (liftA2' xor') x xs : []
xnorB (x:xs) = foldr (liftA2' xnor') x xs : []

addB :: [[Bit]] -> [Bit]
addB inpts = stringToBits $ toBin $ foldr (+) 0 $ map toDecimal inpts

-- subB :: [[Bit]] -> [Bit]
-- subB inpts = map (\y -> if y == '1' then One else Zero) (toBin (foldr (-) 0 (map toDecimal inpts)))


stringToBits :: String -> [Bit]
stringToBits s = map (\y -> if y == '1' then One else Zero) s

toDecimal :: [Bit] -> Int
toDecimal x = foldr (\c s -> s * 2 + c) 0 (reverse (map convert x))
                      where convert c = if c == Zero then 0 else 1
