module Bits where

import Control.Applicative
import Data.Monoid

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

notB :: [Bit] -> [Bit]
notB = map not'

liftA2' :: (a -> b -> c) -> [a] -> [b] -> [c]
liftA2' f xs ys = map (\(x, y) -> x `f` y) (zip xs ys)

andB, nandB, orB, norB, xorB, xnorB :: [Bit] -> [Bit] -> [Bit]
andB = liftA2' and'
nandB = liftA2' nand'
orB = liftA2' or'
norB = liftA2' nor'
xorB = liftA2' xor'
xnorB = liftA2' xnor'


-- do these functions need to take the actual [Bit] input(s) as a parameter too????
convertFunction :: ([Bit]->[Bit]) -> ([[Bit]]->[[Bit]])
convertFunction f =

convertFunction' :: ([Bit]->[Bit]->[Bit]) -> ([[Bit]]->[[Bit]]->[[Bit]])
-- should this go to ([[Bit]]->[[Bit]]->[[Bit]]) or simply [[Bit]]->[[Bit]]??
convertFunction' f =
