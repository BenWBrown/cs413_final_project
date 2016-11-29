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

addB :: [[Bit]] -> [[Bit]]
-- Adding 4 + 4 (0100 + 0100) -> 0 1000
-- the carry-out '0' is necesary to denote positive number
addB (x:xs) = let (y:ys) = decToBinary $ show (foldr (+) 0 $ map binaryToDecimal (x:xs))
             in case length (y:ys) > length x of
             True -> [y] : ys : []
             False -> (y:ys) : []

multiplyB :: [[Bit]] -> [Bit]
multiplyB inpts = decToBinary $  show (foldr (*) 1 $ map binaryToDecimal inpts)

subB :: [[Bit]] -> [Bit]
-- will only ever have two inputs
subB inpts = let (x:xs) = reverse $ map binaryToDecimal inpts
             in decToBinary $ show (foldr (-) x xs)

divideB :: [[Bit]] -> [Bit]
-- will only ever have two inputs
divideB inpts = let (x:y) = reverse $ map binaryToDecimal inpts
                in decToBinary $ show (foldr div x y)


negatorB :: [[Bit]] -> [[Bit]]
negatorB inpt = decIntToBinary $ map (* (-1)) (map binaryToDecimal inpt)

decIntToBinary :: [Int] -> [[Bit]]
decIntToBinary inpt = map decToBinary $ map show inpt

-- BIN [BIT] --> DEC INT * TWOS COMPLEMENT
binaryToDecimal :: [Bit] -> Int
binaryToDecimal (x:xs) = case x == One of
     False -> binToDecimal (x:xs)
     True -> (binToDecimal (tail $ decToBinary $ show (binToDecimal (map not' (x:xs)) + 1) )) * (-1)


binToBinary :: String -> [Bit]
-- from a binary string in Twos Complement to a Twos Complement string of bits
binToBinary s = map (\y -> if y == '1' then One else Zero) s

decToBinary :: String -> [Bit]
-- from a decimal string to a Twos Complement string of bits
decToBinary (s:s') = case s == '-' of
  False -> decToBinaryPositive (read (s:s') :: Int)
  True -> decToBinaryNegative (read (s') :: Int)

hexToBinary :: String -> [Bit]
hexToBinary x = binToBinary $ parseHex x

decToBinaryNegative :: Int -> [Bit]
decToBinaryNegative x = plusOne $ flipBits x

decToBinaryPositive :: Int -> [Bit]
-- decimal to binary positive
decToBinaryPositive 0 = [Zero]
decToBinaryPositive x = let s = (decToBin $ x `div` 2) ++ (show $ x `mod` 2) in
                        map (\y -> if y == '1' then One else Zero) s

-- TWOS COMPLEMENT --
flipBits :: Int -> [Bit]
flipBits x = map not' (decToBinaryPositive x)

plusOne :: [Bit] -> [Bit]
plusOne x = tail $ decToBinary $ show (binToDecimal x + 1)

-- BIN [BIT] -> DEC INT (NOT TWOS COMPLEMENT)
binToDecimal :: [Bit] -> Int
-- from a string to bits to a decimal integer
binToDecimal (x:xs) = foldr (\c s -> s * 2 + c) 0 (reverse (map convert (x:xs)))
                         where convert c = if c == Zero then 0 else 1

-- DEC INT -> BIN STRING
decToBin :: Int -> String
-- Convert base 10 Int to base 2 binary String
decToBin 0 = "0"
decToBin x =  (decToBin $ x `div` 2) ++ (show $ x `mod` 2)
