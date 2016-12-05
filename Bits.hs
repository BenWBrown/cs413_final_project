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

multiplyB :: [[Bit]] -> [[Bit]]
-- returns [[carry out - upper bits],[output]
multiplyB (x:xs) = let y = decToBinary $  show (foldr (*) 1 $ map binaryToDecimal (x:xs))
                   in setCarry (length y) (length x) y : drop (length y - length x) y : []

setCarry :: Int -> Int -> [Bit] -> [Bit]
setCarry lenY lenX x = let x' = take (lenY - lenX) x
                  in case head x' == Zero of
                  True -> (take (lenX - (lenY - lenX)) $ repeat Zero) ++ x'
                  False -> (take (lenX - (lenY - lenX)) $ repeat One) ++ x'


subB :: [[Bit]] -> [Bit]
-- will only ever have two inputs
subB inpts = let (x:xs) = reverse $ map binaryToDecimal inpts
             in decToBinary $ show (foldr (-) x xs)


divideB :: [[Bit]] -> [[Bit]]
-- returns [[quotient, remainder]]
divideB (x:y) = let (x':y':[]) = map binaryToDecimal (x:y)
                in addBits (length x) $ decIntToBinary $ (div x' y') : (mod x' y') : []

addBits :: Int -> [[Bit]] -> [[Bit]]
-- ensures consistent bitwidth for both quotient and remainder
-- ex: [Zero, One] and bitwidth = 4 -> [Zero, Zero, Zero, One]
addBits len (x:y:[])
    | (length x) < len && (length y < len)  = ((take (len-length x) $ repeat $ head x) ++ x) : ((take (len-length y) $ repeat $ head y) ++ y) : []
    | (length x) < len                       = ((take (len-length x) $ repeat $ head x) ++ x) : y : []
    | (length y) < len                       = x : ((take (len-length y) $ repeat $ head y) ++ y) : []
    | otherwise                              = (x:y:[])

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
decToBinary _ = []


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

comparator :: [[Bit]] -> [[Bit]]
comparator (x:y:[]) = comparator' (binaryToDecimal x) (binaryToDecimal y)
comparator _ = undefined

comparator' x y = if (x < y) then [[One], [Zero], [One]]
            else if (x==y) then [[Zero], [One], [Zero]]
            else [[Zero], [Zero], [One]]
