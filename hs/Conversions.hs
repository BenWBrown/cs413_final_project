module Conversions where

import Bits
import ArgumentBase
import Data.Char

decIntToBinary :: [Int] -> [[Bit]]
decIntToBinary inpt = map decToBinary $ map show inpt

decIntToBinary' :: Int -> [Bit]
-- variation of decIntToBinary, takes Int instead of [Int]
decIntToBinary' inpt = decToBinary $ show inpt

-- BIN: [BIT] --> DEC INT **** TWOS COMPLEMENT
binaryToDecimal :: [Bit] -> Int
binaryToDecimal (x:xs) = case x == One of
     False -> binToDecimal (x:xs)
     True -> (binToDecimal (tail $ decToBinary $ show (binToDecimal (map not' (x:xs)) + 1) )) * (-1)

binaryToString :: [Bit] -> String
-- convert [Bit] in base 2 to string of 1s and 0s - for printing final output
binaryToString [] = ""
binaryToString (Zero:xs) = "0" ++ binaryToString xs
binaryToString (One:xs) = "1" ++ binaryToString xs

binaryToHexString :: [Bit] -> String
-- convert [Bit] in base 16 to string of 1s and 0s - for printing final output
binaryToHexString = reverse . binaryToHexString' . reverse

binaryToHexString' :: [Bit] -> String
-- helper function
binaryToHexString' [] = ""
binaryToHexString' (w:x:y:z:rest) = ((hexCharOf . reverse $ (w:x:y:z:[])) ++ binaryToHexString' rest)
binaryToHexString' rest = hexCharOf . (signExtend 4) . reverse $ rest

hexCharOf :: [Bit] -> String
hexCharOf = charToHex . binaryToString

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
hexToBinary x = binToBinary . parseHex . (map toUpper) $ x

decToBinaryNegative :: Int -> [Bit]
-- helper for decToBinary if NEGATIVE number
decToBinaryNegative x = plusOne $ flipBits x

decToBinaryPositive :: Int -> [Bit]
-- helper for decToBinary if POSITIVE number
decToBinaryPositive 0 = [Zero]
decToBinaryPositive x = let s = (decToBin $ x `div` 2) ++ (show $ x `mod` 2) in
                        map (\y -> if y == '1' then One else Zero) s

-- TWOS COMPLEMENT HELPERS --
flipBits :: Int -> [Bit]
flipBits x = map not' (decToBinaryPositive x)

plusOne :: [Bit] -> [Bit]
plusOne x = tail $ decToBinary $ show (binToDecimal x + 1)
---


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

signExtend :: Int -> [Bit] -> [Bit]
-- extends a [Bit] to proper bitwidth using it's most significant bit
signExtend n bitstring | n < length bitstring = undefined
                       | otherwise = (take (n - length bitstring) (repeat . head $ bitstring)) ++ bitstring


signExtend' :: Int -> [Bit] -> [Bit]
-- variation of signExtend, can take a bitwidth longer than the established bitwidth
-- used in multiplication function
signExtend' n bitstring = (take (n - length bitstring) (repeat . head $ bitstring)) ++ bitstring
