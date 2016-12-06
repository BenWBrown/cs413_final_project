module Conversions where

import Bits
import ArgumentBase

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
