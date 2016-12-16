module Arithmetic where
import Bits
import Conversions

addB :: [[Bit]] -> [[Bit]]
-- Adding 4 + 4 (0100 + 0100) -> 0 1000
-- the carry-out '0' is necesary to denote positive number
addB (x:xs) = let y = decToBinary $ show (foldr (+) 0 $ map binToDecimal (x:xs))
              in addB' y (length x)

addB' y x = if (x < length(y)) then [One] : (reverse $ take x $ reverse y) : []
                 else [Zero] : signExtend x y : []
                --  else if (x >length(y:ys)) then (y:ys) : []

subB :: [[Bit]] -> [[Bit]]
subB (x:y:[]) = let (x':y':[]) = map binToDecimal (x:y:[])
                 in case (x' - y' < 0) of
                   True -> [One] : signExtend (length x) (decIntToBinary' (x'-y')) : []
                   False -> [Zero] : signExtend (length x) (decIntToBinary' (x'-y')) : []


multiplyB :: [[Bit]] -> [[Bit]]
multiplyB (x:y:[]) = let (x':y':[]) = map binToDecimal (x:y:[])
                     in case (length x * 2) >= length (decIntToBinary' $ x' * y') of
                       True -> multiplyB' $ splitAt (length x) $ signExtend' (length x * 2) (decIntToBinary' $ x' * y')
                       False -> multiplyB' $ splitAt (length x) $ signExtend' (length x * 2) (tail $ decIntToBinary' $ x' * y')

multiplyB' :: ([Bit],[Bit]) -> [[Bit]]
multiplyB' x = fst x : snd x : []


divideB :: [[Bit]] -> [[Bit]]
-- returns [[quotient], [remainder]]
divideB (x:y) = let (x':y':[]) = map binToDecimal (x:y)
                in addBits (length x) $ decIntToBinary $ (div x' y') : (mod x' y') : []

  -- Helpers --
setCarry :: [Bit] -> [Bit] -> [Bit]
-- for multiplication
setCarry x y
  | (length x) == (length y) = decToBinary $ show (foldr (+) 0 (map binaryToDecimal $ x:y:[] ))
  | otherwise                = let x' = take ((length y) - (length x)) x
                               in case head x' == Zero of
                               True -> (take ((length x) - ((length y) - (length x))) $ repeat Zero) ++ x'
                               False -> (take ((length x) - ((length y) - (length x))) $ repeat One) ++ x'


addBits :: Int -> [[Bit]] -> [[Bit]]
-- ensures consistent bitwidth for both quotient and remainder
-- ex: [Zero, One] and bitwidth = 4 -> [Zero, Zero, Zero, One]
addBits len (x:y:[])
    | (length x) < len && (length y < len)  = ((take (len-length x) $ repeat $ head x) ++ x) : ((take (len-length y) $ repeat $ head y) ++ y) : []
    | (length x) < len                       = ((take (len-length x) $ repeat $ head x) ++ x) : y : []
    | (length y) < len                       = x : ((take (len-length y) $ repeat $ head y) ++ y) : []
    | otherwise                              = (x:y:[])
