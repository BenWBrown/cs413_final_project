module Arithmetic where
import Bits
import Conversions

addB :: [[Bit]] -> [[Bit]]
-- output: [[carry], [sum]]
-- If the sum of unsigned values do not yield a result that fits into bitWidth bits,
-- then the carry bit will be 0; otherwise, it will be 1.
addB (x:xs) = let y = decToBinary $ show (foldr (+) 0 $ map binToDecimal (x:xs))
              in addB' y (length x)

-- helper function to set proper carry out
addB' y x = if (x < length(y)) then [One] : (reverse $ take x $ reverse y) : []
                 else [Zero] : signExtend x y : []


subB :: [[Bit]] -> [[Bit]]
-- output: [[carry], [sum]]
--If the values subtracted as unsigned values yield a negative value,
-- then this carry bit will be 1; otherwise, it will be 0.
subB (x:y:[]) = let (x':y':[]) = map binToDecimal (x:y:[])
                 in case (x' - y' < 0) of
                   True -> [One] : signExtend (length x) (decIntToBinary' (x'-y')) : []
                   False -> [Zero] : signExtend (length x) (decIntToBinary' (x'-y')) : []


multiplyB :: [[Bit]] -> [[Bit]]
-- output: [[most sig. bits of proper bitwidth], [least sig. bits of proper bitwidth]]
multiplyB (x:y:[]) = let (x':y':[]) = map binToDecimal (x:y:[])
                     in case (length x * 2) >= length (decIntToBinary' $ x' * y') of
                       True -> multiplyB' $ splitAt (length x) $ signExtend' (length x * 2) (decIntToBinary' $ x' * y')
                       False -> multiplyB' $ splitAt (length x) $ signExtend' (length x * 2) (tail $ decIntToBinary' $ x' * y')

-- helper function to extract the two [Bit] from the tuple
multiplyB' :: ([Bit],[Bit]) -> [[Bit]]
multiplyB' x = fst x : snd x : []


divideB :: [[Bit]] -> [[Bit]]
-- returns [[quotient], [remainder]]
divideB (x:y) = let (x':y':[]) = map binToDecimal (x:y)
                in addBits (length x) $ decIntToBinary $ (div x' y') : (mod x' y') : []

-- division helper to ensure consistent bitwidth for both quotient and remainder
addBits :: Int -> [[Bit]] -> [[Bit]]
-- ex: [Zero, One] and bitwidth = 4 -> [Zero, Zero, Zero, One]
addBits len (x:y:[])
    | (length x) < len && (length y < len)  = ((take (len-length x) $ repeat $ head x) ++ x) : ((take (len-length y) $ repeat $ head y) ++ y) : []
    | (length x) < len                       = ((take (len-length x) $ repeat $ head x) ++ x) : y : []
    | (length y) < len                       = x : ((take (len-length y) $ repeat $ head y) ++ y) : []
    | otherwise                              = (x:y:[])
