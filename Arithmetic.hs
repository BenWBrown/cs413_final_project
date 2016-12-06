import Bits
import Conversions

addB :: [[Bit]] -> [[Bit]]
-- Adding 4 + 4 (0100 + 0100) -> 0 1000
-- the carry-out '0' is necesary to denote positive number
addB (x:xs) = let (y:ys) = decToBinary $ show (foldr (+) 0 $ map binaryToDecimal (x:xs))
             in case length (y:ys) > length x of
             True -> [y] : ys : []
             False -> (y:ys) : []


subB :: [[Bit]] -> [[Bit]]
-- returns [[upper bit carry],[lower bits]
subB (x:y:[]) = let (x':y':[]) = map binaryToDecimal (x:y:[])
                in case length (head $ decIntToBinary $ x' - y' : []) > (length x) of
                    True -> take 1 (head $ decIntToBinary $ x' - y' : []) : drop 1 (head $ decIntToBinary $ x' - y' : []) : []
                    False -> decIntToBinary $ x' - y' : []


multiplyB :: [[Bit]] -> [[Bit]]
-- returns [[carry out - upper bits],[output]
multiplyB (x:xs) = let y = decToBinary $  show (foldr (*) 1 $ map binaryToDecimal (x:xs))
                  in setCarry (length y) (length x) y : drop (length y - length x) y : []


divideB :: [[Bit]] -> [[Bit]]
-- returns [[quotient, remainder]]
divideB (x:y) = let (x':y':[]) = map binaryToDecimal (x:y)
                in addBits (length x) $ decIntToBinary $ (div x' y') : (mod x' y') : []


  -- Helpers --
setCarry :: Int -> Int -> [Bit] -> [Bit]
-- for multiplication
setCarry lenY lenX x = let x' = take (lenY - lenX) x
                  in case head x' == Zero of
                  True -> (take (lenX - (lenY - lenX)) $ repeat Zero) ++ x'
                  False -> (take (lenX - (lenY - lenX)) $ repeat One) ++ x'


addBits :: Int -> [[Bit]] -> [[Bit]]
-- ensures consistent bitwidth for both quotient and remainder
-- ex: [Zero, One] and bitwidth = 4 -> [Zero, Zero, Zero, One]
addBits len (x:y:[])
    | (length x) < len && (length y < len)  = ((take (len-length x) $ repeat $ head x) ++ x) : ((take (len-length y) $ repeat $ head y) ++ y) : []
    | (length x) < len                       = ((take (len-length x) $ repeat $ head x) ++ x) : y : []
    | (length y) < len                       = x : ((take (len-length y) $ repeat $ head y) ++ y) : []
    | otherwise                              = (x:y:[])
