import Bits

addB :: [[Bit]] -> [[Bit]]
-- Adding 4 + 4 (0100 + 0100) -> 0 1000
-- the carry-out '0' is necesary to denote positive number
addB (x:xs) = let (y:ys) = decToBinary $ show (foldr (+) 0 $ map binaryToDecimal (x:xs))
             in case length (y:ys) > length x of
             True -> [y] : ys : []
             False -> (y:ys) : []


subB :: [[Bit]] -> [[Bit]]
subB (x:y:[]) = let (x':y':[]) = map binaryToDecimal (x:y:[])
                in case length (head $ decIntToBinary $ x' - y' : []) > (length x) of
                    True -> drop 1 (head $ decIntToBinary $ x' - y' : []) : []
                    False -> decIntToBinary $ x' - y' : []


multiplyB :: [[Bit]] -> [[Bit]]
-- returns [[carry out - upper bits],[output]
multiplyB (x:xs) = let y = decToBinary $  show (foldr (*) 1 $ map binaryToDecimal (x:xs))
                  in setCarry (length y) (length x) y : drop (length y - length x) y : []


divideB :: [[Bit]] -> [[Bit]]
-- returns [[quotient, remainder]]
divideB (x:y) = let (x':y':[]) = map binaryToDecimal (x:y)
                in addBits (length x) $ decIntToBinary $ (div x' y') : (mod x' y') : []
