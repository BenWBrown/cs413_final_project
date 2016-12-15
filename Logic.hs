module Logic where

import Bits
import Conversions

negatorB :: [[Bit]] -> [[Bit]]
negatorB (x:xs) = let (y:ys) = decIntToBinary $ map (* (-1)) (map binaryToDecimal (x:xs)) in
                  signExtend (length x) y : []

comparator :: [[Bit]] -> [[Bit]]
comparator (x:y:[]) = comparator' (binaryToDecimal x) (binaryToDecimal y)
comparator _ = undefined

comparator' x y = if (x > y) then [[One], [Zero], [Zero]]
            else if (x==y) then [[Zero], [One], [Zero]]
            else [[Zero], [Zero], [One]]

mux :: [[Bit]] -> [[Bit]]
-- when enable is Zero, output nothing
-- unsigned binary for selection values
-- XML specification: [[Enable],[Selection],[Inputs]]
mux ([Zero]:s:inputs) = []
mux ([One]:s:inputs)  = if (binToDecimal s > length inputs) then []
                        else inputs !! (binToDecimal s) : []
mux _                 = []


decoder :: [[Bit]] -> [[Bit]]
-- when enable is Zero, output nothing
-- unsigned binary for the selection values
-- returned in format [bitx... bit2, bit1, bit0]
-- XML specification: [[Enable],[Selection]]
decoder ([Zero]:s)  = []
decoder ([One]:s:[])   = let xs = take (2^(length s)) $ repeat [Zero] in
                       reverse $ take (binToDecimal s) xs ++ [[One]] ++ drop ((binToDecimal s) + 1) xs
decoder _           = []
