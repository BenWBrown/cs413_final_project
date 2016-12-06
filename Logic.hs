import Bits
import Conversions

negatorB :: [[Bit]] -> [[Bit]]
negatorB inpt = decIntToBinary $ map (* (-1)) (map binaryToDecimal inpt)

comparator :: [[Bit]] -> [[Bit]]
comparator (x:y:[]) = comparator' (binaryToDecimal x) (binaryToDecimal y)
comparator _ = undefined

comparator' x y = if (x < y) then [[One], [Zero], [One]]
            else if (x==y) then [[Zero], [One], [Zero]]
            else [[Zero], [Zero], [One]]
