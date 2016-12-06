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

mux :: Bit -> [Bit] -> [[Bit]] -> [[Bit]]
-- when enable is Zero, output nothing
mux Zero _  _         = []
mux One select inputs = if (binaryToDecimal select < 0) then []
        else if (binaryToDecimal select > length inputs) then []
        else inputs !! (binaryToDecimal select) : []
