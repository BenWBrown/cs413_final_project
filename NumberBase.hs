import Circuit
import Bits

data NumberBase = Binary | Decimal | Hex deriving Show

getNumberBase :: [String] -> NumberBase
getNumberBase (x:xs)
  | x == "-b" = Binary
  | x == "-d" = Decimal
  | x == "-h" = Hex

stringToValue :: NumberBase -> String -> [Bit]
stringToValue _ "0" = [Zero] -- this always cuts off leading Zero bit (see line 21)
stringToValue Binary s = map (\y -> if y == '1' then One else Zero) s
stringToValue Decimal s = stringToValue Binary $ toBin (read s :: Int)
stringToValue Hex s = stringToValue Binary $ toBin (parseHex s)


-- Helper functions for stringToValue
toBin :: Int -> String
toBin 0 = [] -- this as "0" results in extra Zero bit (fixed (?) in line 13)
toBin x =  (toBin $ x `div` 2) ++ (show $ x `mod` 2)

parseHex :: String -> Int
parseHex hxStr = go (reverse hxStr)
    where go []     = 0
          go (x:xs) = hexChar x + 16 * parseHex xs

hexChar '0' = 0
hexChar '1' = 1
hexChar '2' = 2
hexChar '3' = 3
hexChar '4' = 4
hexChar '5' = 5
hexChar '6' = 6
hexChar '7' = 7
hexChar '8' = 8
hexChar '9' = 9
hexChar 'A' = 10
hexChar 'B' = 11
hexChar 'C' = 12
hexChar 'D' = 13
hexChar 'E' = 14
hexChar 'F' = 15
hexChar _ = 0
