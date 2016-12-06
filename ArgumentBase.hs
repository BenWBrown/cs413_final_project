-- Functions required for stringToValue :: "NumberBase" -> String -> "Bit"
module ArgumentBase where

parseHex :: String -> String
-- Convert Hex String to base 10 Int
parseHex hxStr = go hxStr
    where go ""     = ""
          go (x:xs) = hexChar x ++ parseHex xs

hexChar :: Char -> String
hexChar '0' = "0000"
hexChar '1' = "0001"
hexChar '2' = "0010"
hexChar '3' = "0011"
hexChar '4' = "0100"
hexChar '5' = "0101"
hexChar '6' = "0110"
hexChar '7' = "0111"
hexChar '8' = "1000"
hexChar '9' = "1001"
hexChar 'A' = "1010"
hexChar 'B' = "1011"
hexChar 'C' = "1100"
hexChar 'D' = "1101"
hexChar 'E' = "1110"
hexChar 'F' = "1111"
