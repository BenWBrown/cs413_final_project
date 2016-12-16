-- Functions required for stringToValue :: "NumberBase" -> String -> "Bit"
module ArgumentBase where

parseHex :: String -> String
-- Convert Hex String to base 10 Int
parseHex hxStr = go hxStr
    where go ""     = ""
          go (x:xs) = hexToChar x ++ parseHex xs

hexToChar :: Char -> String
hexToChar '0' = "0000"
hexToChar '1' = "0001"
hexToChar '2' = "0010"
hexToChar '3' = "0011"
hexToChar '4' = "0100"
hexToChar '5' = "0101"
hexToChar '6' = "0110"
hexToChar '7' = "0111"
hexToChar '8' = "1000"
hexToChar '9' = "1001"
hexToChar 'A' = "1010"
hexToChar 'B' = "1011"
hexToChar 'C' = "1100"
hexToChar 'D' = "1101"
hexToChar 'E' = "1110"
hexToChar 'F' = "1111"

charToHex :: String -> String
charToHex "0000" = "0"
charToHex "0001" = "1"
charToHex "0010" = "2"
charToHex "0011" = "3"
charToHex "0100" = "4"
charToHex "0101" = "5"
charToHex "0110" = "6"
charToHex "0111" = "7"
charToHex "1000" = "8"
charToHex "1001" = "9"
charToHex "1010" = "A"
charToHex "1011" = "B"
charToHex "1100" = "C"
charToHex "1101" = "D"
charToHex "1110" = "E"
charToHex "1111" = "F"
