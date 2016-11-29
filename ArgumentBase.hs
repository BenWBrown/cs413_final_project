-- Functions required for stringToValue :: [NumberBase] -> String -> [Bit]
module ArgumentBase
(
parseHex,
hexChar,
) where


parseHex :: String -> Int
-- Convert Hex String to base 10 Int
parseHex hxStr = go (reverse hxStr)
    where go []     = 0
          go (x:xs) = hexChar x + 16 * parseHex xs

hexChar :: Char -> Int
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
