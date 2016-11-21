module Bits where

data NumberBase = Binary | Decimal | Hex deriving Show

-- Command line argument options: "b", "d", "h"
getNumberBase :: [String] -> NumberBase
getNumberBase x
  | x == "-b" = Binary
  | x == "-d" = Decimal
  | x == "-h" = Hex
  -- | x = _  error option for incorrect command line argument?


stringToValue :: NumberBase -> String -> [Bit]
-- take NumberBase and String and convert to Bits
-- stringToValue Binary s = map (\x -> if x == "1" then One else Zero) s:[]
-- stringToValue Decimal s = []
-- stringToValue Hex s = []
