import Bits

-- XML specification: VOLTAGE BIT FIRST
pmos :: [[Bit]] -> [[Bit]]
pmos (x:xs)
   | x == [Zero] = xs
   | otherwise   = []


nmos :: [[Bit]] -> [[Bit]]
nmos (x:xs)
  | x == [One] = xs
  | otherwise  = []
