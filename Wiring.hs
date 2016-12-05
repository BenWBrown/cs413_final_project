import Bits

pmos :: [[Bit]] -> [Bit] -> [[Bit]]
pmos input [Zero] = input
pmos _ _ = []

nmos :: [[Bit]] -> [Bit] -> [[Bit]]
nmos input [One] = input
nmos _ _ = []
