-- Program Skeleton HS


-- Data Structures:

-- Bit
data Bit = Zero | One
   deriving (Eq, Show)

not' :: Bit -> Bit
and', or', xor' :: Bit -> Bit -> Bit


-- Circuit
data Circuit = Circuit {
      runCircuit :: [String] -> [Maybe String]
}

-- Input
data Input = Input {
      bitWidth :: String, --read in from parser?
      connection :: String
      -- parent field??? ex: if it's an input to a logic function
}

-- Constant
data Constant = Constant {
       bitWidth :: String,
       value :: [Bit], --value may or may not start in base 2. value :: toBin v?
       connection :: String
}




--Functions:

not' :: [Bit] -> [Bit] -> [Bit]
and' :: [Bit] -> [Bit] -> [Bit]
nand :: [Bit] -> [Bit] -> [Bit]
or' :: [Bit] -> [Bit] -> [Bit]
nor :: [Bit] -> [Bit] -> [Bit]
xor :: [Bit] -> [Bit] -> [Bit]
xnor :: [Bit] -> [Bit] -> [Bit]
