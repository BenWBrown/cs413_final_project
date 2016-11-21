-- Program Skeleton HS
module Circuit where

import Bits

-- Data Structures:

-- Circuit
data Circuit = Circuit {
      runCircuit :: [String] -> [Maybe String]
}

data ConnectedElement = Input {
      bitWidth :: String, --read in from parser?
      connection :: String
      -- parent field??? ex: if it's an input to a logic function
} | Output {
      bitWidth :: String, --read in from parser?
      connection :: String
} | Constant {
       bitWidth :: String,
       value :: [Bit], --value may or may not start in base 2. value :: toBin v?
       connection :: String
} --TODO: add logic part??
