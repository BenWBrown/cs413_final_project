-- Program Skeleton HS
module Circuit where

import Bits

-- Data Structures:

-- Circuit


data ConnectedElement = Input {
      label :: String,
      bitWidth :: String, --read in from parser?
      connection :: String
      -- parent field??? ex: if it's an input to a logic function
} | Output {
      label :: String,
      bitWidth :: String, --read in from parser?
      connection :: String
} | Constant {
      label :: String,
      bitWidth :: String,
      value :: [Bit], --value may or may not start in base 2. value :: toBin v?
      connection :: String
}

data LogicElement = LogicElement ([[Bit]] -> [[Bit]]) [ConnectedElement] [ConnectedElement]
                  | Circuit [LogicElement] [ConnectedElement] [ConnectedElement]

inputs :: LogicElement -> [ConnectedElement]
inputs (LogicElement _ inputs _) = inputs
inputs (Circuit _ inputs _) = inputs

outputs :: LogicElement -> [ConnectedElement]
outputs (LogicElement _ _ outputs) = outputs
outputs (Circuit _ _ outputs) = outputs

type Circuit = LogicElement
