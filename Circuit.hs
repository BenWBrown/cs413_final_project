-- Program Skeleton HS
module Circuit where

import Bits
import Text.ParserCombinators.Parsec

-- Data Structures:

-- Circuit


data ConnectedElement = Input {
      name :: String,
      bitWidth :: String, --read in from parser?
      connection :: String,
      value :: [Bit]
      -- parent field??? ex: if it's an input to a logic function
} | Output {
      name :: String,
      bitWidth :: String, --read in from parser?
      connection :: String,
      value :: [Bit]
} | Constant {
      name :: String,
      bitWidth :: String,
<<<<<<< Updated upstream
      connection :: String,
      value :: [Bit]
} deriving (Show)

instance Show (a -> b) where
  show _ = "<function>"
=======
      value :: [Bit], --value may or may not start in base 2. value :: toBin v?
      connection :: String
}deriving (Show)
>>>>>>> Stashed changes

data LogicElement = LogicElement ([[Bit]] -> [[Bit]]) [ConnectedElement] [ConnectedElement]
                  | Circuit [LogicElement] [ConnectedElement] [ConnectedElement]
                  deriving (Show)

inputs :: LogicElement -> [ConnectedElement]
inputs (LogicElement _ inputs _) = inputs
inputs (Circuit _ inputs _) = inputs

outputs :: LogicElement -> [ConnectedElement]
outputs (LogicElement _ _ outputs) = outputs
outputs (Circuit _ _ outputs) = outputs

type Circuit = LogicElement
