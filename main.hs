import Circuit
import Bits

import System.Environment
import Data.Foldable

data NumberBase = Binary | Decimal | Hex deriving Show

main :: IO ()
main = do
  args <- getArgs  --gets command line arguments
  let numberBase = getNumberBase args
  circuitXML <- getCircuitXML args
  let circuit = parseXML circuitXML
  inputValueStrings <- getInputValueStrings circuit
  let inputValues = map (stringToValue numberBase) inputValueStrings
  let outputValues = runCircuit circuit inputValues
  displayOutputValues circuit outputValues numberBase

getNumberBase :: [String] -> NumberBase
getNumberBase _ = Binary

--gets the first not flag arguemnt as the XML filename
getCircuitXML :: [String] -> IO String
getCircuitXML args = readFile fileName where
    fileName = case find (\x -> head x /= '-') args of
      Nothing -> undefined
      Just str -> str

parseXML :: String -> Circuit
parseXML _ = Circuit [] [] []

--get the actual values for the inputs in the circuit from the user via the command line
getInputValueStrings :: LogicElement -> IO [String]
getInputValueStrings _ = return ["some", "inputs"]

stringToValue :: NumberBase -> String -> [Bit]
stringToValue _ _ = []

runCircuit :: Circuit -> [[Bit]] -> [[Bit]]
runCircuit _ _ = []

displayOutputValues :: Circuit -> [[Bit]] -> NumberBase -> IO ()
displayOutputValues _ _ _ = putStrLn "value for output 1: 42"
