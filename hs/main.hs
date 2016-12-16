import Circuit
import Bits
import ArgumentBase
import XMLParser
import Update
import Conversions
import Validate

import System.Environment
import System.Exit
import Data.Foldable
import Data.List
import Control.Monad


data NumberBase = Binary | Decimal | Hex deriving Show

maxRecursion = 10000

usage = "usage: runhaskell main.hs [-bdh] filename.xml"

main :: IO ()
main = do
  args <- getArgs  --gets command line arguments
  let notFlaggedArgs = filter (\x -> head x /= '-') args
  if length notFlaggedArgs /= 1 then do
    putStrLn usage
    exitWith . ExitFailure $ 1
  else do
    let numberBase = getNumberBase args
    circuitXML <- getCircuitXML args
    circuit <- parseCircuit circuitXML
    let valid = and (map (validateConnection circuit) (allConnections circuit))
    if not valid then do
      putStrLn "Error: incompatible widths"
      exitWith . ExitFailure $ 1
    else do
      inputValueStrings <- getInputValueStrings circuit
      let inputValues = map (stringToValue numberBase) inputValueStrings
      let outputValues = runCircuit maxRecursion circuit inputValues
      displayOutputValues circuit outputValues numberBase

getNumberBase :: [String] -> NumberBase
getNumberBase x
  | find (== "-b") x == Just "-b"   = Binary
  | find (== "-d") x == Just "-d"   = Decimal
  | find (== "-h") x == Just "-h"   = Hex
  | otherwise                       = error usage
                                      exitWith . ExitFailure $ 1


--gets the first arguemnt that's not a compiler flag as the XML filename
getCircuitXML :: [String] -> IO String
getCircuitXML args = readFile fileName where
    fileName = case find (\x -> head x /= '-') args of
      Nothing -> undefined
      Just str -> str

getInputValueStrings :: Circuit -> IO [String]
getInputValueStrings n = let inputList = map (\n' -> (name n', bitWidth n')) . (filter isInput) . inputs $ n
                         in forM inputList (\(a, width) -> do
                            putStr $ "Enter value for Input " ++ show a ++ " (" ++ show width ++ " bits): "
                            value <- getLine
                            return value)


stringToValue :: NumberBase -> String -> [Bit]
stringToValue _ "0" = [Zero]
stringToValue Binary s = binToBinary s
stringToValue Decimal s = decToBinary s
stringToValue Hex s = hexToBinary s

valueToString :: NumberBase -> [Bit] -> String
valueToString Binary bitString = binaryToString bitString
valueToString Decimal bitString = show . binaryToDecimal $ bitString
valueToString Hex bitString = binaryToHexString bitString


displayOutputValues :: Circuit -> [[Bit]] -> NumberBase -> IO ()
displayOutputValues (Circuit _ _ outputs) values base = do
  let list = zip (map (\x -> name x) outputs) (map (valueToString base) values)
  forM list (\(name, value) -> putStrLn ("Value for output " ++ show name ++ ": " ++ show value))
  return ()
