import Circuit
import Bits
import ArgumentBase
import XMLParser
import Update

import System.Environment
import System.Exit
import Data.Foldable
import Data.List
import Control.Monad


data NumberBase = Binary | Decimal | Hex deriving Show

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
    inputValueStrings <- getInputValueStrings circuit
    let inputValues = map (stringToValue numberBase) inputValueStrings
    let outputValues = runCircuit circuit inputValues
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

getInputValueStrings :: LogicElement -> IO [String]
getInputValueStrings n = let inputList = map (\n' -> name n') $ inputs n
                         in forM inputList (\a -> do
                            putStrLn $ "Enter value for Input " ++ show a ++ ":"
                            value <- getLine
                            return value)


stringToValue :: NumberBase -> String -> [Bit]
stringToValue _ "0" = [Zero]
stringToValue Binary s = binToBinary s
stringToValue Decimal s = decToBinary s
stringToValue Hex s = hexToBinary s

valueToString :: NumberBase -> [Bit] -> String
valueToString _ bitString = show . binaryToDecimal $ bitString --TODO: WRITE THIS


displayOutputValues :: Circuit -> [[Bit]] -> NumberBase -> IO ()
displayOutputValues circuit outputBits base = do
  putStrLn . show $ outputBits
  return ()
