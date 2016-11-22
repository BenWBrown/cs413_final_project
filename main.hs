import Circuit
import Bits
import ArgumentBase

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
    let circuit = parseXML circuitXML
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

parseXML :: String -> Circuit
parseXML _ = Circuit [] [] []

--get the actual values for the inputs in the circuit from the user via the command line
getInputValueStrings :: LogicElement -> IO [String]
getInputValueStrings _ = return ["some", "inputs"]

-- WORK IN PROGRESS - does not compile
-- -- get the actual values for the inputs in the circuit from the user via the command line
-- getInputValueStrings :: LogicElement -> IO [String]
-- getInputValueStrings n = helper (inputs n)

-- the "name"s need to be extracted from the Inputs. inputs LogicElement returns the ConnectedElement Input... I just care about their names
-- these names will be used in asking for input (see helper function that takes [String] below)
-- Question: how do I extract the names from the Input in the LogicElement?

-- this probably does not have to be a helper function - I just don't understand how to combine it with the above.
helper :: [String] -> IO [String]
helper inputList = forM inputList (\a -> do
                   putStrLn $ "Enter value for Input " ++ show a ++ ":"
                   value <- getLine
                   return value)


stringToValue :: NumberBase -> String -> [Bit]
stringToValue _ "0" = [Zero]
stringToValue Binary s = map (\y -> if y == '1' then One else Zero) s
stringToValue Decimal s = stringToValue Binary $ toBin (read s :: Int)
stringToValue Hex s = stringToValue Binary $ toBin (parseHex s)

runCircuit :: Circuit -> [[Bit]] -> [[Bit]]
runCircuit _ _ = []

displayOutputValues :: Circuit -> [[Bit]] -> NumberBase -> IO ()
displayOutputValues _ _ _ = putStrLn "value for output 1: 42"
