{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module XMLParser where

import Text.XML.HXT.Core
import Data.Map as M

import Circuit
import Bits
import Conversions
import Wiring
import Logic

parseCircuit :: String -> IO Circuit
parseCircuit string = fmap head $ runX (parseXML string >>> getCircuit )


functionMap :: Map String ([[Bit]] -> [[Bit]])
functionMap = fromList [
  ("not", notB),
  ("and", andB),
  ("nand", nandB),
  ("or", orB),
  ("nor", norB),
  ("xor", xorB),
  ("xnor", xnorB),
  ("negator", negatorB),
  ("comparator", comparator),
  ("mux", mux)]

parseXML string = readString [ withValidate no, withRemoveWS yes] string
parseXML' file = readDocument [ withValidate no, withRemoveWS yes] file


atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText

getCircuit = atTag "circuit" >>>
  proc circ -> do
    circName <- getAttrValue "name" -< circ
    logics    <- listA getLogic                         -< circ
    inputs    <- listA (getConnectedElement "input")    -< circ
    constants <- listA (getConnectedElement "constant") -< circ
    outputs   <- listA (getConnectedElement "output")   -< circ
    returnA   -< (Circuit logics (inputs ++ constants) outputs)

getLogic = getChildren >>> isElem >>> hasName "logic" >>> ifA (hasAttrValue "type" (=="splitter") )
  (this >>> proc logic -> do --if splitter
    outputBits <- listA getOutputBits -< logic
    let intList = tuplesToIntList (zip [0..] outputBits)
    inputs <- listA (getConnectedElement "input") -< logic
    outputs <- listA (getConnectedElement "output") -< logic
    returnA -< LogicElement (splitter intList) inputs outputs)
  (this >>> proc logic -> do --if not splitter
    funcName <- getAttrValue "type" -< logic
    let func = case M.lookup funcName functionMap of
                Just x -> x
                Nothing -> undefined
    inputs <- listA (getConnectedElement "input") -< logic
    outputs <- listA (getConnectedElement "output") -< logic
    returnA -< LogicElement func inputs outputs)

getOutputBits = getChildren >>> isElem >>> hasName "output" >>>
  proc element -> do
    whichBits <- getAttrValue "whichBits" -< element
    returnA -< stringToList whichBits

--takes a csv string of ints and returns a list of ints
stringToList :: String -> [Int]
stringToList str = read ('[' : str ++ "]")

--takes a list of tuples, each of type (Int, [Int]), representing a splitter output wire
--and the list of bits routed to that output wire. evaluates to a single list where each
--bit's destination is represented as an integer at that bit's index
tuplesToIntList :: [(Int, [Int])] -> [Int]
tuplesToIntList tuples = tuplesToIntList' startingList tuples where
  startingList = take (1 + Prelude.foldr max 0 (Prelude.map ((Prelude.foldr max 0) . snd) tuples)) (repeat 0) --make a list of all 0's of size 1+ max value in the list of tuples

tuplesToIntList' :: [Int] -> [(Int, [Int])] -> [Int]
tuplesToIntList' lst [] = lst
tuplesToIntList' lst (x:xs) = tuplesToIntList' (updateIndices lst x) xs

updateIndices :: [Int] -> (Int, [Int]) -> [Int]
updateIndices lst (_, []) = lst
updateIndices lst (value, (index:rest)) = updateIndices (replace lst index value) (value, rest)

replace :: [Int] -> Int -> Int -> [Int]
replace lst index value = (take index lst) ++ [value] ++ (drop (index + 1) lst)

getConnectedElement elementType = getChildren >>> isElem >>> hasName elementType >>>
  proc element -> do
    name        <- getAttrValue "name" -< element
    bitWidth    <- getAttrValue "bitwidth" -< element
    connection  <- getAttrValue "connection" -< element
    valueString <- getAttrValue "value"      -< element
    let value = if elementType == "constant" then (signExtend (read bitWidth)) . readValue $ valueString else []
    returnA -< if elementType == "input" then (Input name bitWidth connection value)
               else if elementType == "output" then (Output name bitWidth connection value)
               else (Constant name bitWidth connection value)

readValue :: String -> [Bit]
readValue ('0':'x':rest) = hexToBinary rest
readValue ('0':'b':rest) = binToBinary rest
readValue rest = decToBinary rest
