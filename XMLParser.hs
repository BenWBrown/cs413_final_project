{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module XMLParser where

import Text.XML.HXT.Core
import Data.Map as M

import Circuit
import Bits
import Conversions

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
  ("xnor", xnorB)]

parseXML string = readString [ withValidate no, withRemoveWS yes] string
parseXML' file = readDocument [ withValidate no, withRemoveWS yes] file


atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText

getCircuit = atTag "circuit" >>>
  proc circ -> do
    circName <- getAttrValue "name" -< circ
    logics    <- listA getLogic                         -< circ
    inputs    <- listA (getConnectedElement "input")    -< circ
    outputs   <- listA (getConnectedElement "output")   -< circ
    returnA   -< (Circuit logics inputs outputs)

getLogic = getChildren >>> isElem >>> hasName "logic" >>>
  proc logic -> do
    funcName <- getAttrValue "type" -< logic
    let func = case M.lookup funcName functionMap of
                Just x -> x
                Nothing -> undefined
    inputs <- listA (getConnectedElement "input") -< logic
    outputs <- listA (getConnectedElement "output") -< logic
    returnA -< LogicElement func inputs outputs

getConnectedElement elementType = getChildren >>> isElem >>> hasName elementType >>>
  proc element -> do
    name        <- getAttrValue "name" -< element
    bitWidth    <- getAttrValue "bitwidth" -< element
    connection  <- getAttrValue "connection" -< element
    valueString <- getAttrValue "value"      -< element
    let value = decToBinary valueString
    returnA -< if elementType == "input" then (Input name bitWidth connection value)
               else if elementType == "output" then (Output name bitWidth connection value)
               else (Constant name bitWidth connection value)
