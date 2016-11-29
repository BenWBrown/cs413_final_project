{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core

import Circuit
import Bits

{-
  todo:
  map from function name to function
  get stringToValue actually working (probably need to pass in bitwidth as well)
  figure out how to parse from a string instead of a file (should be easy)
-}


parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText

foo :: [[Bit]] -> [[Bit]]
foo x = x

stringToValue :: String -> [Bit]
stringToValue x = []

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
    let func = foo --TODO: MAP FROM NAME TO FUNC
    inputs <- listA (getConnectedElement "input") -< logic
    outputs <- listA (getConnectedElement "output") -< logic
    returnA -< LogicElement func inputs outputs

getConnectedElement elementType = getChildren >>> isElem >>> hasName elementType >>>
  proc element -> do
    name        <- getAttrValue "name" -< element
    bitWidth    <- getAttrValue "bitwidth" -< element
    connection  <- getAttrValue "connection" -< element
    valueString <- getAttrValue "value"      -< element
    let value = stringToValue valueString
    returnA -< if elementType == "input" then (Input name bitWidth connection value)
               else if elementType == "output" then (Output name bitWidth connection value)
               else (Constant name bitWidth connection value)
