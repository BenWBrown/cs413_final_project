module Validate where

import Circuit

import Data.Set (Set)
import qualified Data.Set as Set

allConnections :: Circuit -> [String]
allConnections = Set.toList . allConnections'

allConnections' :: Circuit -> Set String
allConnections' (Circuit logicElts ins outs) = Set.unions [ Set.unions (map allConnections' logicElts),
                                                            Set.fromList (map connection ins),
                                                            Set.fromList (map connection outs)]
allConnections' (LogicElement _ ins outs) = Set.union (Set.fromList (map connection ins)) (Set.fromList (map connection outs))

validateConnection :: Circuit -> String  -> Bool
validateConnection circ connectionName = validateConnection' "" connectionName circ

validateConnection' :: String -> String -> Circuit -> Bool
validateConnection' "" connectionName e@(LogicElement _ ins outs) =
  case connectedElts of
    [] -> True
    (x:_) -> validateConnection' (bitWidth x) connectionName e
  where
    connectedElts = filter (\x -> connection x == connectionName) (ins ++ outs)

validateConnection' connectionBitWidth connectionName (LogicElement _ ins outs) =
  foldl (\x y -> x && bitWidth y == connectionBitWidth) True connectedElts where
    connectedElts = filter (\x -> connection x == connectionName) (ins ++ outs)

validateConnection' "" connectionName (Circuit logicElts ins outs) =
  case connectedElts of
    [] -> and (map (validateConnection' "" connectionName) logicElts)
    (elt:_) -> and (map (validateConnection' (bitWidth elt) connectionName) logicElts)
          && foldl (\x y -> x && bitWidth y == bitWidth elt) True connectedElts
  where
    connectedElts = filter (\x -> connection x == connectionName) (ins ++ outs)

validateConnection' connectionBitWidth connectionName (Circuit logicElts ins outs) =
  and (map (validateConnection' connectionBitWidth connectionName) logicElts)
  && foldl (\x y -> x && bitWidth y == connectionBitWidth) True connectedElts
  where
    connectedElts = filter (\x -> connection x == connectionName) (ins ++ outs)
