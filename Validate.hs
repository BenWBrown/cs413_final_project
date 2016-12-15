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
