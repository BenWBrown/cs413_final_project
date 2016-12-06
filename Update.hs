module Update where

import Circuit
import Bits

import Data.List

update :: Circuit -> Circuit -> Circuit
update containingCircuit (LogicElement f ins outs) = LogicElement f newInputs newOutputs where
  newInputs = map (update' containingCircuit) ins
  newOutputs = case find (==[]) (map value ins) of
    Nothing -> map newValue (zip outs (f (map value ins)))
    Just _ -> outs
update containingCircuit c@(Circuit logicElts ins outs) =
  case (map (update containingCircuit) logicElts) of --apply in turn
  listOfLogicElts -> Circuit listOfLogicElts ins (map (update' c) outs)

update' :: Circuit -> ConnectedElement -> ConnectedElement
update' containingCircuit elt = case valueForConnection containingCircuit (connection elt) of
    Just x -> newValue (elt, x)
    Nothing -> elt

    -- (\input -> case valueForConnection containingCircuit (connection input) of
    --   Just x -> newValue (input, x)
    --   Nothing -> input)

-- foo input = case valueForConnection containingCircuit (connection input) of
--   Just x -> newValue (input, x)
--   Nothing -> input


newValue :: (ConnectedElement, [Bit]) -> ConnectedElement
newValue ((Output name bitwidth connection _), value) = Output name bitwidth connection value
newValue ((Input name bitwidth connection _), value) = Input name bitwidth connection value
--newValue Constant _ _ _ _ = un

valueForConnection :: Circuit -> String -> Maybe [Bit]
valueForConnection (LogicElement _ ins outs) name = fmap value (find (\x -> connection x == name) outs)
valueForConnection (Circuit logicElts ins _) name =
  case fmap value (find (\x -> connection x == name) ins) of
    Nothing -> fmap value (find (\x -> connection x == name) (concat (map outputs logicElts)))
    Just x -> Just x


updateInputs :: Circuit -> [[Bit]] -> Circuit
updateInputs (Circuit logicElts ins outs) inputValues = Circuit logicElts (map newValue $ zip ins inputValues) outs

runCircuit :: Circuit -> [[Bit]] -> [[Bit]]
runCircuit circuit ins = map value (outputs (runCircuit' (updateInputs circuit ins) circuit ))

runCircuit' :: Circuit -> Circuit -> Circuit
runCircuit' containingCircuit circuit =
  case allOutputsFilled circuit of
    False -> runCircuit' containingCircuit (update containingCircuit circuit)
    True -> circuit

allOutputsFilled :: Circuit -> Bool
allOutputsFilled (Circuit _ _ outs) = and (map (/=[]) (map value outs))

--testing circuit



in1 = Input "and input 1" "4" "connection 1" []
in2 = Input "and input 2" "4" "connection 2" []
out1 = Output "and output" "4" "connection 3" []
x = LogicElement orB [in1, in2] [out1]

circIn1 = Input "input 1" "4" "connection 1" [Zero, One, Zero, One]
circIn2 = Input "input 2" "4" "connection 2" [Zero, Zero, One, One]

circOut1 = Output "output 1" "4" "connection 3" []

circuit = Circuit [x] [circIn1, circIn2] [circOut1]


circIn3 = Input "input 3" "4" "connection 1" [Zero, One, Zero, One]
circOut3 = Output "output 3" "4" "connection 1" []
circuit2 = Circuit [] [circIn3] [circOut3]
