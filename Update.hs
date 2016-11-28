import Circuit
import Bits

import Data.List

update :: Circuit -> Circuit -> Circuit
update containingCircuit (LogicElement f inputs outputs) = LogicElement f newInputs newOutputs where
  newInputs = map (update' containingCircuit) inputs
  newOutputs = case find (==[]) (map value inputs) of
    Nothing -> map newValue (zip outputs (f (map value inputs)))
    Just _ -> outputs
update containingCircuit c@(Circuit logicElts inputs outputs) =
  case (map (update containingCircuit) logicElts) of --apply in turn
  listOfLogicElts -> Circuit listOfLogicElts inputs (map (update' c) outputs)

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



--testing circuit

foo :: [[Bit]] -> [[Bit]]
foo (x:xs) = [[One, One]]

in1 = Input "and input 1" "4" "connection 1" []
in2 = Input "and input 2" "4" "connection 2" []
out1 = Output "and output" "4" "connection 3" []
x = LogicElement foo [in1, in2] [out1]

circIn1 = Input "input 1" "4" "connection 1" [Zero, One, Zero, One]
circIn2 = Input "input 2" "4" "connection 2" [Zero, Zero, One, One]

circOut1 = Output "output 1" "4" "connection 3" []

circuit = Circuit [x] [circIn1, circIn2] [circOut1]


circIn3 = Input "input 3" "4" "connection 1" [Zero, One, Zero, One]
circOut3 = Output "output 3" "4" "connection 1" []
circuit2 = Circuit [] [circIn3] [circOut3]
