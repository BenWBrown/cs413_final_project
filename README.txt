cs413_final_project: Circuit Simulator

Authors:
  Ben Brown
  Ellen Sartorelli

About:
  Our program will take an XML representation of a circuit (see below for specification), parse it, then run it. When running the circuit, the program will prompt the user for the values to any circuit inputs, simulate the circuit, then display the output values. Values can be inputted and displayed as binary, decimal, or hex. Included in the *samples* folder are sample XML files to run.

Dependencies:
  HXT
    install using: cabal install hxt
    documentation: https://hackage.haskell.org/package/hxt-9.3.1.16/docs/Text-XML-HXT-Core.html

Usage:
   run on the command line: runhaskell main.hs [-bdh] path/to/circuit.xml
   -b: inputs / outputs specified in binary
   -d: inputs / outputs specified in two's complement encoded decimal
   -h: inputs / outputs specified in unsigned hex

Current Features:
  Combinatorial logic including transistors, logic gates, inverters, muxes / decoders, splitters, and arithmetic elements.

Future Features:
  Nested circuits.
  GUI.
  Sequential logic / memory elements.

XML Specification: Context Free Grammar
  CIRCUIT       -> <circuit> ELEMENTS </circuit>
  ELEMENTS      -> ELEMENT ELEMENTS | EPSILON
  ELEMENT       -> INPUT | OUTPUT | CONSTANT | LOGIC
  INPUT         -> <input FIELDS ></input>
  OUTPUT        -> <output FIELDS ></output>
  CONSTANT      -> <constant FIELDS ></constant>
  LOGIC         -> <logic FIELDS > ELEMENTS </logic>
  FIELDS        -> FIELD FIELDS | EPSILON
  FIELD         -> NAME | TYPE | BITWIDTH | CONNECTION | WHICHBITS
  NAME          -> name=”ANYSTRING”
  TYPE          -> type=”TYPESTRING”
  TYPESTRING    -> not | and | nand | or | nor | xor | xnor | negator | comparator | mux | decoder | multiplier | divider | adder | pmos | nmons  
  BITWIDTH      -> bitwidth=”NUMBER”
  CONNECTION    -> connection=”ANYSTRING”
  WHICHBITS     -> whichBits=”NUMBERLIST”
  NUMBERLIST    -> NUMBER , NUMBERLIST | EPSILON
  EPSILON       -> *empty string*
  NUMBER        -> *base-10 encoded value*
  ANYSTRING     -> *any string with no special characters*
