# cs413_final_project: Circuit Simulator

Authors:
  Ben Brown
  Ellen Sartorelli

Dependencies:
  HXT
    install using: cabal install hxt
    documentation: https://hackage.haskell.org/package/hxt-9.3.1.16/docs/Text-XML-HXT-Core.html

Usage:
   run on the command line: runhaskell main.hs [-bdh] path/to/circuit.xml
   -b: inputs / outputs specified in binary
   -d: inputs / outputs specified in two's complement encoded decimal
   -h: inputs / outputs specified in unsigned hex
