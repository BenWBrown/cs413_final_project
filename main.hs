import Circuit
import Bits

main :: IO ()
main = do
  putStrLn . show . sillyFunction $ [One, Zero]


sillyFunction :: [Bit] -> [Bit]
sillyFunction xs = map not' xs
