import Bits
import Data.Sequence (fromList, update, Seq)

-- XML specification: VOLTAGE BIT FIRST
pmos :: [[Bit]] -> [[Bit]]
pmos (x:xs)
   | x == [Zero] = xs
   | otherwise   = []


nmos :: [[Bit]] -> [[Bit]]
nmos (x:xs)
  | x == [One] = xs
  | otherwise  = []


splitter :: [Int] -> [Bit] -> [[Bit]]
-- input: [[0, 0, 1, 2, 1, 3, 4, 5], [Zero, One, Zero, Zero, One, One, One, Zero]]
-- zipped: [(0,Zero),(0,One),(1,Zero),(2,Zero),(1,One),(3,One),(4,One),(5,Zero)]
-- result: [[Zero, One], [Zero, One], [Zero], [One], [One], [Zero]]
-- 0 - [Zero, One]
-- 1 - [Zero, One]
-- 2 - [Zero]
-- 3 - [One]
-- 4 - [One]
-- 5 - [Zero]
splitter order inputs = let emptyList = take ((foldr max 0 order)+1) $ repeat [] in
                        let zipped = zip order inputs in
                        filter (not . null) $ helper zipped emptyList


helper :: [(Int, Bit)] -> [[Bit]] -> [[Bit]]
helper [] list = list
helper ((i,b):xs) empty = let list = insert i b empty in
                          helper xs list

insert :: Int -> Bit -> [[Bit]] -> [[Bit]]
insert n _ y | n > length y = []
insert n x y = take n y ++ (y !! n ++ (x:[])) : [] ++ drop (n+1) y
