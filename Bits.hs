data Bit = Zero | One
   deriving (Eq, Show)

not' :: Bit -> Bit
not' Zero = One
not' _ = Zero

and', or', xor' :: Bit -> Bit -> Bit
and' Zero _       = Zero
and' _ Zero       = Zero
and' One One      = One

or' One _         = One
or' _ One         = One
or' Zero Zero     = Zero

xor' One One      = Zero
xor' Zero Zero    = Zero
xor' One Zero     = One
xor' Zero One     = One

notB :: [Bit] -> [Bit]
notB x = map not' x

andB :: [Bit] -> [Bit] -> [Bit]
andB x y = map (\(x',y') -> and' x' y') $ zip x y

nandB :: [Bit] -> [Bit] -> [Bit]
nandB x y = notB $ andB x y

orB :: [Bit] -> [Bit] -> [Bit]
orB x y = map (\(x',y') -> or' x' y') $ zip x y

norB :: [Bit] -> [Bit] -> [Bit]
norB x y = notB $ orB x y

xorB :: [Bit] -> [Bit] -> [Bit]
xorB x y = map (\(x',y') -> and' (or' x' y') ( not' (and' x' y'))) $ zip x y

xnorB :: [Bit] -> [Bit] -> [Bit]
xnorB x y = notB $ xorB x y
