module Z where

import T

data Z = Y T T
  deriving (Show,Eq)

instance Num Z where
  fromInteger = fromI
  (+) = addZ
  (*) = mulZ
  (-) = subZ
  negate = negZ
  signum = signZ
  abs = absZ

fromI :: Integer -> Z
fromI i | i < 0 = Y 0 (t (negate i))
        | i > 0 = Y (t i) 0
        | otherwise = Y 0 0

toI :: Z -> Integer
toI (Y x (F [])) = n x
toI (Y (F []) y) = negate (n y)

fromT :: T -> Z
fromT n = Y n 0

norm :: Z -> Z
norm x@(Y _ (F [])) = x
norm x@(Y (F []) _) = x
norm (Y a b) = case cmp a b of
  LT -> Y 0 (sub b a)
  EQ -> Y 0 0
  GT -> Y (sub a b) 0

addZ :: Z -> Z -> Z
addZ (Y a b) (Y c d) = norm (Y (add a c) (add b d))

negZ :: Z -> Z
negZ (Y a b) = Y b a

subZ :: Z -> Z -> Z
subZ x y = addZ x (negZ y)

mulZ :: Z -> Z -> Z
mulZ (Y a (F [])) (Y c (F [])) = Y (mul a c) (F [])
mulZ (Y a (F [])) (Y (F []) d) = Y (F []) (mul a d)
mulZ (Y (F []) b) (Y c (F [])) = Y (F []) (mul b c)
mulZ (Y (F []) b) (Y (F []) d) = Y (mul b d) (F [])

absZ :: Z -> Z
absZ i = case cmpZ i (Y 0 0) of
  LT -> negZ i
  _  -> i

signZ :: Z -> Z
signZ i = case cmpZ i (Y 0 0) of
  LT -> Y (F []) (F [F[]])
  GT -> Y (F [F[]]) (F [])
  EQ -> i

toT :: Z -> T
toT (Y x (F [])) = x
toT (Y _ x)      = x

-- ignores the sign atm
divZ :: Z -> Z -> (Z,Z)
divZ a b = let (q,r) = division (toT a) (toT b) in (fromT q, fromT r)

-- ignores sign
powZ :: Z -> Z -> Z
powZ a b = fromT (pow (toT a) (toT b))

cmpZ :: Z -> Z -> Ordering
cmpZ (Y (F []) (F [])) (Y (F []) (F [])) = EQ
cmpZ (Y (F []) _) (Y _ (F [])) = LT
cmpZ (Y _ (F [])) (Y (F []) _) = GT
cmpZ (Y (F []) b) (Y (F []) d) = cmp d b
cmpZ (Y b (F [])) (Y c (F [])) = cmp b c
