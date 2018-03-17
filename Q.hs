module Q where

import Data.Ratio

import T
import Z

data Q = W Z T
  deriving (Eq)

instance Show Q where
  show (W a@(Y x y) b) = case cmpZ a (Y 0 0) of
    GT -> show (n x) ++ "/" ++ show (n b)
    LT -> "-" ++ show (n y) ++ "/" ++ show (n b)
    EQ -> "0"

instance Num Q where
  fromInteger = itoq
  (+) = addQ
  (*) = mulQ
  negate = negQ
  abs = absQ
  signum = signQ
  

instance Fractional Q where
  fromRational q = W (fromI (numerator q)) (t (denominator q))
  x / y = x * recipQ y

instance RealFrac Q where
  properFraction (W a b) = (fromIntegral q, W (fromT r) b) where
    (q, r) = division (toT a) b

instance Real Q where
  toRational (W a b) = toI a % toInteger b

instance Ord Q where
  compare = cmpQ

itoq :: Integer -> Q
itoq i = W (fromI i) 1

normQ :: Q -> Q
normQ (W a b) = W (fst (divZ a (fromT c))) (fst (division b c)) where
  c  = gcd_ (toT a) b

recipQ :: Q -> Q
recipQ (W a b) = case cmpZ a (Y 0 0) of
  GT -> W (fromT b) (toT a)
  EQ -> error "recipQ zero"
  LT -> W (Y 0 b) (toT a)

addQ :: Q -> Q -> Q
addQ (W a b) (W c d) = normQ (W (addZ a' c') e) where
  e  = mul b d
  a' = mulZ a (fromT d)
  c' = mulZ c (fromT b)

mulQ :: Q -> Q -> Q
mulQ (W a b) (W c d) = normQ (W (mulZ a c) (mul b d))

absQ :: Q -> Q
absQ (W a b) = W (absZ a) b

cmpQ :: Q -> Q -> Ordering
cmpQ (W a b) (W c d) = cmpZ a' c' where
  a' = mulZ a (fromT d)
  c' = mulZ c (fromT b)

signQ :: Q -> Q
signQ (W a _) = case cmpZ a (Y 0 0) of
  LT -> W (-1) 1
  EQ -> W 0 1
  GT -> W 1 1
  
negQ :: Q -> Q
negQ (W a b) = W (negZ a) b

-- 1 > q > 0 is less than 1 / 2^what
scaleQ :: Q -> T
scaleQ q@(W a b) = s' $ head (filter (\n -> q >= W 1 (exp2 n)) [0..])
