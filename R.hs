module R where

import T
import Q

data R = J Q | K (T -> Q)

instance Show R where
  show (J q) = show q
  show (K f) = "~" ++ show (f 30)

itor i = J (itoq i)

approx :: T -> R -> Q
approx n (J q) = q
approx n (K f) = f n

close :: Q -> R -> R -> Bool
close e r1 r2 | e <= 0 = error ("close, bad tolerance " ++ show e)
              | otherwise = 
  let (ew, ef) = properFraction e in
  let n = if ew > 0 then 0 else scaleQ ef in
  case cmpQ (absQ (approx n r1 - approx n r2)) e of
    LT -> True
    _  -> False

experimentZero :: R
experimentZero = K (\n -> W 1 (exp2 (s (s n))))

extend :: (Q -> Q) -> R -> R
extend f (J q) = J (f q)
extend f (K x) = K (f . x)

extend2 :: (Q -> Q -> Q) -> R -> R -> R
extend2 f (J q1) (J q2) = J (f q1 q2)
extend2 f (K x) (J q) = K (\n -> f (x n) q)
extend2 f (J q) (K x) = K (\n -> f q (x n))
extend2 f (K x1) (K x2) = K (\n -> f (x1 n) (x2 n))

instance Num R where
  fromInteger = itor
  (+) = extend2 (+)
  (-) = extend2 (-)
  (*) = extend2 (*)
  negate = extend negate
  abs = extend abs
  signum = extend signum -- this is broke, not continuous

minR :: R -> R -> R
minR = extend2 min

maxR :: R -> R -> R
maxR = extend2 max

instance Fractional R where
  fromRational q = J (fromRational q)
  (/) = extend2 (/)

sqrR :: R -> R
sqrR = extend (\x -> x * x)


