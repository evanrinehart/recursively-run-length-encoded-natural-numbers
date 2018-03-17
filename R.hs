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

dumbZero :: R
dumbZero = K (\n -> W 1 (exp2 (s (s n))))
