module Lib.Arithmetic where

-- | modPow x y m == x^y mod m
modPow :: Integer -> Integer -> Integer -> Integer
modPow _ _ 0 = error "you fool"
modPow x 0 _ = 1
modPow x y m 
  | r == 0 = (pm * pm)     `mod` m
  | r == 1 = (pm * pm * x) `mod` m
  where 
    pm = modPow x y' m
    (y', r) = divMod y 2

-- | given a and b, returns (a^(-1) mod b, gcd a b)
invGCD :: Integer -> Integer -> (Integer, Integer)
invGCD a b 
  = invGCD' 0 b 1 a 
  where 
    invGCD' _ 0 s' r' 
      = (if s' >= 0 then s' else b + s', r')
    invGCD' s r s' r' 
      = invGCD' (s' - q * s) (r' - q * r) s r
      where 
        q = quot r' r