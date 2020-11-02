module Lib.EllipticCurve where 

import Lib.Arithmetic

type Ring = Integer

data ECPoint = ECPoint Ring Ring 
  deriving Show

-- | EC a b n ==> y^2 = x^3 + ax + b over Z/nZ
data EC   = EC Ring Ring Ring 

-- | add two points on an elliptic curve, if the sum of the points 
--   exists, then return that point, otherwise, return the gcd of 
--   (x' - x) mod n and n
add :: EC -> ECPoint -> ECPoint -> Either Integer ECPoint
add (EC _ _ n) (ECPoint x y) (ECPoint x' y') 
  | d == 1    = Right $ ECPoint x'' y''
  | otherwise = Left d
  where
    (z, d) = invGCD ((x' - x) `mod` n) n
    λ      = ((y' - y) * z)      `mod` n
    x''    = (λ * λ - x - x')    `mod` n
    y''    = (λ * (x - x'') - y) `mod` n

-- | given a point P on EC, return 2P if it exists, otherwise,
--   return the gcd of 2y mod n and n
double :: EC -> ECPoint -> Either Integer ECPoint 
double (EC a _ n) (ECPoint x y)
  | d == 1    = Right $ ECPoint x'' y''
  | otherwise = Left d
  where
    (z, d) = invGCD ((2 * y) `mod` n) n
    λ      = ((3 * x * x + a) * z) `mod` n
    x''    = (λ * λ - 2 * x )      `mod` n
    y''    = (λ * (x - x'') - y)   `mod` n

-- | Given a point P on EC, and an integer n, return nP 
--   if it exists
mult :: EC -> ECPoint -> Integer -> Either Integer ECPoint 
mult ec p n 
  | n == 1 = Right p
  | r == 0 = p''
  | r == 1 = p'' >>= add ec p   
  where 
    p'      = mult ec p n'
    (n', r) = divMod n 2
    p''     = p' >>= double ec

discriminant :: EC -> Integer
discriminant (EC a b _) = 4 * a * a * a + 27 * b * b