module EllipticCurve where 

import Arithmetic

data ECPoint = ECPoint Integer Integer 
  deriving Show

type Ring = Integer
data EC = EC Integer Integer Ring -- EC a b n ==> y^2 = x^3 + ax + b over Z/nZ

add :: EC -> ECPoint -> ECPoint -> Either Integer ECPoint
add (EC _ _ n) (ECPoint x y) (ECPoint x' y') 
  | d == 1    = Right $ ECPoint x'' y''
  | otherwise = Left d
  where
    (z, d) = invGCD (x' - x) n
    λ      = ((y' - y) * z)      `mod` n
    x''    = (λ * λ - x - x')    `mod` n
    y''    = (λ * (x - x'') - y) `mod` n

double :: EC -> ECPoint -> Either Integer ECPoint 
double (EC a _ n) (ECPoint x y)
  | d == 1    = Right $ ECPoint x'' y''
  | otherwise = Left d
  where
    (z, d) = invGCD (2 * y) n
    λ      = ((3 * x * x + a) * z) `mod` n
    x''    = (λ * λ - 2 * x )      `mod` n
    y''    = (λ * (x - x'') - y)   `mod` n

mult :: EC -> ECPoint -> Int -> Either Integer ECPoint 
mult ec p n 
  | n == 1 = Right p
  | r == 0 = p''
  | r == 1 = p'' >>= add ec p   
  where 
    p'      = mult ec p n'
    (n', r) = divMod n 2
    p''     = p' >>= double ec