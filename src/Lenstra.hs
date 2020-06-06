module Lenstra where

import Control.Monad
import Data.Maybe

import Debug.Trace

import EllipticCurve

lenstras :: Integer -> Integer -> Maybe Integer
lenstras n b
  | d > 1     = Just d
  | otherwise = listToMaybe $ mapMaybe lenstras' points_curves
  where
    d = gcd 6 n
    -- Check n is not perfect power.

    points_curves :: [(ECPoint, EC)]
    points_curves = [ (ECPoint x y, EC a ((y * y - x * x * x - a * x) `mod` n) n)
                    | x <- [1..(n-1)]
                    , y <- [1..(n-1)]
                    , a <- [1..(n-1)]
                    ]                                                              
    
    lenstras' :: (ECPoint, EC) -> Maybe Integer
    lenstras' (point, curve)
      | d == n    = Nothing
      | d > 1     = Just d
      | otherwise = case result of 
                      Left d' | d' < n -> Just d'
                      _                -> Nothing
      where
        d      = gcd (discriminant curve) n
        result = foldM (mult curve) point [1..b]

