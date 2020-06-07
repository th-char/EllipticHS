module Lenstra where

import           Control.Monad
import           Data.Maybe
import qualified Data.Euclidean as E
import           System.Random

import EllipticCurve

lenstras :: Integer -> Integer -> Maybe Integer
lenstras n b
  | d > 1     = Just d
  | otherwise = listToMaybe $ mapMaybe lenstras' points_curves
  where
    d = gcd 6 n
    -- Check n is not perfect power.

    points_curves :: [(ECPoint, EC)]
    points_curves = take 1000000 $ genPointsCurves (mkStdGen 42)
    
    genPointsCurves :: RandomGen g => g -> [(ECPoint, EC)]
    genPointsCurves g = (ECPoint x y, EC a ((y * y - x * x * x - a * x) `mod` n) n) : genPointsCurves g'''
      where 
        (x, g')   = randomR (1, n - 1) g 
        (y, g'')  = randomR (1, n - 1) g'
        (a, g''') = randomR (1, n - 1) g''

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

{-- 
    lenstras' :: (ECPoint, EC) -> Maybe Integer
    lenstras' (point, curve)
      | δ `mod` n == 0 = Nothing
      | E.coprime δ n  = case result of 
                           Left d' | d' < n -> Just d'
                           _                -> Nothing
      | otherwise      = Just $ E.gcd δ n
      where
        δ      = discriminant curve
        result = foldM (mult curve) point [1..b]
--}