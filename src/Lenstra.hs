module Lenstra where

import Control.Monad
import Data.Maybe
import System.Random
import Control.Applicative

import EllipticCurve

type LenstraSample = (ECPoint, EC)

runLenstra :: RandomGen g => g -> Integer -> Maybe Integer 
runLenstra g n = foldr (<|>) Nothing [ lenstras n firstCurves  2000 
                                     , lenstras n secondCurves 11000
                                     , lenstras n thirdCurves  50000
                                     ]
  where 
    (firstCurves,  cs)  = splitAt 15  points_curves
    (secondCurves, cs') = splitAt 60  cs
    (thirdCurves,  _)   = splitAt 300 cs'

    points_curves :: [LenstraSample]
    points_curves = genPointsCurves g
    
    genPointsCurves :: RandomGen g => g -> [LenstraSample]
    genPointsCurves g = (ECPoint x y, EC a ((y * y - x * x * x - a * x) `mod` n) n) : genPointsCurves g'''
      where 
        (x, g')   = randomR (1, n - 1) g 
        (y, g'')  = randomR (1, n - 1) g'
        (a, g''') = randomR (1, n - 1) g''

lenstras ::  Integer -> [LenstraSample] -> Integer -> Maybe Integer
lenstras n cs b
  | d > 1     = Just d
  | otherwise = listToMaybe $ mapMaybe lenstras' cs
  where
    d = gcd 6 n
    -- Check n is not perfect power.

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