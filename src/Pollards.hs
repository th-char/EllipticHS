module Pollards where

import Arithmetic 
import Data.Maybe

pollards :: Integer -> Integer -> Maybe Integer
pollards n b
  | rem n 2 == 0 = Just $ gcd n 2
  | otherwise    = listToMaybe $ filter (\x -> 1 < x && x < n) $ map (\a -> gcd n (a - 1)) aks
  where 
    aks = scanl (\x y-> modPow x y n) 2 [1..b]
