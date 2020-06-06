module Main where 

import Data.Time.Clock
import System.IO
import Text.Printf

import Lenstra
import Pollards

main :: IO ()
main = mapM_ benchFactorisation [1579057808224947489159455409497076163663656780411374497] 

benchFactorisation :: Integer -> IO ()
benchFactorisation n = do 
    print $ printf "Factorising " ++ (show n)

    print $ "  With Pollards:"
    t <- getCurrentTime
    print $ "   " ++ (show $ pollards n 10000000000000000)
    t' <- getCurrentTime
    print $ "   " ++ (show $ diffUTCTime t' t)

    print $ "  With Lenstras:"
    t <- getCurrentTime
    print $ "   " ++ (show $ lenstras n 11)
    t' <- getCurrentTime
    print $ "   " ++ (show $ diffUTCTime t' t)
