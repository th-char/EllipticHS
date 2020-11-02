module Main where 

import Data.Time.Clock
import Data.Maybe
import System.Random

import Lib.Lenstra
import Lib.Pollards

main :: IO ()
main = mapM_ benchFactorisation tests
  where 
    tests = [ 2253907771
            , 34090746634016945057
            , 2192392375347842185105699
            , 101858515341186156233861774747
            , 408690345942924410509381586819
            , 37474679579109250157949172138642181
            , 52635022169628958833592981554606658319
            ]

benchFactorisation :: Integer -> IO ()
benchFactorisation n = do 
     putStrLn $ "Factorising " ++ (show n)
 
     putStrLn $ "  With Pollards:"
     t1 <- getCurrentTime
     putStrLn $ "    Result: " ++ (formatResult $ pollards n 70000000)
     putStrLn $ "    Result: " ++ (formatResult $ pollards n 70000000)
     putStrLn $ "    Result: " ++ (formatResult $ pollards n 70000000)
     t1' <- getCurrentTime
     putStrLn $ "    Time: " ++ (show $ (diffUTCTime t1' t1) / 3)
 
     putStrLn $ "  With Lenstras:"
     t2 <- getCurrentTime
     putStrLn $ "    Result: " ++ (formatResult $ runLenstra (mkStdGen 42) n)
     putStrLn $ "    Result: " ++ (formatResult $ runLenstra (mkStdGen 42) n)
     putStrLn $ "    Result: " ++ (formatResult $ runLenstra (mkStdGen 42) n)
     t2' <- getCurrentTime
     putStrLn $ "    Time: " ++ (show $ (diffUTCTime t2' t2) / 3)
   where 
     formatResult :: Maybe Integer -> String
     formatResult res = fromMaybe "FAILED" (show <$> res) 
 