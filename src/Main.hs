module Main where 

import Data.Time.Clock
import System.IO
import Text.Printf
import Data.Maybe
import System.Random

import Criterion.Main.Options
import Criterion.Main
import Criterion.Types

import Lenstra
import Pollards

settings = defaultConfig { resamples = 5
                         , timeLimit = 5
                         }

-- main = defaultMainWith settings [
--              bench "Lenstra1 : 23131 * 97441"                                           $ nf testLenstra1 2253907771
--            , bench "Lenstra2 : 23131 * 97441"                                           $ nf testLenstra2 2253907771
--            , bench "Lenstra3 : 23131 * 97441"                                           $ nf testLenstra3 2253907771
-- 
--            , bench "Lenstra1 : 3459853037 * 9853235461"                                 $ nf testLenstra1 34090746634016945057
--            , bench "Lenstra2 : 3459853037 * 9853235461"                                 $ nf testLenstra2 34090746634016945057
--            , bench "Lenstra3 : 3459853037 * 9853235461"                                 $ nf testLenstra3 34090746634016945057
-- 
--            , bench "Lenstra1 : 2345834957 * 934589353273007"                            $ nf testLenstra1 2192392375347842185105699
--            , bench "Lenstra2 : 2345834957 * 934589353273007"                            $ nf testLenstra2 2192392375347842185105699
--            , bench "Lenstra3 : 2345834957 * 934589353273007"                            $ nf testLenstra3 2192392375347842185105699
-- 
--            , bench "Lenstra1 : 234509283489427 * 938942893412861"                       $ nf testLenstra1 220190825171739459583039320647
--            , bench "Lenstra2 : 234509283489427 * 938942893412861"                       $ nf testLenstra2 220190825171739459583039320647
--            , bench "Lenstra3 : 234509283489427 * 938942893412861"                       $ nf testLenstra3 220190825171739459583039320647
-- 
--            , bench "Lenstra1 : 68480406462161287469 * 768614336404564651"               $ nf testLenstra1 52635022169628958833592981554606658319
--            , bench "Lenstra2 : 52635022169628958833592981554606658319"                  $ nf testLenstra2 52635022169628958833592981554606658319
--            , bench "Lenstra3 : 52635022169628958833592981554606658319"                  $ nf testLenstra3 52635022169628958833592981554606658319
-- 
--            , bench "Lenstra1 : 1579057808224947489159455409497076163663656780411374497" $ nf testLenstra1 1579057808224947489159455409497076163663656780411374497
--            , bench "Lenstra2 : 1579057808224947489159455409497076163663656780411374497" $ nf testLenstra2 1579057808224947489159455409497076163663656780411374497
--            , bench "Lenstra3 : 1579057808224947489159455409497076163663656780411374497" $ nf testLenstra3 1579057808224947489159455409497076163663656780411374497
-- 
--            , bench "Pollards: 23131 * 97441"                                            $ nf testPollards 2253907771   
--            , bench "Pollards: 3459853037 * 9853235461"                                  $ nf testPollards 34090746634016945057  
--            , bench "Pollards: 2345834957 * 934589353273007"                             $ nf testPollards 2192392375347842185105699
--            , bench "Pollards: 234509283489427 * 938942893412861"                        $ nf testPollards 220190825171739459583039320647
--            , bench "Pollards: 52635022169628958833592981554606658319"                   $ nf testPollards 52635022169628958833592981554606658319
--            , bench "Pollards: 1579057808224947489159455409497076163663656780411374497"  $ nf testPollards 1579057808224947489159455409497076163663656780411374497
--          ] 
--   where
--     testPollards = (flip pollards) 100000000 
--     testLenstra1 = runLenstra (mkStdGen 42)
--     testLenstra2 = runLenstra (mkStdGen 25)
--     testLenstra3 = runLenstra (mkStdGen 348726)


-- main :: IO ()
-- main = mapM_ benchFactorisation [ 1333333333333333333333 :: Integer
--                                 --, 6080003382493017003919 :: Integer
--                                 --, 194920496263521028482429080527 :: Integer
--                                 --, 20401106006396526450232252303 :: Integer -- (188748146801 :: Integer) * (108086391056891903 :: Integer)
--                                 --, 1080863899760280032397200756891903 :: Integer -- (9999999900000001 :: Integer) * (108086391056891903 :: Integer)
--                                 --, 52635022169628958833592981554606658319 :: Integer -- (768614336404564651 :: Integer)  *  (68480406462161287469 :: Integer)
--                                 --, !257453527804370787467302518623219878314457 :: Integer --(18987964267331664557 :: Integer) * (13558774610046711780701 :: Integer)
--                                 -- , 4486305984726509142281946353450748524133119509 :: Integer -- (23768741896345550770650537601358309 :: Integer) * (188748146801 :: Integer)
--                                 --, !45671926166590716193865025092610967840375526741 :: Integer -- (226673591177742970257407 :: Integer) *  (201487636602438195784363 :: Integer) 
--                                 , 12405555718893576339315513693746127723745848097837 :: Integer -- 66241160488780141071579864797 * 187278659180417234321
--                                 , 28147864812044107759956774833241355317766711881179 :: Integer
--                                 , 1579057808224947489159455409497076163663656780411374497 :: Integer
--                                 ]
--                                 -- [ 23502345343 * 8462047541
--                                 -- , 63955294699 * 1738934833453
--                                 -- , 4826501740275275989 * 308523485829329342371
--                                 -- ]
--   where 
--     -- tests :: [Integer]
--     -- tests = [ (1333333333333333333333, 200, 100)
--     --         , ((10^8+7)*(9*10^8+11), 200, 100)
--     --         , (10^20+699, 500, 150)
--     --         , (10^30+427, 1000, 500)
--     --         , (1000000000000000003 * 10000000000000000051, 3000, 5000)
--     --         , (1579057808224947489159455409497076163663656780411374497, 500, 1000)
--     --         ]
-- 

main = mapM_ benchFactorisation tests
  where 
    tests = [ 2253907771
            --, 34090746634016945057
            --, 2192392375347842185105699
            , 408690345942924410509381586819
            --, 37474679579109250157949172138642181
            --, 52635022169628958833592981554606658319
            ]



benchFactorisation :: Integer -> IO ()
benchFactorisation n = do 
     putStrLn $ "Factorising " ++ (show n)
 
     putStrLn $ "    With Pollards:"
     t <- getCurrentTime
     putStrLn $ "       Result: " ++ (formatResult $ pollards n 50000000)
     putStrLn $ "       Result: " ++ (formatResult $ pollards n 50000000)
     putStrLn $ "       Result: " ++ (formatResult $ pollards n 50000000)
     t' <- getCurrentTime
     putStrLn $ "   Time: " ++ (show $ (diffUTCTime t' t) / 3)
 
     putStrLn $ "  With Lenstras:"
     t <- getCurrentTime
     putStrLn $ "       Result: " ++ (formatResult $ runLenstra (mkStdGen 42) n)
     putStrLn $ "       Result: " ++ (formatResult $ runLenstra (mkStdGen 42) n)
     putStrLn $ "       Result: " ++ (formatResult $ runLenstra (mkStdGen 42) n)
     t' <- getCurrentTime
     putStrLn $ "   Time: " ++ (show $ (diffUTCTime t' t) / 3)
   where 
     formatResult :: Maybe Integer -> String
     formatResult res = fromMaybe "FAILED" (show <$> res) 
 