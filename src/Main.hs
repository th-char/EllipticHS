module Main where 

import Data.Time.Clock
import System.IO
import Text.Printf
import Data.Maybe

import Lenstra
import Pollards

main :: IO ()
main = mapM_ benchFactorisation tests
                                -- [ 1333333333333333333333 :: Integer
                                -- 6080003382493017003919 :: Integer
                                --, 194920496263521028482429080527 :: Integer
                                --, 20401106006396526450232252303 :: Integer -- (188748146801 :: Integer) * (108086391056891903 :: Integer)
                                --, 1080863899760280032397200756891903 :: Integer -- (9999999900000001 :: Integer) * (108086391056891903 :: Integer)
                                --, 52635022169628958833592981554606658319 :: Integer -- (768614336404564651 :: Integer)  *  (68480406462161287469 :: Integer)
                                --, 257453527804370787467302518623219878314457 :: Integer --(18987964267331664557 :: Integer) * (13558774610046711780701 :: Integer)
                                --, 4486305984726509142281946353450748524133119509 :: Integer -- (23768741896345550770650537601358309 :: Integer) * (188748146801 :: Integer)
                                --, 45671926166590716193865025092610967840375526741 :: Integer -- (226673591177742970257407 :: Integer) *  (201487636602438195784363 :: Integer) 
                                --, 12405555718893576339315513693746127723745848097837 :: Integer -- 66241160488780141071579864797 * 187278659180417234321
                                --, 28147864812044107759956774833241355317766711881179 :: Integer
                                --, 1579057808224947489159455409497076163663656780411374497 :: Integer
                                --]
  where 
    tests :: [(Integer, Int, Integer)]
    tests = [ (1333333333333333333333, 200, 100)
            , ((10^8+7)*(9*10^8+11), 200, 100)
            , (10^20+699, 500, 150)
            , (10^30+427, 1000, 500)
            , (1000000000000000003 * 10000000000000000051, 3000, 5000)
            , (1579057808224947489159455409497076163663656780411374497, 500, 1000)
            ]

benchFactorisation :: (Integer, Int, Integer) -> IO ()
benchFactorisation (n, a, b) = do 
    putStrLn $ "Factorising " ++ (show n)

    putStrLn $ "  With Pollards:"
    t <- getCurrentTime
    putStrLn $ "   Result: " ++ (formatResult $ runLenstra n)
    t' <- getCurrentTime
    putStrLn $ "   Time: " ++ (show $ diffUTCTime t' t)

    --putStrLn $ "  With Lenstras:"
    --t <- getCurrentTime
    --putStrLn $ "   Result: " ++ (formatResult $ lenstras n a b)
    --t' <- getCurrentTime
    --putStrLn $ "   Time: " ++ (show $ diffUTCTime t' t)
  where 
    formatResult :: Maybe Integer -> String
    formatResult res = fromMaybe "FAILED" (show <$> res) 
