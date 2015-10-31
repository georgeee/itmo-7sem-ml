{-# LANGUAGE PackageImports #-}
module Main (main) where

import System.Console.GetOpt
import Control.Monad
import Data.Maybe
import qualified Data.KdMap.Static as Kd
import qualified "hashmap" Data.HashMap as HM
import Train
import Data.List
import System.Exit
import System.IO
import System.Environment
import Knn
import Common
import Control.Monad.Random


data Options = Options  { optAlgo :: Options -> IO ()
                        , optInput :: IO String
                        , optOutput :: String -> IO ()
                        , optG :: !Double
                        , optK :: !Int
                        , optT :: !Int
                        , optGC :: !Int
                        }

startOptions = Options { optAlgo = undefined
                       , optInput = getContents
                       , optOutput = putStr
                       , optK = 10
                       , optT = 10
                       , optG = 0.1
                       , optGC = 10
                       }

generalAlgo :: Show a => (String -> IO b) -> (b -> IO a) -> Options -> IO ()
generalAlgo reader f opts = optInput opts >>= reader >>= \s -> (f $! s) >>= optOutput opts . (++ "\n")  . show

algos :: HM.Map String (Options -> IO ())
algos = HM.fromList [ ("knn1", knn1Algo)
                    , ("knn2", knn2Algo)
                    , ("knn3", knn3Algo)
                    ]

knnAlgo :: Show conf' => KnnTestConfig conf -> ((conf, Double) -> conf') -> Options -> IO ()
knnAlgo testConfig printHelper opts = generalAlgo chipsReadPoints (evalRandIO . algo) opts
  where algo = learn testConfig (tkFoldCv (optT opts) (optK opts)) >=> return . printHelper

knn1Algo = knnAlgo knn1TestConfig forPrint
  where
    forPrint ((k, t), e) = (Kd.assocs t, k, e)

knn2Algo opts = knnAlgo (knn2TestConfig g) forPrint opts
  where
    g = optG opts
    forPrint (Knn2Config k g' t, e) = (Kd.assocs t, k, g', e)

knn3Algo opts = knnAlgo (knn3TestConfig gc) forPrint opts
  where
    gc = optGC opts
    forPrint (Knn2Config k g' t, e) = (Kd.assocs t, k, g', e)

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"] (ReqArg inputArgParser "FILE") "Input file"
    , Option "o" ["output"] (ReqArg outputArgParser "FILE") "Output file"
    , Option "a" ["algo"] (ReqArg algoArgParser "ALGO") $ "Algo to use, choose one of: " ++ (intercalate ", " (HM.keys algos))
    , Option "t" [] (ReqArg (intArgParser $ \v o -> o { optT = v })  "T") $ "t param"
    , Option "k" [] (ReqArg (intArgParser $ \v o -> o { optK = v })  "K") $ "k param"
    , Option "c" [] (ReqArg (intArgParser $ \v o -> o { optGC = v })  "GC") $ "gaussian kernel param tryout count"
    , Option "g" [] (ReqArg (doubleArgParser $ \v o -> o { optG = v })  "G") $ "gaussian kernel param"
    , Option "h" ["help"] (NoArg $ const helpMsgPrinter) "Show help"
    ]
  where
    doubleArgParser f arg = return . f (read arg :: Double)
    intArgParser f arg = return . f (read arg :: Int)
    outputArgParser :: String -> Options -> IO Options
    outputArgParser arg opt = return $ opt { optOutput = writeFile arg }
    inputArgParser :: String -> Options -> IO Options
    inputArgParser arg opt = return $ opt { optInput = readFile arg }
    algoArgParser :: String -> Options -> IO Options
    algoArgParser arg opt = maybe (fail $ "No such algo: " ++ arg) (\algo -> return opt { optAlgo = algo }) (HM.lookup arg algos)
    helpMsgPrinter = do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess

main = do
    args <- getArgs
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    (optAlgo opts) opts
