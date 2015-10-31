{-# LANGUAGE PackageImports #-}
module Main (main) where

import System.Console.GetOpt
import Control.Monad
import Data.Maybe
import qualified "hashmap" Data.HashMap as HM
import Train
import Data.List
import System.Exit
import System.IO
import System.Environment
import Svm
import Common
import Control.Monad.Random
import Graphics.EasyPlot


data Options = Options  { optInput :: IO String
                        , optOutput :: String -> IO ()
                        , optC :: !Double
                        , optG :: !Double
                        , optK :: !Int
                        , optT :: !Int
                        , optSF :: Options -> SimilarityFunction DPoint2d
                        }

startOptions = Options { optInput = getContents
                       , optOutput = putStr
                       , optC = 0.1
                       , optG = 0.1
                       , optK = 10
                       , optT = 10
                       , optSF = const dotProduct
                       }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"] (ReqArg inputArgParser "FILE") "Input file"
    , Option "o" ["output"] (ReqArg outputArgParser "FILE") "Output file"
    , Option "c" [] (ReqArg (doubleArgParser $ \v o -> o { optC = v })  "C") $ "svm balance parameter"
    , Option "t" [] (ReqArg (intArgParser $ \v o -> o { optT = v })  "T") $ "t param"
    , Option "k" [] (ReqArg (intArgParser $ \v o -> o { optK = v })  "K") $ "k param"
    , Option "g" [] (ReqArg (doubleArgParser $ \v o -> o { optG = v })  "G") $ "gaussian kernel parameter"
    , Option "f" [] (ReqArg sfArgParser  "Similarity Function") $ "similarity (kernel-based) function"
    , Option "h" ["help"] (NoArg $ const helpMsgPrinter) "Show help"
    ]
  where
    doubleArgParser f arg = return . f (read arg :: Double)
    intArgParser f arg = return . f (read arg :: Int)
    outputArgParser :: String -> Options -> IO Options
    outputArgParser arg opt = return $ opt { optOutput = writeFile arg }
    inputArgParser :: String -> Options -> IO Options
    inputArgParser arg opt = return $ opt { optInput = readFile arg }
    sfArgParser arg opt = case arg of
                            "dot" -> return $ opt { optSF = const dotProduct }
                            "rbf" -> return $ opt { optSF = \o -> rbf $ optG o }
                            s -> fail $ "Unknown similarity function: " ++ s
    helpMsgPrinter = do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess

generalAlgo :: Show a => (String -> IO b) -> (b -> IO a) -> Options -> IO ()
generalAlgo reader f opts = optInput opts >>= reader >>= \s -> (f $! s) >>= optOutput opts . (++ "\n")  . show

svmAlgo opts = optInput opts >>= chipsReadPoints'
    >>= \ps -> (algo $! ps) >>= \res -> do optOutput opts $ (++ "\n") $ show res
                                           plots ps res
  where algo = evalRandIO . learn testConfig (tkFoldCv (optT opts) (optK opts))
        plots ps res = do let (SvmConfig ws w sf, _) = res
                          plot X11 $ origPointGraphs ps
        testConfig = svmTestConfig (optC opts) (optSF opts opts)
        origPointGraphs = map (\(cl, xs) -> Data2D [Title $ "Graph " ++ (show cl)] [] xs) . collectByClass


main = do
    args <- getArgs
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    svmAlgo opts
