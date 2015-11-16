{-# LANGUAGE PackageImports, RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module Main (main) where

import System.Console.GetOpt
import Control.Monad
import LogReg
import Data.Maybe
import qualified "hashmap" Data.HashMap as HM
import Train
import Data.List
import System.Exit
import System.IO
import System.Environment
import Svm
import Common
import Data.Char
import Control.Monad.Random (evalRandIO)
import Graphics.EasyPlot


data Options = Options  { optInput :: IO String
                        , optOutput :: String -> IO ()
                        , optC :: !Double
                        , optH :: !Double
                        , optG :: !Double
                        , optK :: !Int
                        , optT :: !Int
                        , optPlot :: Bool
                        , optGLC :: Maybe GaussLiftConfig
                        , optAlgo :: Options -> IO ()
                        }

startOptions = Options { optInput = getContents
                       , optOutput = putStr
                       , optH = 0.1
                       , optC = 0.1
                       , optG = 0.1
                       , optK = 10
                       , optT = 10
                       , optPlot = False
                       , optGLC = Nothing
                       , optAlgo = undefined
                       }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"] (ReqArg inputArgParser "FILE") "Input file"
    , Option "o" ["output"] (ReqArg outputArgParser "FILE") "Output file"
    , Option "a" ["algo"] (ReqArg algoArgParser "ALGO") "Algo: svm, logistic"
    , Option "c" [] (ReqArg (doubleArgParser $ \v o -> o { optC = v })  "C") $ "svm balance parameter"
    , Option "h" [] (ReqArg (doubleArgParser $ \v o -> o { optH = v })  "H") $ "stohastic gradient speed"
    , Option "t" [] (ReqArg (intArgParser $ \v o -> o { optT = v })  "T") $ "t param"
    , Option "k" [] (ReqArg (intArgParser $ \v o -> o { optK = v })  "K") $ "k param"
    , Option "g" [] (ReqArg (doubleArgParser $ \v o -> o { optG = v })  "G") $ "gaussian kernel parameter"
    , Option "l" [] (ReqArg glcArgParser "Gauss lift config") $ "lift from 2d to 3d, based on gauss lift"
    --, Option "f" [] (ReqArg sfArgParser  "Similarity Function") $ "similarity (kernel-based) function"
    , Option "h" ["help"] (NoArg $ const helpMsgPrinter) "Show help"
    , Option "p" ["plot"] (NoArg $ \opt -> return $ opt { optPlot = True } ) "Draw plot"
    ]
  where
    glcArgParser arg opt = let (g:wx:wy:bx:by:[]) = map (read :: String -> Double) $ split (\x -> isSpace x || x == ':') arg
                            in return $ opt { optGLC = Just $ GaussLiftConfig g (wx, wy) (bx, by) }
    doubleArgParser f arg = return . f (read arg :: Double)
    intArgParser f arg = return . f (read arg :: Int)
    outputArgParser :: String -> Options -> IO Options
    outputArgParser arg opt = return $ opt { optOutput = writeFile arg }
    inputArgParser :: String -> Options -> IO Options
    inputArgParser arg opt = return $ opt { optInput = readFile arg }
    --sfArgParser arg opt = case arg of
    --                        "dot" -> return $ opt { optSF = const dotProduct }
    --                        "rbf" -> return $ opt { optSF = \o -> rbf $ optG o }
    --                        s -> fail $ "Unknown similarity function: " ++ s
    algoArgParser arg opt = return $ opt { optAlgo = case arg of
                                            ('s':_) -> linClassAlgo (undefined :: SvmAlgo)
                                            ('l':_) -> linClassAlgo (undefined :: LogisticRegressionAlgo)
                                         }
    helpMsgPrinter = do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess


class Point p => PlottablePoint p where
  plotPoints :: [(p, Class)] -> LinClassConfig p -> IO ()

plot_' a b c = plot' a b c >> return ()

instance PlottablePoint DPoint3d where
  plotPoints ps (LinClassConfig (wx,wy,wz) w0) = plot_' [Interactive] X11 $ (Function3D [Style Linespoints, Color Grey] [] $ \x y -> (w0 - wx*x - wy *y) / wz) : (plots'' (\cl -> Data3D [Title $ "Graph " ++ (show cl)] []) ps)

instance PlottablePoint DPoint2d where
  plotPoints ps (LinClassConfig (wx,wy) w0) = plot_' [Interactive] X11 $ (Function2D [Style Lines] [] $ \x -> (w0 - wx*x) / wy) : (plots'' (\cl -> Data2D [Title $ "Graph " ++ (show cl)] []) ps)

plots'' cr = map (\(cl, xs) -> cr cl xs) . collectByClass

class Algo a where
  testConfig :: Point p => a -> Options -> TestConfig (p, Class) (LinClassConfig p) Double

linClassAlgo :: Algo a => a -> Options -> IO ()
linClassAlgo algo opts = optInput opts >>= chipsReadPoints'
    >>= case optGLC opts of
          Just glc -> subAlgo algo opts . map (\(x, y) -> (gaussLift3d glc x, y))
          Nothing -> subAlgo algo opts
    >> return ()

subAlgo :: (Algo a, Point p, Show p, PlottablePoint p) => a -> Options -> [(p, Class)] -> IO ()
subAlgo algo opts ps = (subAlgo' $! ps)
                  >>= \(c, q) -> ((optOutput opts $ (++ "\n") $ show c)
                  >> when (optPlot opts) (plotPoints ps c >> return ()))
  where
    subAlgo' = evalRandIO . learn (testConfig algo opts) (tkFoldCv (optT opts) (optK opts))

data SvmAlgo
instance Algo SvmAlgo where
  testConfig = const $ svmTestConfig . optC

data LogisticRegressionAlgo
instance Algo LogisticRegressionAlgo where
  testConfig = const $ logRegTestConfig . optH

main = do
    args <- getArgs
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    optAlgo opts opts
