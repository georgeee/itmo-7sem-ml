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
                        , optLRTrainConfig :: LRTrainConfig
                        , optK :: !Int
                        , optT :: !Int
                        , optPlot :: Bool
                        , optGLC :: Maybe GaussLiftConfig
                        , optAlgo :: Algo
                        }

startOptions = Options { optInput = getContents
                       , optOutput = putStr
                       , optLRTrainConfig = LRTrainConfig { lrPrec = 0.001, lrMaxIter = 10000, lrTempo = 0.001, lrSmoothness = 0.001 }
                       , optC = 0.1
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
    , Option "l" [] (ReqArg lrcArgParser "Linear regression config") $ "stohastic gradient params, in form precision:maxIter:tempo:smoothness"
    , Option "t" [] (ReqArg (intArgParser $ \v o -> o { optT = v })  "T") $ "t param"
    , Option "k" [] (ReqArg (intArgParser $ \v o -> o { optK = v })  "K") $ "k param"
    , Option "g" [] (ReqArg glcArgParser "Gauss lift config") $ "lift from 2d to 3d, based on gauss lift, in form g:wx:wy:bx:by, wx:wy - ratios for x and y coordinates, bx:by - base point"
    , Option "h" ["help"] (NoArg $ const helpMsgPrinter) "Show help"
    , Option "p" ["plot"] (NoArg $ \opt -> return $ opt { optPlot = True } ) "Draw plot"
    ]
  where
    splitter = split (\x -> isSpace x || x == ':')
    glcArgParser arg opt = let (g:wx:wy:bx:by:[]) = splitter arg
                            in return $ opt { optGLC = Just $ GaussLiftConfig (read g) (read wx, read wy) (read bx, read by) }
    lrcArgParser arg opt = let (p:m:t:s:[]) = splitter arg
                            in return $ opt { optLRTrainConfig = LRTrainConfig { lrPrec = read p, lrMaxIter = read m, lrTempo = read t, lrSmoothness = read s } }
    doubleArgParser f arg = return . f (read arg :: Double)
    intArgParser f arg = return . f (read arg :: Int)
    outputArgParser :: String -> Options -> IO Options
    outputArgParser arg opt = return $ opt { optOutput = writeFile arg }
    inputArgParser :: String -> Options -> IO Options
    inputArgParser arg opt = return $ opt { optInput = readFile arg }
    algoArgParser arg opt = return $ opt { optAlgo = case arg of
                                            ('s':_) -> SvmAlgo
                                            ('l':_) -> LogRegAlgo
                                            _ -> error $ "Unknown algo: " ++ arg
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

linClassAlgo :: Algo -> Options -> IO ()
linClassAlgo algo opts = optInput opts >>= chipsReadPoints'
    >>= case optGLC opts of
          Just glc -> subAlgo algo opts . map (\(x, y) -> (gaussLift3d glc x, y))
          Nothing -> subAlgo algo opts
    >> return ()

subAlgo :: (Point p, Show p, PlottablePoint p) => Algo -> Options -> [(p, Class)] -> IO ()
subAlgo algo opts ps = (subAlgo' $! ps)
                  >>= \(c, q) -> ((optOutput opts $ (++ "\n") $ show $ (c, q))
                  >> when (optPlot opts) (plotPoints ps c >> return ()))
  where
    subAlgo' = evalRandIO . learn (testConfig algo opts) (tkFoldCv (optT opts) (optK opts))

data Algo = SvmAlgo | LogRegAlgo
  deriving Show

testConfig :: Point p => Algo -> Options -> TestConfig (p, Class) (LinClassConfig p) Double
testConfig SvmAlgo = svmTestConfig . optC
testConfig LogRegAlgo = logRegTestConfig . optLRTrainConfig

main = do
    args <- getArgs
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt Permute options args
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    putStrLn $ "Algo: " ++ (show $ optAlgo opts)

    linClassAlgo (optAlgo opts) opts
