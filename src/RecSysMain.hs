{-# LANGUAGE PackageImports, RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module Main (main) where

import Data.Foldable
import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Csv as Csv
import System.Console.GetOpt
import Control.Monad
import Data.Maybe
import qualified "unordered-containers" Data.HashMap.Strict as HM
import Train
import Data.List
import System.Exit
import System.IO
import qualified Data.ByteString.Lazy as BSL
import System.Environment
import Common
import Data.Char
import Control.Monad.Random (evalRandIO)
import RecSys

type RawUser = Integer
type RawItem = Integer

data Options = Options  { optInput :: [(User, Item, Rating)]
                        , optInputV :: Maybe ([(User, Item, Rating)])
                        , optTest :: Maybe ([(Int, User, Item)], [(Int, Rating)] -> IO ())
                        , optOutput :: String -> IO ()
                        , optC :: !Double
                        , optK :: !Int
                        , optT :: !Int
                        , optAlgo :: Algo
                        , optVerbose :: Bool
                        , optUMap :: HM.HashMap RawUser User
                        , optIMap :: HM.HashMap RawItem Item
                        , optSvdConfig :: SvdTrainConfig
                        }

startOptions = Options { optInput = undefined
                       , optInputV = Nothing
                       , optTest = Nothing
                       , optOutput = putStr
                       , optC = 0.1
                       , optK = 10
                       , optT = 10
                       , optAlgo = undefined
                       , optVerbose = False
                       , optUMap = HM.empty
                       , optIMap = HM.empty
                       , optSvdConfig = SvdTrainConfig { svdMaxIter = 100000
                                                       , svdDim = 4
                                                       , svdIMax = 0
                                                       , svdUMax = 0
                                                       , svdL4 = 10
                                                       , svdSmoothness = 0.01
                                                       , svdTempo = 10
                                                       , svdPrec = 0.01
                                                       , svdRateBase = 1
                                                       , svdL5 = 10
                                                       }
                       }


--data SvdTrainConfig = SvdTrainConfig { svdMaxIter :: Int
--                                     , svdDim :: Int
--                                     , svdIMax :: Int
--                                     , svdUMax :: Int
--                                     , svdL4 :: Double
--                                     , svdSmoothness :: Double
--                                     , svdTempo :: Double
--                                     , svdPrec :: Double
--                                     }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"] (ReqArg inputArgParser "FILE") "Input file"
    , Option "v" ["validation"] (ReqArg inputVArgParser "FILE") "Validation input file (optional)"
    , Option "u" ["test"] (ReqArg testArgParser "FILE") "Test input file"
    , Option "a" ["algo"] (ReqArg algoArgParser "ALGO") "Algo: svd1, svd2"
    --, Option "c" [] (ReqArg (doubleArgParser $ \v o -> o { optC = v })  "C") $ "svm balance parameter"
    , Option "s" [] (ReqArg svdArgParser "Svd config") $ "svd params, in form maxIter:dim:l4:smoothness:tempo:prec:base:l5"
    , Option "t" [] (ReqArg (intArgParser $ \v o -> o { optT = v })  "T") $ "t param"
    , Option "k" [] (ReqArg (intArgParser $ \v o -> o { optK = v })  "K") $ "k param"
    , Option "h" ["help"] (NoArg $ const helpMsgPrinter) "Show help"
    , Option "" ["verbose"] (NoArg $ (\os -> return os {optVerbose = True })) ""
    ]
  where
    splitter = split (\x -> isSpace x || x == ':')
    svdArgParser arg opts = let (m:d:l4:s:t:p:b:l5:[]) = splitter arg
                                s' = (optSvdConfig opts) { svdMaxIter = read m
                                                       , svdDim = read d
                                                       , svdL4 = read l4
                                                       , svdSmoothness = read s
                                                       , svdTempo = read t
                                                       , svdPrec = read p
                                                       , svdRateBase = read b
                                                       , svdL5 = read l5
                                                       }
                             in return $ opts { optSvdConfig = s' }
    doubleArgParser f arg = return . f (read arg :: Double)
    intArgParser f arg = return . f (read arg :: Int)
    inputArgParser arg opts = inputParser arg opts (\opts bs -> opts { optInput = bs }) replaceIds
    testArgParser arg opts = inputParser arg opts (\opts bs -> opts { optTest = Just (bs, BSL.writeFile (arg ++ ".out") . Csv.encode) }) replaceIds'
    inputVArgParser arg opts = inputParser arg opts (\opts bs -> opts { optInputV = Just bs }) replaceIds
    algoArgParser arg opt = return $ opt { optAlgo = case arg of
                                            "svd1" -> Svd1Algo
                                            "svd2" -> Svd2Algo
                                            "svdPP" -> SvdPPAlgo
                                            "svdPP2" -> SvdPP2Algo
                                            _ -> error $ "Unknown algo: " ++ arg
                                         }
    helpMsgPrinter = do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess

inputParser arg opts lens replacer = BSL.readFile arg >>= readCsv
                                      >>= \bs -> let ((um', im'), bs') = bs `deepseq` replacer (um, im) bs
                                                     um = optUMap opts
                                                     im = optIMap opts
                                                     s' = (optSvdConfig opts) { svdIMax = HM.size im'
                                                                              , svdUMax = HM.size um'
                                                                              }
                                                  in s' `seq` return $
                                                    lens (opts { optUMap = um'
                                                               , optIMap = im'
                                                               , optSvdConfig = s'
                                                               }) bs'
replaceId m u = case HM.lookup u m of
      Just v -> (v, m)
      Nothing -> let v = HM.size m
                  in (v, HM.insert u v m)

replaceIds :: (HM.HashMap RawUser User, HM.HashMap RawItem Item) -> [(RawUser, RawItem, Rating)] -> ((HM.HashMap RawUser User, HM.HashMap RawItem Item), [(User, Item, Rating)])
replaceIds rM = foldr' (\(u, i, r) ((m1, m2), bs)
                         -> let (u', m1') = replaceId m1 u
                                (i', m2') = replaceId m2 i
                             in u' `seq` i' `seq` m1' `seq` m2' `seq` ((m1', m2'), (u', i', r) : bs)) (rM, [])
replaceIds' :: (HM.HashMap RawUser User, HM.HashMap RawItem Item) -> [(Int, RawUser, RawItem)] -> ((HM.HashMap RawUser User, HM.HashMap RawItem Item), [(Int, User, Item)])
replaceIds' rM = foldr' (\(id, u, i) ((m1, m2), bs)
                         -> let (u', m1') = replaceId m1 u
                                (i', m2') = replaceId m2 i
                             in u' `seq` i' `seq` m1' `seq` m2' `seq` ((m1', m2'), (id, u', i') : bs)) (rM, [])
readCsv :: (Csv.FromRecord a, Monad m) => BSL.ByteString -> m [a]
readCsv = either fail (return . V.toList) . Csv.decode Csv.HasHeader

runAlgo :: Algo -> Options -> IO ()
runAlgo Svd1Algo opts = runAlgo' AlgoConfig { testConfig = svdTestConfig $ optSvdConfig opts
                                            , rate = svd
                                            , splitter = dummySplitter
                                            } opts
runAlgo Svd2Algo opts = runAlgo' AlgoConfig { testConfig = svdTestConfig $ optSvdConfig opts
                                            , rate = svd
                                            , splitter = tkFoldCv (optT opts) (optK opts)
                                            } opts
runAlgo SvdPPAlgo opts = runAlgo' AlgoConfig { testConfig = svdPPTestConfig $ optSvdConfig opts
                                             , rate = svdPP
                                             , splitter = dummySplitter
                                             } opts
runAlgo SvdPP2Algo opts = runAlgo' AlgoConfig { testConfig = svdPPTestConfig $ optSvdConfig opts
                                             , rate = svdPP
                                             , splitter = tkFoldCv (optT opts) (optK opts)
                                             } opts

runAlgo' :: Show c => AlgoConfig c -> Options -> IO ()
runAlgo' algo opts = do (c, q) <- case optInputV opts of
                         Just vInput -> do c <- evalRandIO $ learn' conf splitter' $ optInput opts
                                           return (c, test conf vInput c)
                         Nothing -> evalRandIO $ learn conf (splitter algo) 0.2 $ optInput opts
                        optOutput opts $ (++ "\n") $ show $ q
                        when (optVerbose opts) $
                           optOutput opts $ (++ "\n") $ show $ (c, q)
                        case optTest opts of
                          Just (tInput, tOutput) -> tOutput $ test' c $ tInput
                          Nothing -> return ()
     where conf = testConfig algo
           test' c = map (\(id, u, i) -> (id, rate algo c u i))
           splitter' = splitter algo

data Algo = Svd1Algo | Svd2Algo | SvdPPAlgo | SvdPP2Algo
  deriving Show

data AlgoConfig c = AlgoConfig { testConfig :: TestConfig (User, Item, Rating) c Double
                               , splitter :: RandSplitter (User, Item, Rating)
                               , rate :: c -> User -> Item -> Rating
                               }

main = do
    args <- getArgs
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt Permute options args
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    putStrLn $ "Algo: " ++ (show $ optAlgo opts)

    runAlgo (optAlgo opts) opts
