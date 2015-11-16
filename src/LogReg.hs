module LogReg where
import Train
import Common
import qualified Data.Array as A

data LogRegTrainConfig = LogRegTrainConfig { lrPrec :: Double
                                           , lrMaxIter :: Int
                                           , lrH :: Double
                                           }

logRegTestConfig :: Point p => Double -> TestConfig (p, Class) (LinClassConfig p) Double
logRegTestConfig h = TestConfig { train = lrTrain h
                                , test = classifierTest linearClassifier fScore
                                , finalTestCoef = 0.2
                                , finalTest = classifierTest linearClassifier fScore
                                }


lrTrain :: (Point p) => Double -> [(p, Class)] -> RandMonad (LinClassConfig p)
lrTrain h = sgd' sgdConfig
  where lossF x = logBase 2 $ 1 + exp ( negate x )
        lossF' x = recip $ 1 + exp ( negate x )
        sgdConfig  = SGDConfig { sgdLoss = lossF
                               , sgdLoss' = lossF'
                               , sgdTempo = h
                               , sgdSmoothness = 0.1
                               , sgdPrec = 0.001
                               , sgdMaxIter = 10000
                               }

