module LogReg where
import Train
import Common
import qualified Data.Array as A

data LRTrainConfig = LRTrainConfig { lrPrec :: !Double
                                   , lrMaxIter :: !Int
                                   , lrTempo :: !Double
                                   , lrSmoothness :: !Double
                                   }

logRegTestConfig :: Point p => LRTrainConfig -> TestConfig (p, Class) (LinClassConfig p) Double
logRegTestConfig c = TestConfig { train = lrTrain c
                                , test = classifierTest linearClassifier fScore
                                , finalTestCoef = 0.2
                                , finalTest = classifierTest linearClassifier fScore
                                }


lrTrain :: (Point p) => LRTrainConfig -> [(p, Class)] -> RandMonad (LinClassConfig p)
lrTrain c = sgd' sgdConfig
  where lossF x = log $ 1 + exp ( negate x )
        lossF' x = negate $ recip $ 1 + exp x
        sgdConfig  = SGDConfig { sgdLoss = lossF
                               , sgdLoss' = lossF'
                               , sgdTempo = lrTempo c
                               , sgdSmoothness = lrSmoothness c
                               , sgdPrec = lrPrec c
                               , sgdMaxIter = lrMaxIter c
                               }

