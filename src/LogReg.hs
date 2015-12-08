module LogReg where
import Train
import ClassCommon
import Linear
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
                                }


lrTrain :: (Point p) => LRTrainConfig -> [(p, Class)] -> RandMonad (LinClassConfig p)
lrTrain c = lsgd' lsgdConfig
  where lossF x = log $ 1 + exp ( negate x )
        lossF' x = negate $ recip $ 1 + exp x
        lsgdConfig  = LSGDConfig { lsgdLoss = lossF
                                 , lsgdLoss' = lossF'
                                 , lsgdTempo = lrTempo c
                                 , lsgdSmoothness = lrSmoothness c
                                 , lsgdPrec = lrPrec c
                                 , lsgdMaxIter = lrMaxIter c
                                 }

