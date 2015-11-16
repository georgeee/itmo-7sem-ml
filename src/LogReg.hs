module LogReg where

import Train
import Common
import qualified Data.Array as A

data LogRegTrainConfig = LogRegTrainConfig { lrPrec :: Double
                                           , lrMaxIter :: Int
                                           , lrH :: Double
                                           }

logRegTestConfig :: Point p => Double -> TestConfig (p, Class) (LinClassConfig p) Double
logRegTestConfig h = TestConfig { train = lrTrain $ LogRegTrainConfig { lrPrec = 0.001, lrMaxIter = 10000, lrH = h }
                                , test = classifierTest linearClassifier fScore
                                , finalTestCoef = 0.2
                                , finalTest = classifierTest linearClassifier fScore
                                }


lrTrain :: (Point p) => LogRegTrainConfig -> [(p, Class)] -> RandMonad (LinClassConfig p)
lrTrain c points = fmap fst <$> lrTrain' (lrMaxIter c) pNull $ sum (lossF' pNull) points
  where h = lrH c
        p = lrPrec c
        ps = A.listArray (0, (length points - 1)) points
        lossF x = logBase 2 $ 1 + exp ( negate x )
        lossF' w (x, y) = lossF $ (dotProduct w x) * y
        lrTrain' = undefined

