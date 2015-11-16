{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
module Svm where

import Data.Array.Repa (fromListUnboxed, Z(..), (:.)(..))
import Smo
import Common
import Train
import qualified Data.Vector.Unboxed         as UV

svmTestConfig :: Point p => Double -> TestConfig (p, Class) (LinClassConfig p) Double
svmTestConfig c = TestConfig { train = return . smo c
                             , test = classifierTest linearClassifier fScore
                             , finalTestCoef = 0.2
                             , finalTest = classifierTest linearClassifier fScore
                             }

smo :: (Point p) => Double -> [(p, Class)] -> LinClassConfig p
smo c ps = LinClassConfig w w0
    where label = UV.fromList $ map snd ps
          matrix = buildMatrix dotProduct ps
          (w0, alphaUV) = smoC c c label matrix
          alpha = UV.toList alphaUV
          alpha' = filter ((>0) . fst) $ zip alpha ps
          w = foldl (\b (a, (p, c)) -> b `pAdd` a `pMul` c `pMul'` p) pNull alpha'

buildMatrix :: (Point p) => SimilarityFunction p -> [(p, Class)] -> Matrix Double
buildMatrix f ls = fromListUnboxed (Z :. len :. len) $ concat $ map (flip map ls . impl') ls
  where len = length ls
        impl' (pi, ci) (pj, cj) = (realToFrac $ ci * cj) * (f pi pj)
