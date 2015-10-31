{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
module Svm where

import Data.Array.Repa (fromListUnboxed, Z(..), (:.)(..))
import Smo
import Common
import Train
import qualified Data.Vector.Unboxed         as UV

data SvmConfig p = SvmConfig { scWs :: [p], scW0 :: Double, scSimF :: SimilarityFunction p }

instance Show p => Show (SvmConfig p) where
  show (SvmConfig ws w0 _) = "ws = " ++ (show ws) ++ ", w0 = " ++ (show w0)

svmTestConfig :: Point p => Double -> SimilarityFunction p -> TestConfig (p, Class) (SvmConfig p) Double
svmTestConfig c sf = TestConfig { train = return . smo sf c
                                , test = classifierTest svm fScore
                                , finalTestCoef = 0.2
                                , finalTest = classifierTest svm fScore
                                }

svm :: (Point p) => SvmConfig p -> p -> Class
svm (SvmConfig ws w0 sf) p = sgn $ (sum $ map (sf p) ws) - w0
  where sgn x | x < 0 = -1
              | x > 0 = 1
              | otherwise = 0

smo :: (Point p) => SimilarityFunction p -> Double -> [(p, Class)] -> SvmConfig p
smo sf c ps = SvmConfig ws w0 sf
    where label = UV.fromList $ map snd ps
          matrix = buildMatrix sf ps
          (w0, alphaUV) = smoC c c label matrix
          alpha = UV.toList alphaUV
          alpha' = filter ((>0) . fst) $ zip alpha ps
          ws = map (\(a, (p, c)) -> a `pMul` c `pMul'` p) alpha'

buildMatrix :: (Point p) => SimilarityFunction p -> [(p, Class)] -> Matrix Double
buildMatrix f ls = fromListUnboxed (Z :. len :. len) $ concat $ map (flip map ls . impl') ls
  where len = length ls
        impl' (pi, ci) (pj, cj) = (realToFrac $ ci * cj) * (f pi pj)
