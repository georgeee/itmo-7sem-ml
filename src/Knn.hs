{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
module Knn where
import qualified Data.KdMap.Static as Kd
import Control.Monad.Random
import Train
import Data.Ord
import Common
import Data.List
import qualified Data.Foldable as F

kMin = 1
kMax = 20

buildKdTree :: [(DPoint2d, Class)] -> KdMap
buildKdTree = Kd.build (\(x, y) -> [x, y])

type KdMap = Kd.KdMap Double DPoint2d Class

type KnnTestConfig c = TestConfig (DPoint2d, Class) c Double

trainByK impl' = minimumBy (comparing snd) . trainByK' impl'

trainByK' :: (Int -> KdMap -> DPoint2d -> Class) -> KdMap -> [(Int, Int)]
trainByK' impl' tree = map tryK [kMin..kMax]
  where
    tryK :: Int -> (Int, Int)
    tryK !k = (k, Kd.foldrWithKey f 0 tree)
      where
        f (p, cl) = if cl == test' p then id else (+1)
        test' p = impl' k tree p

type Knn1Config = (Int, KdMap)

knn1 :: Knn1Config -> DPoint2d -> Class
knn1 (k, tree) = fst . maximumBy (comparing snd) . countUnique . map snd . Kd.kNearest tree k

knn1' :: Int -> KdMap -> DPoint2d -> Class
knn1' k tree = fst . maximumBy (comparing snd) . countUnique . map snd . tail . Kd.kNearest tree (k + 1)

knn1Train :: [(DPoint2d, Class)] -> Knn1Config
knn1Train ps = (fst $ trainByK knn1' tree, tree)
  where
    tree = buildKdTree ps

knn1TestConfig :: KnnTestConfig Knn1Config -- Trying only k
knn1TestConfig = TestConfig { train = return . knn1Train, test = classifierTest knn1 fScore, finalTestCoef = 0.2, finalTest = classifierTest knn1 fScore }

data Knn2Config = Knn2Config { knn2K :: !Int
                             , knn2G :: !Double
                             , knn2Tree :: !KdMap
                             }
    deriving Show

knn2 :: Knn2Config -> DPoint2d -> Class
knn2 (Knn2Config k g tree) = fst . maximumBy (comparing snd) . sumByClass . measurePoints
  where
    measurePoints !p = Kd.foldrWithKey f [] tree
     where kthNeighbor = fst $ last $ Kd.kNearest tree (k + 1) p
           measureP' n = kthNeighbor `seq` gaussianKernel g $ (sqrDist p n) / (sqrDist kthNeighbor p)
           f (n, cl) = ((cl, measureP' n) :)

knn2' :: Double -> Int -> KdMap -> DPoint2d -> Class
knn2' g k tree = fst . maximumBy (comparing snd) . sumByClass . measurePoints
  where
    measurePoints !p = Kd.foldrWithKey f [] tree
     where kthNeighbor = fst $ last $ Kd.kNearest tree (k + 2) p
           measureP' n = kthNeighbor `seq` gaussianKernel g $ (sqrDist p n) / (sqrDist kthNeighbor p)
           f (n, cl) | n /= p  = ((cl, measureP' n) :)
                     | otherwise = id

knn2Train :: Double -> [(DPoint2d, Class)] -> Knn2Config
knn2Train g = fst . knn2Train' g

knn2Train' !g !ps = (tree `seq` Knn2Config cl g tree, q)
  where
    (cl, q) = trainByK (knn2' g) tree
    tree = buildKdTree ps

knn2TestConfig :: Double -> KnnTestConfig Knn2Config
knn2TestConfig g = TestConfig { train = return . knn2Train g, test = classifierTest knn2 fScore, finalTestCoef = 0.2, finalTest = classifierTest knn2 fScore }

knn3Train :: Int -> [(DPoint2d, Class)] -> RandMonad Knn2Config
knn3Train gc ps = getRandomRs (0, 1) >>= return . fst . minimumBy (comparing snd) . map (flip knn2Train' ps) . take gc

knn3TestConfig :: Int -> KnnTestConfig Knn2Config
knn3TestConfig gc = TestConfig { train = knn3Train gc, test = classifierTest knn2 fScore, finalTestCoef = 0.2, finalTest = classifierTest knn2 fScore }


