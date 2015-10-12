{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
module HW1 where
import qualified Data.KdMap.Static as Kd
import Control.Monad.Random
import Train
import Data.Ord
import Common
import Data.List
import qualified Data.Foldable as F
import qualified "hashmap" Data.HashSet as HS
import qualified "hashmap" Data.HashMap as HM

type Class = Int

kMin = 1
kMax = 20

type RequestPoint = (Double, Double)

buildKdTree :: [(RequestPoint, Class)] -> KdMap
buildKdTree = Kd.build (\(x, y) -> [x, y])
sqrDist = Kd.defaultSqrDist (\(x, y) -> [x, y])

type KdMap = Kd.KdMap Double RequestPoint Class

tuple3To2 (x, y, _) = (x, y)
tuple2To3 z (x, y) = (x, y, z)


dpx (x, _, _) = x
dpy (_, y, _) = y
dpc (_, _, c) = c



type KnnTestConfig c = TestConfig (RequestPoint, Class) c Double

knnTest :: (conf -> RequestPoint -> Class) -> [(RequestPoint, Class)] -> conf -> Double
knnTest impl ps conf = sScore list
  where f (p, cl) = (impl conf p, cl)
        list = map f ps

sScore ps = (fromIntegral $ length $ filter id $ map (\(x, y) -> x == y) ps) / (fromIntegral $ length ps)

-- [(guessed class, real class)]
fScore :: [(Class, Class)] -> Double
fScore list = if isNaN val then 0 else val
  where
    val = 2 * (prec * recall) / (prec + recall)
    classes = HS.toList $ HS.fromList (map fst list) `HS.union` HS.fromList (map snd list)
    prec = errorAvg $ map prec' classes
    recall = errorAvg $ map recall' classes
    prec' :: Class -> Double
    prec' c = (a c c) / (fromIntegral $ sum $ map (a c) classes)
    recall' :: Class -> Double
    recall' c = (a c c) / (fromIntegral $ sum $ map (flip a c) classes)
    a u v = fromIntegral $ length $ filter (== (u, v)) list


trainByK impl' = minimumBy (comparing snd) . trainByK' impl'

trainByK' :: (Int -> KdMap -> RequestPoint -> Class) -> KdMap -> [(Int, Int)]
trainByK' impl' tree = map tryK [kMin..kMax]
  where
    tryK :: Int -> (Int, Int)
    tryK !k = (k, Kd.foldrWithKey f 0 tree)
      where
        f (p, cl) = if cl == test' p then id else (+1)
        test' p = impl' k tree p

type Knn1Config = (Int, KdMap)

knn1 :: Knn1Config -> RequestPoint -> Class
knn1 (k, tree) = fst . maximumBy (comparing snd) . countUnique . map snd . Kd.kNearest tree k

knn1' :: Int -> KdMap -> RequestPoint -> Class
knn1' k tree = fst . maximumBy (comparing snd) . countUnique . map snd . tail . Kd.kNearest tree (k + 1)

knn1Train :: [(RequestPoint, Class)] -> Knn1Config
knn1Train ps = (fst $ trainByK knn1' tree, tree)
  where
    tree = buildKdTree ps

knn1TestConfig :: KnnTestConfig Knn1Config -- Trying only k
knn1TestConfig = TestConfig { train = return . knn1Train, test = knnTest knn1 }

data Knn2Config = Knn2Config { knn2K :: !Int
                             , knn2G :: !Double
                             , knn2Tree :: !KdMap
                             }
    deriving Show

knn2 :: Knn2Config -> RequestPoint -> Class
knn2 (Knn2Config k g tree) = fst . maximumBy (comparing snd) . sumByClass . measurePoints
  where
    measurePoints !p = Kd.foldrWithKey f [] tree
     where kthNeighbor = fst $ last $ Kd.kNearest tree (k + 1) p
           measureP' n = kthNeighbor `seq` gaussianKernel g $ (sqrDist p n) / (sqrDist kthNeighbor p)
           f (n, cl) = ((cl, measureP' n) :)

knn2' :: Double -> Int -> KdMap -> RequestPoint -> Class
knn2' g k tree = fst . maximumBy (comparing snd) . sumByClass . measurePoints
  where
    measurePoints !p = Kd.foldrWithKey f [] tree
     where kthNeighbor = fst $ last $ Kd.kNearest tree (k + 2) p
           measureP' n = kthNeighbor `seq` gaussianKernel g $ (sqrDist p n) / (sqrDist kthNeighbor p)
           f (n, cl) | n /= p  = ((cl, measureP' n) :)
                     | otherwise = id

knn2Train :: Double -> [(RequestPoint, Class)] -> Knn2Config
knn2Train g = fst . knn2Train' g

knn2Train' !g !ps = (tree `seq` Knn2Config cl g tree, q)
  where
    (cl, q) = trainByK (knn2' g) tree
    tree = buildKdTree ps

knn2TestConfig :: Double -> KnnTestConfig Knn2Config
knn2TestConfig g = TestConfig { train = return . knn2Train g, test = knnTest $! knn2 }

knn3Train :: Int -> [(RequestPoint, Class)] -> RandMonad Knn2Config
knn3Train gc ps = getRandomRs (0, 1) >>= return . fst . minimumBy (comparing snd) . map (flip knn2Train' ps) . take gc

knn3TestConfig :: Int -> KnnTestConfig Knn2Config
knn3TestConfig gc = TestConfig { train = knn3Train gc, test = knnTest $! knn2 }

sumByClass :: (Num a) => [(Class, a)] -> [(Class, a)]
sumByClass = HM.toList . foldl f HM.empty
  where f m (cl, x) = maybe (insertInc $ fromInteger 0) insertInc $ HM.lookup cl m
          where insertInc cnt = HM.insert cl (cnt + x) m

countUnique :: [Class] -> [(Class, Int)]
countUnique = sumByClass . flip zip (repeat 1)


