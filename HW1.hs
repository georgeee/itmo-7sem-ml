{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
module HW1 where
import qualified Data.Trees.KdTree as Kd
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

type SamplePoint = (Double, Double, Class)
type RequestPoint = (Double, Double)

tuple3To2 (x, y, _) = (x, y)
tuple2To3 z (x, y) = (x, y, z)


dpx (x, _, _) = x
dpy (_, y, _) = y
dpc (_, _, c) = c

instance Kd.Point SamplePoint where
  dimension = const 2
  coord 0 = dpx
  coord 1 = dpy
  dist2 = minkowskiDistance 2

type KnnTestConfig c = TestConfig SamplePoint c Double

knnTest :: (conf -> RequestPoint -> Class) -> [SamplePoint] -> conf -> Double
knnTest impl ps conf = fScore list
  where f (x, y, c) = (impl conf (x, y), c)
        list = map f ps


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


trainByK :: (conf -> RequestPoint -> Class) -> (Kd.KdTree SamplePoint) -> (Int -> (Kd.KdTree SamplePoint) -> conf) -> (Int, Int)
trainByK !impl !tree !confConstructor = minimumBy (comparing snd) $ trainByK' impl tree confConstructor

trainByK' :: (conf -> RequestPoint -> Class) -> (Kd.KdTree SamplePoint) -> (Int -> (Kd.KdTree SamplePoint) -> conf) -> [(Int, Int)]
trainByK' !impl !tree !confConstructor = map tryK [kMin..kMax]
  where
    tryK :: Int -> (Int, Int)
    tryK !k = (k, F.foldl' (flip f) 0 tree)
      where
        f !p = if dpc p == test' p then id else (+1)
        test' p = impl (confConstructor k $ tree `Kd.remove` p) (tuple3To2 p)

type Knn1Config = (Int, Kd.KdTree SamplePoint)

knn1 :: Knn1Config -> RequestPoint -> Class
knn1 (k, tree) = fst . maximumBy (comparing snd) . countUnique . map dpc . Kd.kNearestNeighbors tree k . tuple2To3 (-1)

knn1Train :: [SamplePoint] -> Knn1Config
knn1Train ps = (fst $ trainByK knn1 tree (,), tree)
  where
    tree = Kd.fromList ps

knn1TestConfig :: KnnTestConfig Knn1Config -- Trying only k
knn1TestConfig = TestConfig { train = return . knn1Train, test = knnTest knn1 }

data Knn2Config = Knn2Config { knn2K :: !Int
                             , knn2G :: !Double
                             , knn2Tree :: !(Kd.KdTree SamplePoint)
                             }
    deriving Show

knn2 :: Knn2Config -> RequestPoint -> Class
knn2 (Knn2Config k g tree) = fst . maximumBy (comparing snd) . sumByClass . measurePoints . tuple2To3 (-1)
  where
    measurePoints !p = F.foldr' (\n ns -> (dpc n, measureP n) : ns) [] tree
     where kthNeighbor = last $ Kd.kNearestNeighbors tree (k + 1) p
           measureP n = kthNeighbor `seq` gaussianKernel g $ (Kd.dist2 p n) / (Kd.dist2 kthNeighbor p)

knn2Train :: Double -> [SamplePoint] -> Knn2Config
knn2Train g = fst . knn2Train' g

knn2Train' !g !ps = (tree `seq` Knn2Config cl g tree, q)
  where
    (cl, q) = trainByK knn2 tree cons
    tree = Kd.fromList ps
    cons !k !tree' = Knn2Config k g tree'

knn2TestConfig :: Double -> KnnTestConfig Knn2Config
knn2TestConfig g = TestConfig { train = return . knn2Train g, test = knnTest $! knn2 }

knn3Train :: Int -> [SamplePoint] -> RandMonad Knn2Config
knn3Train gc ps = getRandomRs (0, 1) >>= return . fst . minimumBy (comparing snd) . map (flip knn2Train' ps) . take gc

knn3TestConfig :: Int -> KnnTestConfig Knn2Config
knn3TestConfig gc = TestConfig { train = knn3Train gc, test = knnTest $! knn2 }

sumByClass :: (Num a) => [(Class, a)] -> [(Class, a)]
sumByClass = HM.toList . foldl f HM.empty
  where f m (cl, x) = maybe (insertInc $ fromInteger 0) insertInc $ HM.lookup cl m
          where insertInc cnt = HM.insert cl (cnt + x) m

countUnique :: [Class] -> [(Class, Int)]
countUnique = sumByClass . flip zip (repeat 1)


