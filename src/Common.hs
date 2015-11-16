{-# LANGUAGE PackageImports, FlexibleInstances #-}
module Common where

import Control.Monad
import Data.Maybe
import qualified Data.List as L
import qualified "hashmap" Data.HashSet as HS
import qualified "hashmap" Data.HashMap as HM

errorAvg :: [Double] -> Double
errorAvg l = let (sum', count) = foldl (\(x, y) el -> (x + el, y + 1)) (0, 0) l
              in sum' / count

errorMin :: [Double] -> Double
errorMin = L.minimum

-- kernel function takes positive distance
type Kernel a = a -> a

gaussianKernel :: Floating a => a -> Kernel a
gaussianKernel c d = exp ( (-0.5) * d / c)

class Point p where
  sqrDist :: p -> p -> Double
  dotProduct :: p -> p -> Double
  pAdd :: p -> p -> p
  pMul :: Double -> p -> p
  pMul' :: Real r => r -> p -> p
  pMul' = pMul . realToFrac
  pNull :: p

infixl 6 `pAdd`
infixr 7 `pMul`
infixr 7 `pMul'`

type SimilarityFunction p = p -> p -> Double

rbf :: Double -> SimilarityFunction (Double, Double)
rbf g x y = gaussianKernel g $ sqrDist x y

instance Point (Double, Double) where
  sqrDist (x1, y1) (x2, y2) = (s $ x1 - x2) + (s $ y1 - y2)
    where s x = x * x
  dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2
  pAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  pMul a (x, y) = (a * x, a * y)
  pNull = (0, 0)

instance Point (Double, Double, Double) where
  sqrDist (x1, y1, z1) (x2, y2, z2) = (s $ x1 - x2) + (s $ y1 - y2) + (s $ z1 - z2)
    where s x = x * x
  dotProduct (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2
  pAdd (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
  pMul a (x, y, z) = (a * x, a * y, a * z)
  pNull = (0, 0, 0)

type Class = Int
type Point2d d = (d, d)
type DPoint2d = (Double, Double)
type DPoint3d = (Double, Double, Double)

data GaussLiftConfig = GaussLiftConfig { glG :: Double -- gaussian param
                                       , glW :: (Double, Double) -- weights
                                       , glB :: (Double, Double) -- base vector
                                       }

gaussLift3d :: GaussLiftConfig -> DPoint2d -> DPoint3d
gaussLift3d (GaussLiftConfig g (wx, wy) (bx, by)) (px, py) = (px, py, gaussianKernel g $ (s $ px - bx) * wx + (s $ py - by) * wy)
  where s x = x * x

split :: (a -> Bool) -> [a] -> [[a]]
split f = reverse . s []
  where s a [] = a
        s a l = let (pl, pr) = span (not . f) l
                    (_, pr') = span f pr
                in s (pl:a) pr'

chipsReadPoints :: Monad m => String -> m [(DPoint2d, Class)]
chipsReadPoints = fmap (map fromJust . filter isJust) . sequence . map readP . lines
readP l = let ws = words l
           in if length ws < 3
              then return Nothing
              else case readP' ws of
                     Just p -> return $ Just p
                     Nothing -> fail $ "Wrong input: " ++ l
readP' ws = do x <- readD 0
               y <- readD 1
               c <- readI 2
               return ((x, y), c)
  where
    readD i = maybeRead (map (\c -> if c == ',' then '.' else c) $ ws !! i) :: Maybe Double
    readI i = maybeRead (ws !! i) :: Maybe Int

chipsReadPoints' :: Monad m => String -> m [(DPoint2d, Class)]
chipsReadPoints' = chipsReadPoints >=> return . map (\(p, c) -> (p, binC c))
  where binC 0 = -1
        binC _ = 1

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- [(guessed class, real class)]
type ClassScore = [(Class, Class)] -> Double


classifierTest :: Point p => Classifier p conf -> ClassScore -> [(p, Class)] -> conf -> Double
classifierTest impl score ps conf = score list
  where f (p, cl) = (impl conf p, cl)
        list = map f ps

type Classifier p conf = conf -> p -> Class

fScore :: ClassScore
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

sumByClass :: (Num a) => [(Class, a)] -> [(Class, a)]
sumByClass = HM.toList . foldl f HM.empty
  where f m (cl, x) = maybe (insertInc $ fromInteger 0) insertInc $ HM.lookup cl m
          where insertInc cnt = HM.insert cl (cnt + x) m

collectByClass :: [(a, Class)] -> [(Class, [a])]
collectByClass = HM.toList . foldl f HM.empty
  where f m (x, cl) = case HM.lookup cl m of
                        Just _ -> HM.adjust (x:) cl m
                        Nothing -> HM.insert cl [x] m

countUnique :: [Class] -> [(Class, Int)]
countUnique = sumByClass . flip zip (repeat 1)

data LinClassConfig p = LinClassConfig { scW :: p, scW0 :: Double }

instance Show p => Show (LinClassConfig p) where
  show (LinClassConfig ws w0) = "ws = " ++ (show ws) ++ ", w0 = " ++ (show w0)

linearClassifier :: (Point p) => LinClassConfig p -> p -> Class
linearClassifier (LinClassConfig w w0) p = sgn $ (dotProduct p w) - w0
  where sgn x | x < 0 = -1
              | x > 0 = 1
              | otherwise = 0

