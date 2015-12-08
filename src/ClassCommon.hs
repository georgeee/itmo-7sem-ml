{-# LANGUAGE PackageImports #-}
module ClassCommon where

import Data.Foldable
import Control.Monad
import Data.Maybe
import qualified "hashmap" Data.HashSet as HS
import qualified "hashmap" Data.HashMap as HM
import qualified Data.Array as A
import Control.Monad.Random
import Linear
import Common

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

data LSGDConfig = LSGDConfig { lsgdLoss :: Double -> Double
                             , lsgdLoss' :: Double -> Double
                             , lsgdTempo :: Double
                             , lsgdSmoothness :: Double
                             , lsgdPrec :: Double
                             , lsgdMaxIter :: Int
                             }

-- Stohastic gradient descent
lsgd :: Point p => LSGDConfig -> [(p, Class)] -> RandMonad p
lsgd config points = step (lsgdMaxIter config) pNull initQ
  where ps = A.listArray (0, l) $ map (\(x, y) -> (x, fromIntegral y)) points
        l = length points - 1
        tempo = lsgdTempo config
        smth = lsgdSmoothness config
        lossF = lF $ lsgdLoss config
        lossF' = lF $ lsgdLoss' config
        lF f w (x, y) =  f $ (dotProduct w x) * y
        initQ = foldr' ((+) . lossF pNull) 0 ps
        step iter w q | iter == 0 = return w
                      | otherwise = do i <- getRandomR (0, l)
                                       let ei = lossF w xi
                                           xi@(xix, xiy) = ps A.! i
                                           w' = w `pMinus` ((tempo * (lossF' w xi) * xiy) `pMul` xix)
                                           q' = (1 - smth) * q + smth * ei
                                       if abs (q - q') > lsgdPrec config
                                          then step (iter - 1) w' q'
                                          else return w'

lsgd' :: Point p => LSGDConfig -> [(p, Class)] -> RandMonad (LinClassConfig p)
lsgd' config points = unlift <$> lsgd config points'
  where points' = map (\(x, c) -> (LiftUp x (-1), c)) points
        unlift (LiftUp w w0) = LinClassConfig w w0


