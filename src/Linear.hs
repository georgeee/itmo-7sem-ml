{-# LANGUAGE FlexibleInstances #-}
module Linear where

import Common

class Point p where
  sqrDist :: p -> p -> Double
  dotProduct :: p -> p -> Double
  pAdd :: p -> p -> p
  pMul :: Double -> p -> p
  pMul' :: Real r => r -> p -> p
  pMul' = pMul . realToFrac
  pNull :: p
  pMinus :: p -> p -> p
  pMinus a b = a `pAdd` (-1) `pMul` b

infixl 6 `pAdd`
infixl 6 `pMinus`
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

data LiftUp p = LiftUp p Double

instance Point p => Point (LiftUp p) where
  sqrDist (LiftUp p pz) (LiftUp q qz) = (sqrDist p q) + (s $ pz - qz)
    where s x = x * x
  dotProduct (LiftUp p pz) (LiftUp q qz) = (dotProduct p q) + pz * qz
  pAdd (LiftUp p pz) (LiftUp q qz) = LiftUp (p `pAdd` q) (pz + qz)
  pMul a (LiftUp p pz) = LiftUp (a `pMul` p) (a * pz)
  pNull = LiftUp pNull 0

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

