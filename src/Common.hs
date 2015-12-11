{-# LANGUAGE PackageImports, FlexibleInstances #-}
module Common where

import Data.Foldable
import Data.Maybe
import qualified Data.Array as A
import Control.Monad.Random

type RandMonad s = Rand StdGen s

gaussianKernel :: Floating a => a -> a -> a
gaussianKernel c d = exp ( (-0.5) * d / c)

errorAvg :: [Double] -> Double
errorAvg l = let (sum', count) = foldl (\(x, y) el -> (x + el, y + 1)) (0, 0) l
              in sum' / count

type Class = Int

split :: (a -> Bool) -> [a] -> [[a]]
split f = reverse . s []
  where s a [] = a
        s a l = let (pl, pr) = span (not . f) l
                    (_, pr') = span f pr
                in s (pl:a) pr'

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

data SGDConfig p c = SGDConfig { sgdLossF :: c -> p -> Double
                               , sgdSmoothness :: Double
                               , sgdMaxIter :: Int
                               , sgdUpdater :: Double -> p -> c -> c
                               , sgdPrec :: Double
                               }

sgd :: SGDConfig p c -> c -> [p] -> RandMonad c
sgd sgdConf c ps = sgd' sgdConf c ps <$> getRandomRs (0, l)
  where l = length ps - 1

-- Stohastic gradient descent
sgd' :: SGDConfig p c -> c -> [p] -> [Int] -> c
sgd' sgdConf initConf points = step initConf initQ . take maxIter
  where ps = A.listArray (0, l) points
        SGDConfig { sgdLossF = lossF
                  , sgdSmoothness = smth
                  , sgdMaxIter = maxIter
                  , sgdUpdater = updater
                  , sgdPrec = prec
                  } = sgdConf
        initQ = foldr' ((+) . lossF initConf) 0 points
        l = length points - 1
        step c _ [] = c
        step c q (index:irest)
            = let e = lossF c xi
                  xi = ps A.! index
                  c' = updater e xi c
                  q' = (1 - smth) * q + smth * e
               in if prec == 0 || abs (q - q') > prec
                   then step c' q' irest
                   else c'
