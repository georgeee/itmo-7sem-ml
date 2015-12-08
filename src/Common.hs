{-# LANGUAGE PackageImports, FlexibleInstances #-}
module Common where

import Data.Foldable
import Data.Maybe
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

