{-# LANGUAGE BangPatterns, PackageImports, FlexibleInstances, FlexibleContexts #-}
module Common where

import Data.Foldable
import Data.Maybe
import qualified Data.Array as A
import Data.Random
import Data.Random.Source.PureMT
import Data.IORef
import Control.Monad
import System.Time


defaultSample seed rvar = seed' >>= newIORef . pureMT >>= runRVar rvar
  where time = getClockTime >>= \(TOD sec _) -> return $ fromIntegral sec
        seed' = case seed of
                  Just s -> return s
                  _ -> do t <- time
                          putStrLn $ "Random seed: "  ++ (show t)
                          return t

getRandomRs :: Distribution Uniform a => a -> a -> Int -> RVar [a]
getRandomRs m s n = replicateM n (uniform m s)

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
                               , sgdSmoothness :: !Double
                               , sgdMaxIter :: !Int
                               , sgdUpdater :: Double -> p -> c -> c
                               , sgdPrec :: !Double
                               }

type SGDMethod p c = SGDConfig p c -> c -> [p] -> RVar c

-- "Full" stohastic gradient descent
fsgd :: Double -> SGDMethod p c
fsgd p sgdConf initConf points = step initConf initQ <$> replicateM maxIter (replicateM count $ uniform 0 l)
  where ps = A.listArray (0, l) points
        SGDConfig { sgdLossF = lossF
                  , sgdSmoothness = smth
                  , sgdMaxIter = maxIter
                  , sgdUpdater = updater
                  , sgdPrec = prec
                  } = sgdConf
        initQ = foldr' ((+) . lossF initConf) 0 points
        l = length points - 1
        count = max l $ round $ (fromIntegral l) * p
        step c _ [] = c
        step c q (is:irest)
            = let
                  (e, c') = foldr' (\i (es, ci) -> let ei = lossF ci xi
                                                       xi = ps A.! i
                                                       c' = updater ei xi ci
                                                    in (es `seq` es + ei, ci `seq` c')) (0, c) is
                  q' = q `seq` (1 - smth) * q + smth * e
               in if prec == 0 || abs (q - q') > prec
                   then step c' q' irest
                   else c'

-- Stohastic gradient descent
sgd :: SGDMethod p c
sgd sgdConf initConf points = step initConf initQ <$> getRandomRs 0 l maxIter
  where ps = A.listArray (0, l) points
        SGDConfig { sgdLossF = lossF
                  , sgdSmoothness = smth
                  , sgdMaxIter = maxIter
                  , sgdUpdater = updater
                  , sgdPrec = prec
                  } = sgdConf
        initQ = foldr' ((+) . lossF initConf) 0 points
        l = length points - 1
        step !c _ [] = c
        step !c !q (!index:irest)
            = let !e = lossF c xi
                  !xi = ps A.! index
                  !c' = updater e xi c
                  !q' = (1 - smth) * q + smth * e
               in if prec == 0 || abs (q - q') > prec
                   then step c' q' irest
                   else c'
