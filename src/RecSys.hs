module RecSys where
import Data.Foldable
import Train
import Data.Maybe
import Common
import Control.Monad.Random
import qualified Data.Array as A
import qualified Numeric.LinearAlgebra as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type User = Int
type Item = Int
type Rating = Int

v `at` i = maybe 0 id $ v V.!? i

data RecConfig = RecConfig { base :: !Double
                           , uBases :: !(V.Vector Double)
                           , iBases :: !(V.Vector Double)
                           , uVects :: !(V.Vector (L.Vector Double))
                           , iVects :: !(V.Vector (L.Vector Double))
                           }
                           deriving Show

rate :: RecConfig -> User -> Item -> Rating
rate rc u i = round $ rate' rc u i

rate' :: RecConfig -> User -> Item -> Double
rate' rc u i = b + bu + bi + (vu L.<.> vi)
  where b = base rc
        bu = uBases rc `at` u
        bi = iBases rc `at` i
        vu = uVects rc `at` u
        vi = iVects rc `at` i

rateLoss :: Double -> RecConfig -> User -> Item -> Rating -> Double
rateLoss l4 rc u i r = major `seq` reg `seq` major + reg
  where b = base rc
        bu = uBases rc `at` u
        bi = iBases rc `at` i
        vu = uVects rc `at` u
        vi = iVects rc `at` i
        reg = l4 * (bi * bi + bu * bu + (vi L.<.> vi) + (vu L.<.> vu))
        major = (b + bu + bi + (vu L.<.> vi) - (fromIntegral r))^2

data SvdTrainConfig = SvdTrainConfig { svdMaxIter :: !Int
                                     , svdDim :: !Int
                                     , svdIMax :: !Int
                                     , svdUMax :: !Int
                                     , svdL4 :: !Double
                                     , svdSmoothness :: !Double
                                     , svdTempo :: !Double
                                     , svdPrec :: !Double
                                     }

svdTestConfig :: SvdTrainConfig -> TestConfig (User, Item, Rating) RecConfig Double
svdTestConfig c = TestConfig { train = svdTrain c
                             , test = rateTest rate rmse
                             }

rateTest rater score ss c = score $ map (\(u, i, r) -> (fromIntegral r, fromIntegral $ rater c u i)) ss

rmse :: [(Double, Double)] -> Double
rmse ds = (** 0.5) $ (sum $ map (\(a, b) -> (a - b)^2) ds) / (fromIntegral $ length ds)

-- Stohastic gradient descent
svdTrain :: SvdTrainConfig -> [(User, Item, Rating)] -> RandMonad RecConfig
svdTrain config rates = step (svdMaxIter config) initConf initQ
  where ps = A.listArray (0, l) $ map (\(u, i, r) -> (u, i, fromIntegral r)) rates
        initConf = RecConfig { base = 0
                             , uBases = V.replicate us 0
                             , iBases = V.replicate is 0
                             , uVects = V.replicate us nullVector
                             , iVects = V.replicate is nullVector
                             }
        us = svdUMax config
        is = svdIMax config
        l4 = svdL4 config
        l4' = L.scalar $ l4
        l = length rates - 1
        tempo = svdTempo config
        tempo' = L.scalar $ tempo
        lossF c (u, i, r) = rateLoss (svdL4 config) c u i r
        smth = svdSmoothness config
        nullVector = L.vector $ replicate (svdDim config) 0
        initQ = foldr' ((+) . lossF initConf) 0 ps
        step iter c q | iter == 0 = return c
                      -- rewrite getRandomR to generating list of randoms before all steps
                      -- and then simple foldr' instead of step
                      -- currently whole execution can't be evaluated because of stubs
                      | otherwise = do index <- getRandomR (0, l)
                                       let e = lossF c xi
                                           e' = L.scalar e
                                           xi@(u, i, r) = ps A.! index
                                           uB = uBases c
                                           iB = iBases c
                                           uV = uVects c
                                           iV = iVects c
                                           bu = uB `at` u
                                           bi = iB `at` i
                                           vu = uV `at` u
                                           vi = iV `at` i
                                           bu' = bu + tempo * (e - l4 * bu)
                                           bi' = bi + tempo * (e - l4 * bi)
                                           vi' = vi + tempo' * (e' * vu - l4' * vi)
                                           vu' = vu + tempo' * (e' * vi - l4' * vu)
                                           c' = c { uBases = bu' `seq` V.modify (\v -> MV.write v u bu') uB
                                                  , iBases = bi' `seq` V.modify (\v -> MV.write v i bi') iB
                                                  , uVects = vu' `seq` V.modify (\v -> MV.write v u vu') uV
                                                  , iVects = vi' `seq` V.modify (\v -> MV.write v i vi') iV
                                                  }
                                           q' = (1 - smth) * q + smth * e
                                       if abs (q - q') > svdPrec config
                                          then step (iter - 1) c' q'
                                          else return c'

