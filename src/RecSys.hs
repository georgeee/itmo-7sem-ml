{-# LANGUAGE BangPatterns, PackageImports #-}
module RecSys( SvdPPConfig(..), SvdConfig(..), svd', svdPP'
             , rmse, svdTestConfig, SvdTrainConfig(..), svdPPTestConfig, User, Item, Rating ) where
import Control.Applicative
import Data.Random.Source.PureMT
import Data.Word
import Data.Functor.Identity
import qualified Data.Random.Lift as RL
import Data.List
import Data.Foldable
import qualified Data.Array as A
import Data.STRef
import Control.Monad.ST
import Train
import Data.Maybe
import Control.DeepSeq
import Control.Monad
import Common
import qualified "unordered-containers" Data.HashSet as HS
import qualified Numeric.LinearAlgebra as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Random

type User = Int
type Item = Int
type Rating = Double

data SvdConfig = SvdConfig { base :: !Double
                           , uBases :: !(V.Vector Double)
                           , iBases :: !(V.Vector Double)
                           , uVects :: !(V.Vector (L.Vector Double))
                           , iVects :: !(V.Vector (L.Vector Double))
                           }
                           deriving Show

data SvdPPConfig = SvdPPConfig { rppConfig :: !SvdConfig
                               , rppRelated :: !(V.Vector [Item])
                               , rppRItemWeights :: !(V.Vector (L.Vector Double))
                               }
                           deriving Show

svdPP' :: SvdPPConfig -> User -> Item -> Double
svdPP' rc u i = (base c) + c `uB` u + c `iB` i + (pu L.<.> c `iV` i)
  where pu = case related of
                [] -> c `uV` u
                (r:rs) -> c `uV` u + rsize' * (foldr' ((+) . weight) (weight r) rs)
        c = rppConfig rc
        rsize' = L.scalar $ (fromIntegral $ length related)**(-0.5)
        weight = (rppRItemWeights rc V.!)
        related = rppRelated rc V.! u

svd' :: SvdConfig -> User -> Item -> Double
svd' c u i = (base c) + c `uB` u + c `iB` i + (c `uV` u L.<.> c `iV` i)

uB SvdConfig { uBases = !v } = (v V.!)
infixl 9 `uB`
iB SvdConfig { iBases = !v } = (v V.!)
infixl 9 `iB`
uV SvdConfig { uVects = !v } = (v V.!)
infixl 9 `uV`
iV SvdConfig { iVects = !v } = (v V.!)
infixl 9 `iV`

data SvdTrainConfig = SvdTrainConfig { svdMaxIter :: !Int
                                     , svdDim :: !Int
                                     , svdIMax :: !Int
                                     , svdUMax :: !Int
                                     , svdL4 :: !Double
                                     , svdSmoothness :: !Double
                                     , svdTempo :: !Double
                                     , svdPrec :: !Double
                                     , svdRateBase :: !Double
                                     , svdL5 :: !Double
                                     }

type SVDTestConfigGetter c = SvdTrainConfig -> SGDMethod (User, Item, Rating) c -> TestConfig (User, Item, Rating) c Double

svdPPTestConfig :: SVDTestConfigGetter SvdPPConfig
svdPPTestConfig c sgdM = TestConfig { train = svdPPTrain sgdM c
                               , test = rateTest svdPP' rmse
                               }

rateTest rater score ss c = score $ map (\(u, i, r) -> (r, rater c u i)) ss

rmse :: [(Double, Double)] -> Double
rmse ds = negate $ (** 0.5) $ (sum $ map (\(a, b) -> (a - b)^2) ds) / (fromIntegral $ length ds)

rateBase config ps = if svdRateBase config < 0
                        then (foldr' (\(_, _, r) -> (+) $ r) 0 ps) / (fromIntegral . length $ ps)
                        else svdRateBase config

getRandConf :: Double -> SvdTrainConfig -> RVar SvdConfig
getRandConf base config = SvdConfig base <$> (rV us) <*> (rV is) <*> (rV' us) <*> (rV' is)
  where rV = fmap V.fromList . flip replicateM (pure 0)
        rV' = fmap V.fromList . takeVectors dim (normal 0 (1 / (fromIntegral dim)))
        dim = svdDim config
        us = svdUMax config
        is = svdIMax config

takeVectors :: Int -> (RVar Double) -> Int -> RVar [L.Vector Double]
takeVectors dim r n = replicateM n $ L.vector <$> replicateM dim r

--(!) :: MV.STVector s a -> Int -> ST s a
--(!) = MV.read
--infixl 9 !
--
--(~+) :: (Applicative f, Num a) => f a -> f a -> f a
--(~+) = liftA2 (+)
--infixl 6 ~+
--(~-) :: (Applicative f, Num a) => f a -> f a -> f a
--(~-) = liftA2 (-)
--infixl 6 ~-
--(~.) :: (Applicative f, L.Numeric a) => f (L.Vector a) -> f (L.Vector a) -> f a
--(~.) = liftA2 (L.<.>)
--infixr 8 ~.
--(~*) :: (Applicative f, Num a) => f a -> f a -> f a
--(~*) = liftA2 (*)
--infixl 7 ~*

svdTestConfig :: SVDTestConfigGetter SvdConfig
svdTestConfig c sgdM = TestConfig { train = svdTrain sgdM c
                             , test = rateTest svd' rmse
                             }

svdTrain :: SGDMethod (User, Item, Rating) SvdConfig -> SvdTrainConfig -> [(User, Item, Rating)] -> RVar SvdConfig
svdTrain sgdM config rates = do initConf <- getRandConf (rateBase config rates) config
                                sgdM sgdConf initConf rates
  where sgdConf = SGDConfig { sgdLossF = lossF
                            , sgdSmoothness = svdSmoothness config
                            , sgdMaxIter = svdMaxIter config
                            , sgdUpdater = updater
                            , sgdPrec = svdPrec config
                            }
        l4 = svdL4 config
        l4' = L.scalar $ l4
        tempo = svdTempo config
        tempo' = L.scalar $ tempo
        lossF c (u, i, r) = r - (svd' c u i)
        updater e (u, i, r) c = bu' `seq` bi' `seq` vu' `seq` vi' `seq`
                                 c { uBases = V.modify (\v -> MV.write v u bu') $ uBases c
                                   , iBases = V.modify (\v -> MV.write v i bi') $ iBases c
                                   , uVects = V.modify (\v -> MV.write v u vu') $ uVects c
                                   , iVects = V.modify (\v -> MV.write v i vi') $ iVects c
                                   }
         where e' = L.scalar e
               bu' = c `uB` u + tempo * (e - l4 * c `uB` u)
               bi' = c `iB` i + tempo * (e - l4 * c `iB` i)
               vi' = c `iV` i + tempo' * (e' * c `uV` u - l4' * c `iV` i)
               vu' = c `uV` u + tempo' * (e' * c `iV` i - l4' * c `uV` u)

-- svdTrain :: SvdTrainConfig -> [(User, Item, Rating)] -> RVar SvdConfig
-- svdTrain config rates = do initConf <- getRandConf (rateBase config rates) config
--                            newSeed <- uniform (-2^61) (2^61)
--                            return $ run' newSeed initConf
--   where !ps = A.listArray (0, l) rates
--         l = length rates - 1
--         smth = svdSmoothness config
--         maxIter = svdMaxIter config
--         prec = svdPrec config
--         l4 = svdL4 config
--         !l4' = pure $ L.scalar l4 :: ST s (L.Vector Double)
--         tempo = svdTempo config
--         !tempo' = L.scalar $ tempo :: L.Vector Double
--         run :: SvdConfig -> Int -> SvdConfig
--         run initConf index = runST $ do
--           let lossF (!u, !i, !r) = pure r ~- ((pure $ base initConf) ~+ uBs ! u ~+ iBs ! i ~+ (uVs ! u ~. iVs ! i))
--               initQ = foldM (\b a -> (+b) <$> lossF a) 0 rates
--           q <- lift $ initQ >>= newSTRef
--           let step conf oldQ index = do
--                 let xi@(u, i, r) <- (A.!) ps index
--                     e <- lift $ lossF xi
--                 uBs <- V.unsafeThaw $ uBases initConf
--                 iBs <- V.unsafeThaw $ iBases initConf
--                 uVs <- V.unsafeThaw $ uVects initConf
--                 iVs <- V.unsafeThaw $ iVects initConf
--                 let e' = L.scalar e :: L.Vector Double
--                     ub = uBs ! u
--                     ib = iBs ! i
--                     uv = uVs ! u
--                     iv = iVs ! i
--                 MV.write uBs u $! ub + tempo * (e - l4 * ub)
--                 MV.write iBs i $! ib + tempo * (e - l4 * ib)
--                 MV.write uVs u $! uv + tempo' * (e' * iv - l4' * uv)
--                 MV.write iVs i $! iv + tempo' * (e' * uv - l4' * iv)
--                 let newQ = (1 - smth) * oldQ + smth * e
--                 (,) newQ . SvdConfig (base initConf)
--                                     <$> (V.unsafeFreeze uBs)
--                                     <*> (V.unsafeFreeze iBs)
--                                     <*> (V.unsafeFreeze uVs)
--                                     <*> (V.unsafeFreeze iVs)
--                 when ((prec == 0 || abs (newQ - oldQ) > prec) && iter > 0) step
--           step



computeRelated us = V.map HS.toList . foldr' f (V.replicate us HS.empty)
  where f (u, i, _) = V.modify (\v -> MV.read v u >>= MV.write v u . HS.insert i)


svdPPTrain :: SGDMethod (User, Item, Rating) SvdPPConfig -> SvdTrainConfig -> [(User, Item, Rating)] -> RVar SvdPPConfig
svdPPTrain sgdM config rates = do initConf <- getRandConf (rateBase config rates) config
                                  initRiWeights <- takeVectors dim (normal 0 (1 / (fromIntegral dim))) (svdIMax config)
                                  let initPPConf = SvdPPConfig { rppConfig = initConf
                                                               , rppRelated = computeRelated (svdUMax config) rates
                                                               , rppRItemWeights = V.fromList initRiWeights
                                                               }
                                  sgdM sgdConf initPPConf rates
  where sgdConf = SGDConfig { sgdLossF = lossF
                            , sgdSmoothness = svdSmoothness config
                            , sgdMaxIter = svdMaxIter config
                            , sgdUpdater = updater
                            , sgdPrec = svdPrec config
                            }
        l4 = svdL4 config
        l5 = svdL5 config
        l5' = L.scalar $ l5
        dim = svdDim config
        tempo = svdTempo config
        tempo' = L.scalar $ tempo
        lossF rc (u, i, r) = r - (svdPP' rc u i)
        updater e (u, i, r) rc = ws' `seq` c' `seq` rc { rppConfig = c'
                                    , rppRItemWeights = ws'
                                    }
         where e' = L.scalar e
               bu' = c `uB` u + tempo * (e - l4 * c `uB` u)
               bi' = c `iB` i + tempo * (e - l4 * c `iB` i)
               vi' = c `iV` i + tempo' * (e' * pu - l5' * c `iV` i)
               vu' = c `uV` u + tempo' * (e' * c `iV` i - l5' * c `uV` u)
               weight = (rppRItemWeights rc V.!)
               related = rppRelated rc V.! u
               pu = case related of
                       [] -> c `uV` u
                       (r:rs) -> c `uV` u + rsize' * (foldr' ((+) . weight) (weight r) rs)
               ws' = V.modify modifier $ rppRItemWeights rc
                where modifier v = forM_ related $ f v
                      f v j = do yj <- MV.read v j
                                 MV.write v j $ yj + tempo' * (e' * rsize' * c `iV` i - l5' * yj)
               rsize' = L.scalar $! (fromIntegral $! length related) ** (-0.5)
               c = rppConfig rc
               c' = bu' `seq` bi' `seq` vu' `seq` vi' `seq`
                        c { uBases = V.modify (\v -> MV.write v u bu') $! uBases c
                          , iBases = V.modify (\v -> MV.write v i bi') $! iBases c
                          , uVects = V.modify (\v -> MV.write v u vu') $! uVects c
                          , iVects = V.modify (\v -> MV.write v i vi') $! iVects c
                          }
