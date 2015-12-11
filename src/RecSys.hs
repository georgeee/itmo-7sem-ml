{-# LANGUAGE PackageImports #-}
module RecSys( SvdPPConfig(..), SvdConfig(..), svd, svd', svdPP, svdPP'
             , rmse, svdTestConfig, SvdTrainConfig(..), svdPPTestConfig, User, Item, Rating ) where
import Data.List
import Data.Foldable
import Train
import Data.Maybe
import Control.DeepSeq
import Control.Monad
import Common
import qualified "unordered-containers" Data.HashSet as HS
import Control.Monad.Random
import qualified Numeric.LinearAlgebra as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type User = Int
type Item = Int
type Rating = Int

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

svd :: SvdConfig -> User -> Item -> Rating
svd rc u i = round $ svd' rc u i

svdPP :: SvdPPConfig -> User -> Item -> Rating
svdPP rc u i = round $ svdPP' rc u i

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

c `uB` u = uBases c V.! u
infixl 9 `uB`
c `iB` i = iBases c V.! i
infixl 9 `iB`
c `uV` u = uVects c V.! u
infixl 9 `uV`
c `iV` i = iVects c V.! i
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

svdPPTestConfig :: SvdTrainConfig -> TestConfig (User, Item, Rating) SvdPPConfig Double
svdPPTestConfig c = TestConfig { train = svdPPTrain c
                               , test = rateTest svdPP rmse
                               }

svdTestConfig :: SvdTrainConfig -> TestConfig (User, Item, Rating) SvdConfig Double
svdTestConfig c = TestConfig { train = svdTrain c
                             , test = rateTest svd rmse
                             }

rateTest rater score ss c = score $ map (\(u, i, r) -> (fromIntegral r, fromIntegral $ rater c u i)) ss

rmse :: [(Double, Double)] -> Double
rmse ds = negate $ (** 0.5) $ (sum $ map (\(a, b) -> (a - b)^2) ds) / (fromIntegral $ length ds)

getRandConf config rs = SvdConfig { base = svdRateBase config
                        , uBases = V.fromList uB
                        , iBases = V.fromList iB
                        , uVects = V.fromList uV
                        , iVects = V.fromList iV
                        }
  where (uB, rest1) = splitAt us rs
        (iB, rest2) = splitAt is rest1
        (uV, rest3) = takeVectors dim us rest2
        (iV, rest4) = takeVectors dim is rest3
        dim = svdDim config
        us = svdUMax config
        is = svdIMax config

takeVectors dim n rest = head $ drop n $ iterate f ([], rest)
  where f (bs, rest) = let (v, r) = splitAt dim rest
                        in (L.vector v : bs, r)

svdTrain :: SvdTrainConfig -> [(User, Item, Rating)] -> RandMonad SvdConfig
svdTrain config rates = do initConf <- getRandConf config <$> getRandomRs (0, 1)
                           sgd sgdConf initConf $ map (\(u, i, r) -> (u, i, fromIntegral r)) rates
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
        lossF c (u, i, r) = (svd' c u i) - (fromIntegral r)
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

computeRelated us = V.map HS.toList . foldr' f (V.replicate us HS.empty)
  where f (u, i, _) = V.modify (\v -> MV.read v u >>= MV.write v u . HS.insert i)


svdPPTrain :: SvdTrainConfig -> [(User, Item, Rating)] -> RandMonad SvdPPConfig
svdPPTrain config rates = do initConf <- getRandConf config <$> getRandomRs (0, 1)
                             initRiWeights <- fst . takeVectors (svdDim config) (svdIMax config) <$> return (repeat 0)
                             let initPPConf = SvdPPConfig { rppConfig = initConf
                                                          , rppRelated = computeRelated (svdUMax config) rates
                                                          , rppRItemWeights = V.fromList initRiWeights
                                                          }
                             sgd sgdConf initPPConf $ map (\(u, i, r) -> (u, i, fromIntegral r)) rates
  where sgdConf = SGDConfig { sgdLossF = lossF
                            , sgdSmoothness = svdSmoothness config
                            , sgdMaxIter = svdMaxIter config
                            , sgdUpdater = updater
                            , sgdPrec = svdPrec config
                            }
        l4 = svdL4 config
        l5 = svdL5 config
        l5' = L.scalar $ l5
        tempo = svdTempo config
        tempo' = L.scalar $ tempo
        lossF rc (u, i, r) = (svdPP' rc u i) - (fromIntegral r)
        updater e (u, i, r) rc = rc { rppConfig = c'
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
               rsize' = L.scalar $ (fromIntegral $ length related) ** (-0.5)
               c = rppConfig rc
               c' = bu' `seq` bi' `seq` vu' `seq` vi' `seq`
                        c { uBases = V.modify (\v -> MV.write v u bu') $ uBases c
                          , iBases = V.modify (\v -> MV.write v i bi') $ iBases c
                          , uVects = V.modify (\v -> MV.write v u vu') $ uVects c
                          , iVects = V.modify (\v -> MV.write v i vi') $ iVects c
                          }
