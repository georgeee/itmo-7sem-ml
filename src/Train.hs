module Train where
import Data.Ord
import Data.Random
import Control.Monad
import Common
import qualified Data.List as L

data TestConfig s c quality = TestConfig { train :: [s] -> RVar c
                                         , test :: [s] -> c -> quality
                                         }

trySamples :: TestConfig s c quality -> [s] -> [s] -> RVar (c, quality)
trySamples tConf p q = do c <- train tConf $! p
                          return $!  c `seq` q `seq` (c, test tConf q c)

--type Splitter s = [s] -> [([s], [s])]

type RandSplitter s = [s] -> RVar [([s], [s])]

splitByK k ls = tail $ map fst $ takeWhile pairNotEmpty $ iterate (splitAt k . snd) ([], ls)
  where pairNotEmpty (as, bs) = not $ (L.null as) && (L.null bs)

kFoldCV :: Int -> RandSplitter s
kFoldCV k ds = shuffle ds >>= return . listsByDivision . splitByK (length ds `div` k)

dummySplitter :: RandSplitter s
dummySplitter ds = (\x -> [(x, [])]) <$> shuffle ds

listsByDivision :: [[s]] -> [([s], [s])]
listsByDivision = l' []
  where
    l' _ [] = []
    l' acc (a:as) = el : l' (acc ++ a) as
      where el = (acc ++ (concat as), a)

tkFoldCv :: Int -> Int -> RandSplitter s
tkFoldCv t k ds = (sequence $ replicate t (kFoldCV k ds)) >>= return . concat

learn' :: Ord q => TestConfig s c q -> RandSplitter s -> [s] -> RVar c
learn' conf splitter xs = fst . L.maximumBy (comparing snd) <$> trySplitted xs
  where trySplitted = splitter >=> sequence . map (uncurry $ trySamples conf)

learn :: Ord q => TestConfig s c q -> RandSplitter s -> Double -> [s] -> RVar (c, q)
learn conf splitter finalTestCoef ds = do
                  xs <- shuffle ds
                  let xs' = take len' xs
                      xs'' = drop len' xs
                  when (L.null xs' || L.null xs'') $
                     fail ("Wrong testCoef supplied: " ++ (show finalTestCoef))
                  c <- learn' conf splitter xs''
                  return (c, test conf xs' c)
  where
    len = length ds
    len' = round $ (fromIntegral len) * finalTestCoef
    ds' = take len' ds
    ds'' = drop len' ds
