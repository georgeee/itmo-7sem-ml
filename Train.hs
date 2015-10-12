module Train where
import Data.Ord
import Control.Monad.Random
import Control.Monad
import System.Random.Shuffle
import qualified Data.List as L

data TestConfig s c quality = TestConfig { train :: [s] -> RandMonad c, test :: [s] -> c -> quality}

trySamples :: TestConfig s c quality -> [s] -> [s] -> RandMonad (c, quality)
trySamples tConf p q = do c <- train tConf $! p
                          return $!  c `seq` q `seq` (c, test tConf q c)

--type Splitter s = [s] -> [([s], [s])]

type RandMonad s = Rand StdGen s
type RandSplitter s = [s] -> RandMonad [([s], [s])]

splitByK k ls = tail $ map fst $ takeWhile pairNotEmpty $ iterate (splitAt k . snd) ([], ls)
  where pairNotEmpty (as, bs) = not $ (L.null as) && (L.null bs)

kFoldCV :: Int -> RandSplitter s
kFoldCV k ds = shuffleM ds >>= return . listsByDivision . splitByK (length ds `div` k)

listsByDivision :: [[s]] -> [([s], [s])]
listsByDivision = l' []
  where
    l' _ [] = []
    l' acc (a:as) = el `seq` el : l' (acc ++ a) as
      where el = (acc ++ (concat as), a)

tkFoldCv :: Int -> Int -> RandSplitter s
tkFoldCv t k ds = (sequence $ replicate t (kFoldCV k ds)) >>= return . concat

learn :: Ord quality => TestConfig s c quality -> RandSplitter s -> [s] -> RandMonad (c, quality)
learn conf splitter ds = lists >>= sequence . map (uncurry (trySamples conf)) >>= return . L.maximumBy (comparing snd)
  where lists = splitter $! ds
