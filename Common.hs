module Common where

import Data.Trees.KdTree
import qualified Data.List as L

minkowskiDistance :: (Point p) => Int -> p -> p -> Double
minkowskiDistance p a b = let d = dimension a
                           in abs $ (sum $ map ((^^p) . (\i -> (coord i a) - (coord i b))) [0..(d-1)]) ** (1.0 / (fromIntegral p :: Double))


errorAvg :: [Double] -> Double
errorAvg l = let (sum', count) = foldl (\(x, y) el -> (x + el, y + 1)) (0, 0) l
                in sum' / count

errorMin :: [Double] -> Double
errorMin = L.minimum

gaussianKernel :: Floating a => a -> a -> a
gaussianKernel c x = exp ( (-0.5) * x * x / ( c * c ) )
