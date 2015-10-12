module Common where

import qualified Data.List as L

errorAvg :: [Double] -> Double
errorAvg l = let (sum', count) = foldl (\(x, y) el -> (x + el, y + 1)) (0, 0) l
              in sum' / count

errorMin :: [Double] -> Double
errorMin = L.minimum

gaussianKernel :: Floating a => a -> a -> a
gaussianKernel c x = exp ( (-0.5) * x * x / ( c * c ) )
