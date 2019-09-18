module Util where

import Data.Function.Between.Lazy
import Control.Applicative

flist :: [a -> b -> Bool] -> a -> b -> [Bool]
flist fs a b = map (a ~$~ b) fs

sumMaybes :: (Num a) => [Maybe a] -> Maybe a
sumMaybes = foldr (liftA2 (+)) (Just 0)
