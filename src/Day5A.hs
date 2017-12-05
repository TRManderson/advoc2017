module Main where
import Data.Vector as V
import Debug.Trace

soln :: Vector Int -> Int -> Int
soln vec idx = case vec !? idx of
  Just n -> 1 + soln (vec // [(idx, n+1)] ) (idx + n)
  Nothing -> 0

main = print . flip soln 0 . fromList . fmap read . lines =<< getContents
