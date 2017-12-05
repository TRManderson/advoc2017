module Main where
import Data.Vector as V
import Debug.Trace

up idx n = flip (//) [(idx, if n >= 3 then n-1 else n+1)]

soln :: Vector Int -> Int -> Int
soln vec idx = case vec !? idx of
  Just n -> 1 + soln (up idx n vec) (idx + n)
  Nothing -> 0

main = print . flip soln 0 . fromList . fmap read . lines =<< getContents
