module Main where
import Data.List.Split

soln :: [String] -> Int
soln = sum . map soln'

soln' :: String -> Int
soln' s = maximum ints - minimum ints
  where
    ints = map read . splitOn " " $ s

main = print . soln . lines =<< getContents