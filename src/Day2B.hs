module Main where
import Data.List.Split

soln :: [String] -> Int
soln = sum . map soln'

soln' :: String -> Int
soln' s = head $ ls
  where
    ints = map read . splitOn " " $ s
    ls = do
      a <- ints
      b <- ints
      let (d, r) = quotRem a b
      if a /= b && r == 0 then
        return d
      else
        []

main = print . soln . lines =<< getContents