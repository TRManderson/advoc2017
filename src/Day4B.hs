module Main where
import Data.Discrimination.Grouping
import Data.Discrimination.Sorting
import Data.List.Split

main = print . length . filter (all (== 1)) . map (map length . group . map sort . splitOn " ") . lines =<< getContents