module Main where
import Data.Bits ((.&.))
import Data.Function (on)
import Data.List (unfoldr)

gen seed mul s = filter (\v -> (v `rem` s) == 0) $ unfoldr (\s -> Just (s, s * mul `rem` 2147483647)) seed
cmp :: Int -> Int -> Bool
cmp = (==) `on` ((2^16 - 1) .&.)

main = print . length . filter id . take 5000000 $ zipWith cmp (gen 883 16807 4) (gen 879 48271 8)
