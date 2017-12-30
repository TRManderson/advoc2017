{-# LANGUAGE TypeOperators #-}
module Main where
import Data.List (foldl')
import qualified Data.ByteString as B
import Numeric (showHex)
import Data.Bits (xor)
import Control.Monad (join)
import Debug.Trace (traceShowId)

type Input = [Int]
type Output = String

val = 256

getInput :: IO Input
getInput = traceShowId . fmap fromIntegral . B.unpack <$> B.getLine

reverseN n ls = (reverse . take n $ ls) ++ drop n ls

data State = State {skip :: Int, totalSkips :: Int, ls :: [Int]} deriving Show

fixed :: ([Int] -> [Int]) -> [Int] -> [Int]
fixed f = take val . f . cycle

step :: Int -> State -> State
step n (State sk t ls) = State (sk + 1) (t + n + sk) (pipeline ls)
  where pipeline = fixed (drop $ n + sk)  . fixed (reverseN n)

restore :: State -> [Int]
restore (State _ t ls) = flip fixed ls . drop $ val - (t `mod` val)

reduce :: [Int] -> [Int]
reduce [] = []
reduce ls = foldr xor 0 (take 16 ls):reduce (drop 16 ls)

output :: [Int] -> Output
output = join . fmap (f . ($ "") . showHex)
  where
    -- showHex drops leading zeros
    f [c] = ['0', c]
    f [] = "00"
    f x = x

solve :: Input -> Output
solve = postprocess . foldl' (flip step) (State 0 0 [0..val-1]) . preprocess
  where
    preprocess = join . replicate 64 . (++ [17, 31, 73, 47, 23])
    postprocess = output . reduce . restore

main :: IO ()
main = print . solve =<< getInput
