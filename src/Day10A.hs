{-# LANGUAGE TypeOperators #-}
module Main where
import Data.List.Split (splitOn)
import Data.List (foldl')

type Input = [Int]
type Output = Int

val = 256

getInput :: IO Input
getInput = fmap read . splitOn "," <$> getLine

reverseN n ls = (reverse . take n $ ls) ++ drop n ls

data State = State {skip :: Int, totalSkips :: Int, ls :: [Int]} deriving Show

fixed :: ([Int] -> [Int]) -> [Int] -> [Int]
fixed f = take val . f . cycle

step :: Int -> State -> State
step n (State sk t ls) = State (sk + 1) (t + n + sk) (pipeline ls)
  where pipeline = fixed (drop $ n + sk)  . fixed (reverseN n)

restore :: State -> [Int]
restore (State _ t ls) = flip fixed ls . drop $ val - (t `mod` val)

solve :: Input -> Output
solve = product . take 2 . restore . foldl' (flip step) (State 0 0 [0..val-1])

main :: IO ()
main = print . solve =<< getInput
