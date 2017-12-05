module Main where
import Prelude hiding (lookup)
import Data.Biapplicative
import Data.Foldable (for_)
import Control.Monad.Trans.Writer
import Data.Map.Strict hiding (map)

tPlus = biliftA2 (+) (+)

right = (1, 0)
up = (0, 1)
left = (-1, 0)
down = (0, -1)

genSeq n = seqStep n >> genSeq (n+2)
seqStep n = do
  tell (right:replicate (n-1) up)
  for_ steps (tell . replicate n)
  where steps = [left, down, right] 
ls = (0,0) : execWriter (genSeq 2)

soln' :: Int -> [(Int, Int)] -> (Int, Int) -> Map (Int, Int) Int -> Int
soln' n (c'':xs) c' m = if nSum > n then nSum else soln' n xs coord written
  where
    coord = tPlus c' c''
    nSum = sum . map (maybe 0 id . flip lookup m) . neighbours $ coord
    written = insert coord nSum m

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours c =
  [tPlus c c' | c' <- [(1,0), (0,1), (-1,0), (0,-1), (1,1), (-1,-1), (-1,1), (1,-1)]]

soln :: Int -> Int
soln n = soln' n (tail ls) (0,0) (singleton (0,0) 1)

main = print . soln . read =<< getLine
