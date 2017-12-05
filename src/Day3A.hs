module Main where
import Debug.Trace
import Data.Biapplicative
import Data.Foldable (for_)
import Control.Monad.Trans.Writer

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

seqFold = foldr tPlus (0, 0)

soln n = let (a, b) = seqFold (take n ls) in (abs a) + (abs b) 

main = print . soln . read =<< getLine
