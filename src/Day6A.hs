{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where
import qualified Data.Vector as V
import Data.Vector.Instances ()
import Control.Monad.Loops (untilM_)
import Control.Lens
import Data.HashSet
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.State.Lazy as L
import Data.List.Split (splitOn)

data PState = PState { _history :: HashSet (V.Vector Int), _cycleCount :: Int, _current :: V.Vector Int}
type Puzzle a = S.State PState a
makeLenses ''PState

getInput :: IO (V.Vector Int)
getInput = V.fromList . fmap read . splitOn "\t" <$> getLine

stateSeen :: Lens' PState Bool
stateSeen = lens getter setter
  where
    getter s = s^.history.contains (s^.current)
    setter s a = history.contains (s^.current) .~ a $ s

update :: V.Vector Int -> V.Vector Int
update = L.execState $ do
  idx <- L.gets V.maxIndex
  val <- L.gets (V.! idx)
  len <- L.gets V.length
  let
    (iters, firstIter) = val `divMod` len
    idxs = [idx+1 .. len-1] ++ [0 .. idx]
    fIdxs = fmap (\i -> (i, iters+1)) . take firstIter $ idxs
    rIdxs = fmap (\i -> (i, iters)) . drop firstIter $ idxs
    shifts = (idx, -val):fIdxs ++ rIdxs
  L.modify $ flip (V.accum (+)) shifts

step :: Puzzle ()
step = do
  stateSeen .= True
  current %= update
  cycleCount += 1

-- soln :: PState -> Int
soln = S.evalState $ do
  step `untilM_` use stateSeen
  (,) <$> use cycleCount <*> use current

main :: IO ()
main = print . soln . PState mempty 0 =<< getInput
