{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where
import qualified Data.Vector as V
import Data.Vector.Instances ()
import Control.Monad.Loops (untilM_)
import Control.Lens
import Data.Bool (bool)
import qualified Data.HashMap.Lazy as M
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.State.Lazy as L
import Data.List.Split (splitOn)

data PState = PState { _history :: M.HashMap (V.Vector Int) Int, _cycleCount :: Int, _current :: V.Vector Int}
type Puzzle a = S.State PState a
makeLenses ''PState

getInput :: IO (V.Vector Int)
getInput = V.fromList . fmap read . splitOn "\t" <$> getLine

stateSeen :: Lens' PState Bool
stateSeen = lens getter setter
  where
    getter s =(history.at (s^.current)._Just) `has` s
    val s = bool Nothing (Just (s^.cycleCount))
    setter s a = history.at (s^.current) .~ val s a $ s

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

soln :: PState -> Int
soln = S.evalState $ do
  step `untilM_` use stateSeen
  v <- use current
  c2 <- use cycleCount
  c1 <- use $ history.at v
  case c1 of
    Nothing -> error "Not possible"
    Just c' -> pure $ c2 - c'

main :: IO ()
main = print . soln . PState mempty 0 =<< getInput
{-
So this implementation is actually totally unnecessary - it's easier to just
cheat and put in the original cycle as the stopping condition.
This implementation will work for any input though.
-}


-- the easy way to do B - run until you see the part A answer
isEnd :: Puzzle Bool
isEnd = (== V.fromList [14,13,12,11,9,8,8,6,6,4,4,3,1,1,0,12]) <$> use current
