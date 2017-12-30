{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables, TemplateHaskell #-}
module Main where
import Control.Monad.Trans.State
import Control.Lens
import Data.Map as M
import Control.Monad
import Data.Maybe
import Data.Bool (bool)

data Direction = DLeft | DRight | DUp | DDown deriving Show
data PState = PState { _infectionMap :: Map (Int, Int) Bool, _direction :: Direction, _position :: (Int, Int), _count :: Int }
makeClassy ''PState
type Puzzle a = State PState a

currentNode :: Lens' PState Bool
currentNode = lens getter setter
  where
    getter s = fromMaybe False $ M.lookup (s^. position) (s^.infectionMap)
    setter s a = infectionMap.at (s^. position) .~ Just a $ s

forward :: Direction -> (Int, Int) -> (Int, Int)
forward d (x, y) = case d of
  DUp -> (x, y - 1)
  DDown -> (x, y + 1)
  DLeft -> (x - 1, y)
  DRight -> (x + 1, y)

instance Monoid Direction where
  mempty = DUp
  mappend DUp = id
  mappend DRight = \case
    DUp -> DRight
    DRight -> DDown
    DDown -> DLeft
    DLeft -> DUp
  mappend DDown = mappend DRight . mappend DRight
  mappend DLeft = mappend DDown . mappend DRight

mkInitial :: [String] -> PState
mkInitial s = PState mData DUp (initRow, initCol) 0 where
  isInfected '.' = False
  isInfected '#' = True
  isInfected _ = error "Not a valid char"
  initRow = length s `div` 2
  initCol = length (head s) `div` 2
  mData = M.fromList $ do
    (row, rowData) <- zip [0..] s
    (col, colData) <- zip [0..] rowData
    return ((col, row), isInfected colData)

step :: Puzzle ()
step = do
  infected <- use currentNode
  direction <>= bool DLeft DRight infected
  unless infected $ count += 1
  currentNode %= not
  d <- use direction
  position %= forward d

main :: IO ()
main = do
  initial <- mkInitial . lines <$> getContents
  print $ evalState (replicateM_ 10000 step >> use count) initial
