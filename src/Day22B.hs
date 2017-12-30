{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables, TemplateHaskell #-}
module Main where
import Control.Monad.Trans.State.Strict
import Control.Lens
import Data.Map as M
import Control.Monad
import Data.Maybe

data Status = Clean | Weakened | Infected | Flagged deriving (Show, Eq)
data Direction = DLeft | DRight | DUp | DDown deriving (Show, Eq)
data PState = PState { _infectionMap :: Map (Int, Int) Status, _direction :: Direction, _position :: (Int, Int), _count :: Int }
makeClassy ''PState
type Puzzle a = State PState a

currentNode :: Lens' PState Status
currentNode = lens getter setter
  where
    getter s = fromMaybe Clean $ M.lookup (s^. position) (s^.infectionMap)
    setter s a = infectionMap.at (s^. position) .~ Just a $ s

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

shiftOf :: Status -> Direction
shiftOf Clean = DLeft
shiftOf Weakened = DUp
shiftOf Infected = DRight
shiftOf Flagged = DDown

next :: Status -> Status
next Clean = Weakened
next Weakened = Infected
next Infected = Flagged
next Flagged = Clean

forward :: Direction -> (Int, Int) -> (Int, Int)
forward d (x, y) = case d of
  DUp -> (x, y - 1)
  DDown -> (x, y + 1)
  DLeft -> (x - 1, y)
  DRight -> (x + 1, y)

mkInitial :: [String] -> PState
mkInitial s = PState mData DUp (initRow, initCol) 0 where
  status '.' = Clean
  status '#' = Infected
  status _ = error "Not a valid char"
  initRow = length s `div` 2
  initCol = length (head s) `div` 2
  mData = M.fromList $ do
    (row, rowData) <- zip [0..] s
    (col, colData) <- zip [0..] rowData
    return ((col, row), status colData)

step :: Puzzle ()
step = do
  status <- use currentNode
  direction <>= shiftOf status
  when (status == Weakened) $ count += 1
  currentNode %= next
  d <- use direction
  position %= forward d

main :: IO ()
main = do
  initial <- mkInitial . lines <$> getContents
  print $ evalState (replicateM_ 10000000 step >> use count) initial
