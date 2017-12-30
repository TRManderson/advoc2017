{-# LANGUAGE OverloadedStrings, RankNTypes, TemplateHaskell #-}
module Main where
import Prelude as P
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text as A
import Data.Char
import Control.Applicative
import Data.Functor
import Data.Maybe
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans.State
import Control.Lens

data Direction = Inc | Dec
type Register = T.Text
data Instruction = Instr Register Direction Int Register (Int -> Int -> Bool) Int
parser = many1 (line <* endOfLine)
  where
    reg = A.takeWhile1 isAlpha
    num = signed decimal <|> decimal
    dir = (Inc <$ string "inc") <|> (Dec <$ string "dec")
    fn = choice
      [ (==) <$ string "=="
      , (/=) <$ string "!="
      , (>=) <$ string ">="
      , (<=) <$ string "<="
      , (>) <$ string ">"
      , (<) <$ string "<"
      ]
    line = do
      r1 <- reg <* skipSpace
      d <- dir <* skipSpace
      v1 <- num <* string " if "
      r2 <- reg <* skipSpace
      c <- fn <* skipSpace
      v2 <- num
      pure (Instr r1 d v1 r2 c v2)

type RegMap = M.Map T.Text Int
data PState = PState {_regMap :: RegMap, _maxVal :: Int}
makeLenses ''PState
type Puzzle a = State PState a

regVal :: T.Text -> Lens' PState Int
regVal v = regMap . lens getter setter
  where
    getter :: RegMap -> Int
    getter s = fromMaybe 0 $ s^.at v
    setter :: RegMap -> Int -> RegMap
    setter s a = at v .~ Just a $ s

exec :: Instruction -> Puzzle ()
exec (Instr r1 d v1 r2 c v2) = do
  r2' <- use $ regVal r2
  when (c r2' v2) $ do
    r1' <- case d of
      Inc -> regVal r1 <+= v1
      Dec -> regVal r1 <-= v1
    maxVal %= max r1'

solve instrs = flip evalState (PState mempty 0) $ do
  traverse exec instrs
  use maxVal

main :: IO ()
main = do
  c <- T.getContents
  case parseOnly parser c of
    Right v -> print $ solve v
    Left v -> error v
