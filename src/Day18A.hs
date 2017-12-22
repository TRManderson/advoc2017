{-# LANGUAGE OverloadedStrings, LambdaCase, TemplateHaskell #-}
module Main where
import Prelude as P
import Data.Bool
import Data.Monoid
import Data.Maybe
import Control.Lens
import Data.Char
import Data.Functor
import Data.Vector as V
import Data.Vector.Lens
import qualified Data.Map as M
import Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State

type Register = Char
data Value = R Register | V Int
  deriving (Eq)
instance Show Value where
  show (R c) = [c]
  show (V n) = show n
makePrisms ''Value
data Statement = Snd Register
               | Set Register Value
               | Add Register Value
               | Mul Register Value
               | Mod Register Value
               | Rcv Value
               | Jgz Value Value
               deriving (Eq, Show)

data IState = IState { _regMap :: M.Map Register Int, _lastRecv :: Int, _tape :: Vector Statement, _counter :: Int}
makeLenses ''IState
type Interp a = State IState a

getReg :: Register -> Interp Int
getReg r = use (regMap.at r) >>= \case
  Just n -> return n
  Nothing -> regMap.at r <?= 0

getVal :: Value -> Interp Int
getVal (R r) = getReg r
getVal (V x) = pure x

setReg r = modifyReg r . const
modifyReg :: Register -> (Int -> Int) -> Interp ()
modifyReg r f = regMap.at r %= Just . maybe (f 0) f

checkBounds :: Interp Bool
checkBounds = do
  t <- use tape
  c <- use counter
  pure $ c >= 0 && c < V.length t

interp :: Statement -> Interp ()
interp s = interp' >> (counter += 1)
  where
    interp' = case s of
      Snd r -> use (regMap.at r) >>= (lastRecv .=) . fromJust
      Set r v -> getVal v >>= setReg r
      Add r v -> getVal v >>= modifyReg r . (+)
      Mul r v -> getVal v >>= modifyReg r . (*)
      Mod r v -> getVal v >>= modifyReg r . flip mod
      Rcv _ -> return ()
      Jgz r' v' -> do
        r <- getVal r'
        when (r > 0) $ do
          v <- getVal v'
          counter += (v-1)

solve :: [Statement] -> Int
solve stmt = evalState solve' (IState mempty (-10000) (fromList stmt) 0)
  where
    solve' = do
      c <- use counter
      instr <- preuse (tape . ix  c)
      case instr of
        Just (Rcv v') -> do
          v <- getVal v'
          if v > 0 then
            use lastRecv
          else
            (counter += 1) >> solve'
        Just x -> interp x >> solve'
        Nothing -> error ("Index " <> show c <> " is out of bounds")

pReg = satisfy isAlpha
pVal = (R <$> pReg) <|> (V <$> (decimal <|> signed decimal))
pStatement :: Parser Statement
pStatement = choice
  [ (Snd <$ string "snd ") <*> pReg
  , (Set <$ string "set ") <*> pReg <*> (char ' ' *> pVal)
  , (Add <$ string "add ") <*> pReg <*> (char ' ' *> pVal)
  , (Mul <$ string "mul ") <*> pReg <*> (char ' ' *> pVal)
  , (Mod <$ string "mod ") <*> pReg <*> (char ' ' *> pVal)
  , (Rcv <$ string "rcv ") <*> pVal
  , (Jgz <$ string "jgz ") <*> pVal <*> (char ' ' *> pVal)
  ]
p = many1 (pStatement <* endOfLine)

main = do
  c <- T.getContents
  case parseOnly p c of
    Left v -> error v
    Right s -> do
      print s
      print . solve $ s
