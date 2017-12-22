{-# LANGUAGE OverloadedStrings, LambdaCase, TemplateHaskell #-}
module Main where
import Prelude as P
import Data.Bool
import Data.Monoid
import Data.Maybe
import Control.Lens
import Data.Char
import Data.Functor
import Control.Exception
import Data.Vector as V
import Data.Vector.Lens
import qualified Data.Map as M
import Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Control.Concurrent.Chan
import Control.Concurrent

type Register = Char
data Value = R Register | V Int
  deriving (Eq)
instance Show Value where
  show (R c) = [c]
  show (V n) = show n
makePrisms ''Value
data Statement = Snd Value
               | Set Register Value
               | Add Register Value
               | Mul Register Value
               | Mod Register Value
               | Rcv Register
               | Jgz Value Value
               deriving (Eq, Show)

data IState = IState { _regMap :: M.Map Register Int, _tape :: Vector Statement, _counter :: Int, _sendChan :: Chan Int, _recvChan :: Chan Int}
makeLenses ''IState
type Interp a = StateT IState IO a

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

interp :: Interp ()
interp = do
  c <- use counter
  instr <- preuse (tape . ix  c)
  case instr of
    Just s -> case s of
      Snd v -> do
        ch <- use sendChan
        v <- getVal v
        liftIO $ writeChan ch v
      Set r v -> getVal v >>= setReg r
      Add r v -> getVal v >>= modifyReg r . (+)
      Mul r v -> getVal v >>= modifyReg r . (*)
      Mod r v -> getVal v >>= modifyReg r . flip mod
      Rcv r -> do
        ch <- use recvChan
        v <- liftIO $ readChan ch
        setReg r v
      Jgz r' v' -> do
        r <- getVal r'
        when (r > 0) $ do
          v <- getVal v'
          counter += (v-1)
    Nothing -> error ("Index " <> show c <> " is out of bounds")
  counter += 1

solve :: [Statement] -> IO ()
solve stmt = do
  toIntermediate <- newChan
  fromIntermediate <- newChan
  returnChan <- newChan
  counter <- newIORef 0
  let
    state0 = IState (M.singleton 'p' 0) (fromList stmt) 0 returnChan fromIntermediate
    state1 = IState (M.singleton 'p' 1) (fromList stmt) 0 toIntermediate returnChan
    action = readChan toIntermediate >>= writeChan fromIntermediate >> modifyIORef counter (+1)
  forkIO $ evalStateT (forever interp) state1
  forkIO $ evalStateT (forever interp) state0
  catch (forever action) (const (readIORef counter >>= print) :: BlockedIndefinitelyOnMVar -> IO ())

pReg = satisfy isAlpha
pVal = (R <$> pReg) <|> (V <$> (decimal <|> signed decimal))
pStatement :: Parser Statement
pStatement = choice
  [ (Snd <$ string "snd ") <*> pVal
  , (Set <$ string "set ") <*> pReg <*> (char ' ' *> pVal)
  , (Add <$ string "add ") <*> pReg <*> (char ' ' *> pVal)
  , (Mul <$ string "mul ") <*> pReg <*> (char ' ' *> pVal)
  , (Mod <$ string "mod ") <*> pReg <*> (char ' ' *> pVal)
  , (Rcv <$ string "rcv ") <*> pReg
  , (Jgz <$ string "jgz ") <*> pVal <*> (char ' ' *> pVal)
  ]
p = many1 (pStatement <* endOfLine)

main = do
  c <- T.getContents
  case parseOnly p c of
    Left v -> error v
    Right s -> do
      print s
      solve s
