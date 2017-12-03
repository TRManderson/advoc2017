module Main where
import Prelude hiding (length)
import Control.Monad.Trans.State
import Data.Semigroup ((<>))
import Data.Foldable hiding (length)
import Data.Vector (Vector, fromList, length, (!))

type StateVal = (Vector Int, Int, Int)
getacc (_, _, a) = a

soln :: String -> Int
soln s = soln' . fromList . map (read . (:[])) $ s

soln' :: Vector Int -> Int
soln' vec = getacc $ execState proc state
  where
    len = length vec
    state = (vec <> vec, len `quot` 2, 0)
    proc = for_ [0..len-1] update

update :: Int -> State StateVal ()
update idx = do
  (vec, step, acc) <- get
  let
    next = idx + step
    val = (vec!idx)
    nval = (vec!next)
  case val == nval of
    True -> put $ (vec, step, acc+val)
    False -> pure ()

main :: IO ()
main = do
  print . soln =<< getLine
