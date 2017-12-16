module Main where
import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.IO as T

data AST = Group [AST] | Garbage [T.Text]

parser :: Parser AST
garbage = Garbage <$> (char '<' *> mid <* char '>')
  where
    mid = do
      fs <- P.takeWhile (\c -> (c /= '>') && (c /= '!'))
      c <- peekChar'
      case c of
        '>' -> pure [fs]
        '!' -> do
          char '!'
          anyChar
          rest <- mid
          pure (fs:rest)
group = Group <$> (char '{' *> (group <|> garbage) `sepBy` char ',' <* char '}')
parser = group

value :: AST -> Int
value (Group vs) = sum (fmap value vs)
value (Garbage cs) = sum (fmap T.length cs)

main :: IO ()
main = do
  c <- T.getLine
  case parseOnly parser c of
    Left v -> print v
    Right v -> print (value v)
