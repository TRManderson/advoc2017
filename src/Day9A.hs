module Main where
import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.IO as T

data AST = Group [AST] | Garbage T.Text

parser :: Parser AST
garbage = Garbage <$> (char '<' *> mid <* char '>')
  where
    mid = do
      fs <- P.takeWhile (\c -> (c /= '>') && (c /= '!'))
      c <- peekChar'
      case c of
        '>' -> pure fs
        '!' -> do
          char '!'
          c <- anyChar
          rest <- mid
          pure $ fs <> T.pack ['!', c] <> rest
group = Group <$> (char '{' *> (group <|> garbage) `sepBy` char ',' <* char '}')
parser = group

value :: AST -> Int
value = value' 1
  where
    value' n (Group vs) = n + sum (fmap (value' (n+1)) vs)
    value' n (Garbage _) = 0

main :: IO ()
main = do
  c <- T.getLine
  case parseOnly parser c of
    Left v -> print v
    Right v -> print (value v)
