{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative
import Data.Traversable
import Data.Char
import Data.Text
import Data.Attoparsec.Text
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS (topsort')
import qualified Data.Text.IO as TIO

data Prog = Prog {pName :: Text, pWeight :: Int, pProgs :: [Text]} deriving (Eq, Show)

extra = option [] $ string " -> " *> sepBy1' name (string ", ")
name = takeWhile1 isAlpha
weight = char '(' *> decimal <* char ')'
line = Prog <$> name <*> (space *> weight) <*> extra
parser = many' (line <* endOfLine)

toGraph :: [Prog] -> Gr Text ()
toGraph ps = run_ G.empty $ do
  insMapNodesM $ pName <$> ps
  insMapEdgesM [(pName p, p', ()) | p <- ps, p' <- pProgs p]

main = do
  eProgs <- parseOnly parser <$> TIO.getContents
  case eProgs of
    Right v -> print . Prelude.head . topsort' . toGraph $ v
    Left v -> error v