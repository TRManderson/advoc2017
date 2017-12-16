{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Graph.Inductive.Graph (mkUGraph)
import Data.Graph.Inductive.Basic (undir)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (reachable)
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T


buildGraph :: [(Int, [Int])] -> Gr () ()
buildGraph ns = undir $ mkUGraph nodes edges
  where
    nodes = fst <$> ns
    edges = do
      (nodeA, nodesB) <- ns
      nodeB <- nodesB
      return (nodeA, nodeB)


pLine = (,) <$> (decimal <* string " <-> ") <*> sepBy1 decimal (string ", ")
p :: Parser [(Int, [Int])]
p = many1' (pLine <* endOfLine)

main = do
  c <- T.getContents
  case parseOnly p c of
    Left v -> error v
    Right v -> print . length . reachable 0 . buildGraph $ v
