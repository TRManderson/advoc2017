{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Char
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Attoparsec.Text
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS (topsort, dff)
import Data.Discrimination.Grouping (groupWith)
import Data.Discrimination.Sorting (sortWith)
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.Text.IO as TIO

data Prog = Prog {pName :: Text, pWeight :: Int, pProgs :: [Text]} deriving (Eq, Show)
parser :: Parser [Prog]
parser = many' (line <* endOfLine)
  where
    extra = option [] $ string " -> " *> sepBy1' name (string ", ")
    name = takeWhile1 isAlpha
    weight = char '(' *> decimal <* char ')'
    line = Prog <$> name <*> (space *> weight) <*> extra

toGraph :: [Prog] -> Gr Text ()
toGraph ps = run_ G.empty $ do
  insMapNodesM $ pName <$> ps
  insMapEdgesM [(pName p, p', ()) | p <- ps, p' <- pProgs p]

toMap :: [Prog] -> M.Map Text Int
toMap ps = M.fromList [(a, b) | (Prog a b _) <- ps]

updatedTree :: M.Map Text Int -> Gr Text () -> T.Tree Node -> T.Tree (Int, Int)
updatedTree m g = fixedTree . fmap ((m M.!) . fromJust . lab g)
  where
    fixedTree (T.Node r fs) = let
      fs' = fmap fixedTree fs
      s = sum . fmap (fst . T.rootLabel) $ fs'
      in T.Node (r + s, r) fs'

mkTree :: [Prog] -> T.Tree (Int, Int)
mkTree = do
  g <- toGraph
  m <- toMap
  let [tree] = dff (topsort g) g
  pure (updatedTree m g tree)


data TreeStep = Unchecked (Int, Int) [TreeStep]
              | AllEqual (Int, Int)
              | NoneLeft Int
              | Soln Int
              deriving Show

isSoln :: TreeStep -> Bool
isSoln (Soln _) = True
isSoln _ = False

grouper :: TreeStep -> Int
grouper (AllEqual (v, _)) = v
grouper (NoneLeft v) = v

stotal :: TreeStep -> Int
stotal (AllEqual (v, _)) = v
stotal (NoneLeft v) = v
snode :: TreeStep -> Int
snode (AllEqual (_, v)) = v
snode (NoneLeft v) = v

mkStep :: T.Tree (Int, Int) -> TreeStep
mkStep (T.Node r ls) = Unchecked r (fmap mkStep ls)
check :: TreeStep -> TreeStep
check (Unchecked (_, r) []) = NoneLeft r
check (Unchecked (l, r) ns) = if any isSoln ns' then head . filter isSoln $ ns'
  else if length grs == 1 then AllEqual (l, r)
  else Soln (snode gr + (stotal gr' - stotal gr) )
  where
    ns' = fmap check ns
    grs = sortWith length . groupWith grouper $ ns'
    [[gr], grs'] = grs
    gr' = head grs'
check _ = error "The name says it: checks, doesn't take already checked"

solve = check . mkStep

main :: IO ()
main = do
  eProgs <- parseOnly parser <$> TIO.getContents
  case eProgs of
    Right v -> print . solve . mkTree $ v
    Left v -> error v
