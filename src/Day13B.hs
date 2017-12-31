{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text as A

parser = many1 sc
  where sc = (,) <$> (decimal <* string ": ") <*> (decimal <* endOfLine)

fromRight (Right v) = v
fromRight (Left e) = error e

ff s (p, r) = (p + s) `mod` ((r-1) * 2) == 0

solve :: [(Int, Int)] -> Int
solve is = fst . head . filter snd . map solve' $ [0..]
  where
    solve' a = (a, not . any (ff a)  $ is)

main = print . solve . fromRight . parseOnly parser =<< T.getContents
