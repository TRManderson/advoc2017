{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text as A

parser = many1 sc
  where sc = (,) <$> (decimal <* string ": ") <*> (decimal <* endOfLine)

fromRight (Right v) = v
fromRight (Left e) = error e

ff (p, r) = p `mod` ((r-1) * 2) == 0
fm (p, r) = p * r

main = print . sum . map fm . filter ff . fromRight . parseOnly parser =<< T.getContents
