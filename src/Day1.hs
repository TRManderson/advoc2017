module Main where

solnA :: String -> Int
solnA s = snd $ foldr step (head conv, 0) conv
  where
    conv = map (read . (:[])) s
    step n (l, acc) = case l == n of
      True -> (l, acc + n)
      False -> (n, acc)


main :: IO ()
main = do
  print . solnA =<< getLine
