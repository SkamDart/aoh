module Main (main) where

import Solutions

main :: IO ()
main = dayTwo "data/day1.txt" >>= (\c -> putStrLn (show c))
