module Main (main) where

import Lib ( tokenize, calculate )

main :: IO ()
main = interact $ unlines . fmap (show . calculate . tokenize) . lines
