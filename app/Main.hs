module Main (main) where

import Lib ( tokenize )

main :: IO ()
main = print $ tokenize "(1 + 2)/ 3"
