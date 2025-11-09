module Main (main) where

import Lib
import GHC.Conc (getNumProcessors)

main :: IO ()
main = do
    n <- getNumProcessors
    startApp 8081 n
