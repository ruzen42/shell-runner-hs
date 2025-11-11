module Main (main) where

import Lib
import GHC.Conc (getNumProcessors)
import System.Environment (getArgs)

main :: IO ()
main = do
    n <- getNumProcessors
    args <- getArgs
    let port = case args of
            (x:_) -> read x :: Int
            []    -> 8080
    startApp port n
