module Main (main) where

import Lib
import GHC.Conc (getNumProcessors)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
    n <- getNumProcessors
    args <- getArgs
    let port = case args of
            (x:_) -> maybe 8080 id (readMaybe x :: Maybe Int)
            []    -> 8080
    startApp port n
