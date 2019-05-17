module Main where

import Lib
import System.Environment
import System.Console.GetOpt

main :: IO ()
main = do
        args <- getArgs
        parser args
