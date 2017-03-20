module Main(main) where

import System.Environment
import Util

main :: IO()
main = k_anonymize =<< getArgs
