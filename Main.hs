module Main(main) where

import System.Environment
import Util

main :: IO()
main = getArgs >>= k_anonymize
