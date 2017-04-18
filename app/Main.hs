module Main(main) where
import Util
import System.Environment


main :: IO()
-- main = getArgs >>= k_anonymize
main = readConfig "config.yaml" >>= k_anonymity'