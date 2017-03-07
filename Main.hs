module Main(main) where

import Data.List
import Data.List.Split
import Util


main = do

  -- csv読み込み
  f <- readFile "./Data/dataset100.txt"
  let dataset = map (\dataset -> splitOn "," dataset) $ lines f
  -- putStrLn "getting dataset done..."
  -- print dataset

  -- 1データセットから作成できる全ての匿名段階の匿名化データセット作成
  let all_anonymous_degrees =
        makeAllAnonymousDegreesCombination $ getRecordElemLength $ dataset !! 0
  let processed =
        map (\degrees-> anonymizeDataset dataset degrees) all_anonymous_degrees
  putStrLn "anonymization done..."
  print processed

  

  -- リストの
  

  -- 
