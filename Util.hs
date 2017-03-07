module Util where

import Data.List
import Data.List.Split

-- 匿名化
addStringN :: String -> String -> Int -> String
addStringN x _ 0 = x 
addStringN x y n = addStringN (x ++ y) y (n - 1)
  
anonymizeString :: String -> Int -> String
anonymizeString string degree =
  addStringN (reverse $ drop degree $ reverse string) "*" degree

anonymizeRecord :: [String] -> [Int] -> [String]
anonymizeRecord record hidden_list = zipWith anonymizeString record hidden_list

anonymizeDataset :: [[String]] -> [Int] -> [[String]]
anonymizeDataset dataset hidden_list =
  map (\dataset -> anonymizeRecord dataset hidden_list) dataset

-- 有用度の計算
countStringUsefulness :: String -> Double
countStringUsefulness string =
  (fromIntegral . length) (elemIndices '*' string) /
  (fromIntegral . length) string

countRecordUsefulness :: [String] -> Double
countRecordUsefulness record =
  (sum $ map countStringUsefulness record) / (fromIntegral . length) record

countDatasetUsefullness :: [[String]] -> Double
countDatasetUsefullness dataset =
  (sum $ map countRecordUsefulness dataset) / (fromIntegral . length) dataset

--  匿名化の最大値から匿名化段階の全ての組み合わせを作成する

getRecordElemLength :: [String] -> [Int]
getRecordElemLength record = map length record

makeCombination :: [[Int]] -> [[Int]] -> [[Int]]
makeCombination x y = do
  a <- x
  b <- y
  return $ a ++ b

makeAllAnonymousDegreesCombination :: [Int]  -> [[Int]]
makeAllAnonymousDegreesCombination max_degrees =
  foldl makeCombination [[]]  partial_degrees
  where
    candidate_degrees = map (\y-> [1..y]) max_degrees
    partial_degrees = map (\zs-> [ [z] | z <- zs ]) candidate_degrees

-- 単一匿名段階の匿名化後データセット群から
-- 可能な複数の匿名段階を含むデータセットを作成する

getDataset :: [[[String]]] -> Int -> [[String]]
getDataset datasets index = datasets !! index

getRecord :: [[String]] -> Int -> [String] 
getRecord dataset index = dataset !! index

makeAnonymousDataset :: [[[String]]] -> [Int] -> [[String]]
makeAnonymousDataset datasets indices =
  zipWith getRecord ordered_datasets [0 .. max_index]
  where 
    ordered_datasets = map (\index-> getDataset datasets index) indices
    max_index = length indices - 1

makeAllAnonymousDatasets :: [[[String]]] -> [[Int]] -> [[[String]]]
makeAllAnonymousDatasets datasets indices_list =
  map (\indices->  makeAnonymousDataset datasets indices) indices_list

--k匿名化されているか確認する．
isK_anonymized :: [[String]] -> Int -> Bool
isK_anonymized dataset k = and $ map isRepeat $ chunksOf k dataset

isRepeat :: Eq a => [a] -> Bool
isRepeat list = and $ map (\elem-> list !! 0 == elem) list




