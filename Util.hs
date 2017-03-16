module Util where

import Data.List
import Data.List.Split

-- データセットの取得
readDatasetFromCSV :: String -> IO [[String]]
readDatasetFromCSV path = do
  f <- readFile path
  let dataset = map (\strings-> splitOn "," strings) $ lines f
  return dataset

-- 匿名化
addStringN :: String -> String -> Int -> String
addStringN x _ 0 = x 
addStringN x y n = addStringN (x ++ y) y (n - 1)
  
anonymizeString :: String -> Int -> String
anonymizeString string degree =
  addStringN (reverse $ drop degree $ reverse string) "*" degree

anonymizeRecord :: [String] -> [Int] -> [String]
anonymizeRecord record degrees = zipWith anonymizeString record degrees

anonymizeDataset :: [[String]] -> [Int] -> [[String]]
anonymizeDataset dataset degrees =
  map (\dataset' -> anonymizeRecord dataset' degrees) dataset

-- 有用度の計算
countStringUsefulness :: String -> Double
countStringUsefulness string = 1.0 - 
  (fromIntegral . length) (elemIndices '*' string) /
  (fromIntegral . length) string

countRecordUsefulness :: [String] -> Double
countRecordUsefulness record =
  (sum $ map countStringUsefulness record) / (fromIntegral . length) record

countDatasetUsefulness :: [[String]] -> Double
countDatasetUsefulness dataset =
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

makeAllDatasetIndicesCombination :: [Int]  -> [[Int]]
makeAllDatasetIndicesCombination max_indices =
  foldl makeCombination [[]]  partial_indices
  where
    candidate_indices = map (\y-> [0..y]) max_indices
    partial_indices = map (\zs-> [ [z] | z <- zs ]) candidate_indices

makeAnonymousDataset :: [[[String]]] -> [Int] -> [[String]]
makeAnonymousDataset datasets indices =
  zipWith getRecord ordered_datasets [0 .. max_index]
  where 
    ordered_datasets = map (\index-> getDataset datasets index) indices
    max_index = length indices - 1

makeAllAnonymousDatasets :: [[[String]]] -> [[Int]] -> [[[String]]]
makeAllAnonymousDatasets datasets indices_list =
  map (\indices->  makeAnonymousDataset datasets indices) indices_list

-- データセットから可能な全ての並び替えを作成する
-- permutationsでOK
permutateDataset :: [[String]] -> [[[String]]]
permutateDataset dataset = permutations dataset

-- k匿名化されているか確認する
isRepeat :: Eq a => [a] -> Bool
isRepeat list = and $ map (\element-> list !! 0 == element) list

isK_anonymized :: [[String]] -> Int -> Bool
isK_anonymized dataset k = and $ map isRepeat $ chunksOf k dataset

-- 匿名化データを標準出力へ書き込み
printDatasetAsCSV :: [[String]] -> IO()
printDatasetAsCSV dataset = do
  let lines' = map (\record-> intercalate "," record) dataset
  mapM_ putStrLn lines'
  putStrLn ""
  
