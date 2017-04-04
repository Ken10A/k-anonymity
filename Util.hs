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

k_anonymizeOpt :: Int -> String -> IO()
k_anonymizeOpt k path = do
  -- データセットを[[String]]で取得
  f <- readFile path
  let raw_data = separateDataset 3 $ map (\string-> splitOn "," string) $ lines f
  let dataset  = getAssociativeIdentifier raw_data
  let confidential_info = getConfidentialInfo raw_data
  
  -- 1レコードを匿名化できる全ての匿名段階のリストを作成する
  let all_anonymous_degrees =
        makeAllAnonymousDegreesCombination
        $ zipWith (-) (map length $ dataset !! 0)
        $ map countCommonDigits $ transpose dataset
        
  let all_single_anonymity_datasets =
        map (\degrees-> anonymizeDataset dataset degrees) all_anonymous_degrees

  -- -- あらゆる匿名段階のレコードを含む匿名化データセットを作成する
  -- let all_dataset_indices_combination =
  --       makeAllDatasetIndicesCombination $
  --       replicate (length dataset) $ (length all_single_anonymity_datasets) - 1
        
  -- let all_multiple_anonymity_datasets = makeAllAnonymousDatasets
  --       all_single_anonymity_datasets all_dataset_indices_combination

  -- -- 各データセットを並び替える
  -- let  permutated_datasets =
  --        concat $ map permutateDataset all_multiple_anonymity_datasets

  -- -- k-匿名化されていないデータセットをフィルター
  -- let all_k_anonymized_datasets =
  --       filter (\datasets-> isK_anonymized datasets k) permutated_datasets

  -- 全てのk匿名化データセットを作成
  let all_k_anonymized_datasets =
        makeAllK_anonymizedDataset all_single_anonymity_datasets k

  -- 有用度計算
  let usefulness_list = map countDatasetUsefulness all_k_anonymized_datasets

  -- 有用度最大のk-匿名化データセットを取り出す
  let most_useful_datasets =
        map (\index-> getDataset all_k_anonymized_datasets index) $
        elemIndices (maximum usefulness_list) usefulness_list
   
  mapM_ printDatasetAsCSV $ map (\assoc_ids-> zipWith (++) assoc_ids confidential_info) most_useful_datasets
  putStrLn $ "maximum usefulness: " ++ (show . maximum) usefulness_list

k_anonymizeSubOpt :: Int -> String -> IO()
k_anonymizeSubOpt k path = do
  -- データセットを[[String]]で取得
  f <- readFile path
  let raw_data = sort $ separateDataset 3 $ map (\string-> splitOn "," string) $ lines f
  let dataset  = getAssociativeIdentifier raw_data
  let confidential_info = getConfidentialInfo raw_data 

  -- 1レコードを匿名化できる全ての匿名段階のリストを作成する
  let all_anonymous_degrees =
        makeAllAnonymousDegreesCombination
        $ zipWith (-) (map length $ dataset !! 0)
        $ map countCommonDigits $ transpose dataset
        
  let all_single_anonymity_datasets =
        map (\degrees-> anonymizeDataset dataset degrees) all_anonymous_degrees

   -- k-匿名化されていないデータセットをフィルター
  let all_k_anonymized_datasets =
        filter (\datasets-> isK_anonymized datasets k)
        all_single_anonymity_datasets
        
   -- 有用度計算
  let usefulness_list = map countDatasetUsefulness all_k_anonymized_datasets

  -- 有用度最大のk-匿名化データセットを取り出す
  let most_useful_datasets =
        map (\index-> getDataset all_k_anonymized_datasets index) $
        elemIndices (maximum usefulness_list) usefulness_list

  mapM_ printDatasetAsCSV $ map (\assoc_ids-> zipWith (++) assoc_ids confidential_info) most_useful_datasets 
  putStrLn $ "maximum usefulness: " ++ (show . maximum) usefulness_list

k_anonymize :: [String] -> IO()
k_anonymize cmdlargs
  | length cmdlargs /= 3 = putStrLn usage
  | option == "-o"       = do dataset <- readDatasetFromCSV path
                              if length dataset `mod` k == 0
                                then k_anonymizeOpt k path
                                else putStrLn k_error
  | option == "-s"       = do dataset <- readDatasetFromCSV path
                              if length dataset `mod` k == 0
                                then k_anonymizeSubOpt k path
                                else putStrLn k_error
  | otherwise            = putStrLn usage
  where
    option  = cmdlargs !! 0
    k       = read $ cmdlargs !! 1
    path    = cmdlargs !! 2
    k_error = "value 'k' is incorrect!\n" ++
              "'k' is divisor of dataset size"
    usage   = "usage: k_anonymize [-o | -s] [k] [input]\n" ++
              "         [-o]    : getting optimal k-anonymity solution\n" ++
              "         [-s]    : getting suboptimal k-anonymity solution\n" ++
              "         [k]     : k-anonymity degree\n" ++
              "         [input] : raw data file path"

-- 識別子と機密情報の切り離し
separateDataset :: Int -> [[String]] -> [([String], [String])]
separateDataset sep dataset = map (splitAt sep) dataset

getAssociativeIdentifier :: [([String],[String])] -> [[String]]
getAssociativeIdentifier separated_dataset = map fst separated_dataset

getConfidentialInfo :: [([String],[String])] -> [[String]]
getConfidentialInfo separated_dataset = map snd separated_dataset

-- N桁の値の一致を確認する
hasCommonNDigits :: Int -> [String] -> Bool
hasCommonNDigits n data' = and $ map (isPrefixOf common) data'
  where
    common = take n $ data' !! 0
    
-- 上位桁から探した共通最大桁数の取得
-- 共通桁0のときに番兵として必ずTrueが返るので必ず停止する．
countCommonDigits :: [String] -> Int
countCommonDigits codes = loopCounting (length $ codes !! 0) codes
  where
    loopCounting n' codes'
      | hasCommonNDigits n' codes' = n'
      | otherwise = loopCounting (n' - 1) codes'

getNRecords :: [[String]] -> [Int] -> [[String]]
getNRecords dataset record_indices = map (getRecord dataset) record_indices

-- 同データセットから取ったk個のレコードを連続させたデータセットのリストを作成
makeAllK_anonymizedDataset :: [[[String]]] -> Int -> [[[String]]]
makeAllK_anonymizedDataset datasets k = do 
  di <- dataset_indices
  ri <- record_indices 
  zipWith (\d r-> getNRecords (getDataset datasets d) r) di ri
  where
    cluster = length (datasets !! 0) `div` k
    all_record_order = permutations [0..(length datasets) - 1]
    record_indices = map (chunksOf k) all_record_order
    dataset_indices = makeAllDatasetIndicesCombination
                      $ replicate cluster (length datasets - 1)

