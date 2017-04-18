{-# LANGUAGE DeriveGeneric #-}
module Util where

import Data.List
import Data.List.Split
import System.Random.Shuffle
import Data.Maybe
import GHC.Generics
import Data.Yaml

data Config = Config { solution :: String
                     , k_size   :: Int
                     , assoc_id :: Int
                     , filepath :: FilePath
                     } deriving (Show, Generic)
instance FromJSON Config

-- 高速化のための正格版
map' :: (a -> b) -> [a] -> [b]
map' f (a:as) = x `seq` x : map' f as
  where
    x = f a
map' _ _ = []

zipWith' ::  (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = x `seq` x : zipWith' f as bs
  where
    x = f a b
zipWith' _ _ _ = []

readConfig :: FilePath -> IO Config
readConfig path =
  (decodeFile path :: IO (Maybe Config)) >>= \conf->
  return $ fromMaybe conf_error conf
  where
    conf_error = (Config { solution = ""
                         , k_size   = 0
                         , assoc_id = 0
                         , filepath = ""
                         })

  -- データセットの取得
readDatasetFromCSV :: String -> IO [[String]]
readDatasetFromCSV path = do
  f <- readFile path
  let dataset = map' (\strings-> splitOn "," strings) $ lines f
  return dataset

-- 匿名化
addStringN :: String -> String -> Int -> String
addStringN x _ 0 = x
addStringN x y n = addStringN (x ++ y) y (n - 1)

anonymizeString :: String -> Int -> String
anonymizeString string degree =
  addStringN (reverse $ drop degree $ reverse string) "*" degree

anonymizeRecord :: [String] -> [Int] -> [String]
anonymizeRecord record degrees = zipWith' anonymizeString record degrees

anonymizeDataset :: [[String]] -> [Int] -> [[String]]
anonymizeDataset dataset degrees =
  map' (\dataset' -> anonymizeRecord dataset' degrees) dataset

-- 有用度の計算
countStringUsefulness :: String -> Double
countStringUsefulness string = 1.0 -
  (fromIntegral . length) (elemIndices '*' string) /
  (fromIntegral . length) string

countRecordUsefulness :: [String] -> Double
countRecordUsefulness record =
  (sum $ map' countStringUsefulness record) / (fromIntegral . length) record

countDatasetUsefulness :: [[String]] -> Double
countDatasetUsefulness dataset =
  (sum $ map' countRecordUsefulness dataset) / (fromIntegral . length) dataset

--  匿名化の最大値から匿名化段階の全ての組み合わせを作成する
makeCombination :: [[Int]] -> [[Int]] -> [[Int]]
makeCombination x y = do
  a <- x
  b <- y
  return $ a ++ b

makeAllAnonymousDegreesCombination :: [Int]  -> [[Int]]
makeAllAnonymousDegreesCombination max_degrees =
  foldl' makeCombination [[]]  partial_degrees
  where
    candidate_degrees = map' (\y-> [0..y]) max_degrees
    partial_degrees = map' (\zs-> [ [z] | z <- zs ]) candidate_degrees

-- k匿名化されているか確認する
isRepeat :: Eq a => [a] -> Bool
isRepeat list = and $ map (\element-> list !! 0 == element) list

-- 匿名化データを標準出力へ書き込み
printDatasetAsCSV :: [[String]] -> IO()
printDatasetAsCSV dataset = do
  let lines' = map' (\record-> intercalate "," record) dataset
  mapM_ putStrLn lines'

-- 識別子と機密情報の切り離し
separateDataset :: Int -> [[String]] -> [([String], [String])]
separateDataset sep dataset = map' (splitAt sep) dataset

getAssociativeIdentifier :: [([String],[String])] -> [[String]]
getAssociativeIdentifier separated_dataset = map' fst separated_dataset

getConfidentialInfo :: [([String],[String])] -> [[String]]
getConfidentialInfo separated_dataset = map' snd separated_dataset

-- N桁の値の一致を確認する
hasCommonNDigits :: Int -> [String] -> Bool
hasCommonNDigits n data' = and $ map' (isPrefixOf common) data'
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

-- 最大有用度の匿名化データセットを返す
updateMoreUsefulDataset :: [[String]] -> [[String]] -> [[String]]
updateMoreUsefulDataset current [] = current
updateMoreUsefulDataset [] new     = new
updateMoreUsefulDataset current new
  | current_score >= new_score = current
  | otherwise                  = new
  where
    current_score  = countDatasetUsefulness current
    new_score      = countDatasetUsefulness new

-- k行の局所最適有用度の匿名後データセットを返す
anonymizePartialOpt :: [[String]] -> [[String]]
anonymizePartialOpt partial  =
  foldl' updateMoreUsefulDataset []
  $ filter isRepeat
  $ map' (anonymizeDataset partial) degrees_combination
  where
    max_degrees = map' length $ partial !! 0
    common_degrees = map' countCommonDigits $ transpose partial
    anonymous_degrees = zipWith' (-) max_degrees common_degrees
    degrees_combination = makeAllAnonymousDegreesCombination anonymous_degrees

-- 匿名化後データセットをk個に分け，k個の中とk個でシャッフル
shuffleDataset :: Int -> [[String]] -> IO [[String]]
shuffleDataset k dataset = (\b-> fmap concat $ shuffleM $ chunksOf k b) =<< a
  where
    a = mconcat $ map' shuffleM $ chunksOf k dataset

getSubOptimalSolution :: Int -> [[String]] -> [[String]]
getSubOptimalSolution k dataset = foldl' (++) [] $ map' anonymizePartialOpt partials
  where
    partials = chunksOf k dataset

isK_anonymized :: Int -> [[String]] -> Bool
isK_anonymized k dataset = and $ map isRepeat $ chunksOf k dataset

getNRecords :: [[String]] -> [Int] -> [[String]]
getNRecords dataset record_indices = map' ((!!) dataset) record_indices

 --- 同データセットから取ったk個のレコードを連続させたデータセットのリストを作成
getOptimalCandidates :: Int -> [[String]]  -> [[[String]]]
getOptimalCandidates k dataset = do
  di <- dataset_indices
  ri <- record_indices
  return $ concat $ zipWith' (\d r-> getNRecords ((!!) datasets d) r) di ri
  where
    max_degrees = map' length $ dataset !! 0
    common_degrees = map' countCommonDigits $ transpose dataset
    anonymous_degrees = zipWith' (-) max_degrees common_degrees
    degrees_combination = makeAllAnonymousDegreesCombination anonymous_degrees
    datasets = map' (anonymizeDataset dataset) degrees_combination

    makeAllDatasetIndicesCombination = makeAllAnonymousDegreesCombination
    cluster = length dataset `div` k
    all_record_order = permutations [0..(length dataset) - 1]

    record_indices = map' (chunksOf k) all_record_order
    dataset_indices = makeAllDatasetIndicesCombination
                      $ replicate cluster (length datasets - 1)

getOptimalSolution :: Int -> [[String]] -> [[String]]
getOptimalSolution k dataset =
  foldl' updateMoreUsefulDataset [] $ filter (isK_anonymized k)
  $ getOptimalCandidates k dataset

-- Main
k_anonymizeOpt :: Int -> String -> IO()
k_anonymizeOpt k path = do

  f <- readFile path
  let raw_data = separateDataset 2 $ map' (splitOn ",") $ lines f
  let assoc_ids  = getAssociativeIdentifier raw_data
  let confidential_info = getConfidentialInfo raw_data

  let optimal_solution = getOptimalSolution k assoc_ids

  let max_usefulness = countDatasetUsefulness optimal_solution
  ans <- shuffleDataset k $ zipWith' (++) optimal_solution confidential_info

  printDatasetAsCSV ans
  putStrLn $ "maximum usefulness: " ++ (show max_usefulness)


k_anonymizeSubOpt :: Int -> String -> IO()
k_anonymizeSubOpt k path = do
  f <- readFile path
  let raw_data = sort $ separateDataset 2 $ map' (splitOn ",") $ lines f
  let assoc_ids = getAssociativeIdentifier raw_data
  let confidential_info = getConfidentialInfo raw_data

  let suboptimal_solution = getSubOptimalSolution k  assoc_ids
  let max_usefulness = countDatasetUsefulness suboptimal_solution
  ans <- shuffleDataset k $ zipWith' (++) suboptimal_solution confidential_info

  printDatasetAsCSV ans
  putStrLn $ "maximum usefulness: " ++ (show max_usefulness)

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
    k_error = "WARNING!!\n" ++
              "value 'k' is incorrect!\n" ++
              "'k' is divisor of dataset column size"
    usage   = "usage: k_anonymize [-o | -s] [k] [input]\n" ++
              "         [-o]    : getting optimal k-anonymity solution\n" ++
              "         [-s]    : getting suboptimal k-anonymity solution\n" ++
              "         [k]     : k-anonymity degree\n" ++
              "         [input] : raw data file path"

k_anonymity' :: Config -> IO()
k_anonymity' config
  | option == "optimal"     = do dataset <- readDatasetFromCSV path
                                 if length dataset `mod` k == 0
                                   then k_anonymizeOpt k path
                                   else putStrLn k_error
  | option == "suboptimal"  = do dataset <- readDatasetFromCSV path
                                 if length dataset `mod` k == 0
                                   then k_anonymizeSubOpt k path
                                   else putStrLn k_error
  | otherwise               = putStrLn usage
  where
    option    = solution config
    k         = k_size config
    assoc     = assoc_id config
    path      = filepath config
    k_error   = "WARNING!!\n" ++
                "value 'k' is incorrect!\n" ++
                "'k' is divisor of dataset column size"
    usage     = "check setting -> config.yaml: \n" ++
                "      solution: String \n" ++
                "      k_size:   Int \n" ++
                "      assoc_id: Int \n" ++
                "      filepath: FilePath \n"
