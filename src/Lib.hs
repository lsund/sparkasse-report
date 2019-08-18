module Lib where

import Data.List.Split (splitAt)

csvFile :: FilePath
csvFile = "/home/lsund/Documents/personal/economy/helena/bank-summary/apr/20190818-123093569-umsatz.CSV"

printLine :: String -> IO ()
printLine xs = print (splitAt ";" xs)

someFunc :: IO ()
someFunc = readFile csvFile >>= mapM_ print . lines
