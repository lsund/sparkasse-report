module Main where

import System.Directory (doesFileExist)
import Filter
import Lib
import Report
import Transaction
import Control.Monad.Extra (allM)

--------------------------------------------------------------------------------
-- Dummy
dummy :: [ReportRow]
dummy =
  [ ReportRow "Nightlife" (Just 10)
  , ReportRow "FastFood" (Just 5)
  , ReportRow "Nightlife" (Just 3)
  , ReportRow "Unknown" Nothing
  ]

dummyFilters :: [Filter]
dummyFilters =
  [Filter "Helena" _details "Unknown", Filter "Lohn" _ocr "Income", Filter "Kesting" _details "Apartment"]

dummyTransactions :: [Transaction]
dummyTransactions = [Transaction "foo" "bar" (Just (-500)) "baz"
                    , Transaction "foo" "bar" (Just 2800) "baz"]

csvFile :: FilePath
csvFile = "data/20190818-123093569-umsatz.CSV"

reportFile :: FilePath
reportFile = "data/report.csv"

filterFile :: FilePath
filterFile = "data/filters.csv"

--------------------------------------------------------------------------------
-- Program
main :: IO ()
main = do
  let requiredFiles = [csvFile, filterFile]
  inputFilesExist <- allM doesFileExist requiredFiles
  if inputFilesExist
    then do
        ts <- readTransactions csvFile
        filters <- Filter.deserialize filterFile
        --
        print $ (apply match ts) filters
        --
        -- (Report.writeToFile reportFile . genReport . applyAssign match ts)  filters
        -- putStrLn "Report Generated"
    else
        putStrLn $ "A file in " ++ show requiredFiles
