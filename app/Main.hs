module Main where

import Util
import Filter
import Lib
import Report
import Transaction

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
reportFile = "data/report.txt"

--------------------------------------------------------------------------------
-- Program
main :: IO ()
main = do
  ts <- readTransactions csvFile
  filters <- Filter.deserialize "data/filters.csv"
  (Report.writeToFile reportFile . genReport . applyAssignMany ts)  filters
  putStrLn "Report Generated"
