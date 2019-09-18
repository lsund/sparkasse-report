module Main where

import Filter
import Lib
import Report
import Transaction

--------------------------------------------------------------------------------
-- Dummy
dummy :: [ReportRow]
dummy =
  [ ReportRow Nightlife (Just 10)
  , ReportRow FastFood (Just 5)
  , ReportRow Nightlife (Just 3)
  , ReportRow Unknown Nothing
  ]

dummyFilters :: [Filter]
dummyFilters =
  [Filter "Lohn" _ocr Income, Filter "Kesting" _details Apartment]

csvFile :: FilePath
csvFile = "data/20190818-123093569-umsatz.CSV"

reportFile :: FilePath
reportFile = "data/report.txt"

--------------------------------------------------------------------------------
-- Program
main :: IO ()
main = do
  ts <- readTransactions csvFile
  print $ ts `applyAssignMany` dummyFilters
