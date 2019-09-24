module Main where

import Control.Monad.Extra (allM)
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Filter
import Lib
import Report
import System.Directory (doesFileExist)
import Transaction

--------------------------------------------------------------------------------
-- Dummy
dummy :: [CategorySum]
dummy =
  [ CategorySum "Nightlife" (Just 10)
  , CategorySum "FastFood" (Just 5)
  , CategorySum "Nightlife" (Just 3)
  , CategorySum "Unknown" Nothing
  ]

dummyFilters :: [Filter]
dummyFilters =
  [ Filter "Helena" _details "Unknown"
  , Filter "Lohn" _ocr "Income"
  , Filter "Kesting" _details "Apartment"
  ]

dummyTransactions :: [Transaction]
dummyTransactions =
  [ Transaction "foo" "bar" (Just (-500)) "baz"
  , Transaction "foo" "bar" (Just 2800) "baz"
  ]

csvFile :: FilePath
csvFile = "data/20190818-123093569-umsatz.CSV"

reportFile :: FilePath
reportFile = "data/report.json"

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
      let reportJSON =
            (encode . fromFilterResult . assignedAndUnmatched ts) filters
      BS.writeFile reportFile reportJSON
    else putStrLn $ "A file in " ++ show requiredFiles
