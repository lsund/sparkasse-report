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
filterFile = "data/filters.json"

--------------------------------------------------------------------------------
-- Program
main :: IO ()
main = do
  let requiredFiles = [csvFile, filterFile]
  inputFilesExist <- allM doesFileExist requiredFiles
  if inputFilesExist
    then do
      ts <- readTransactions csvFile
      jsonStr <- BS.readFile filterFile
      let mfs = decode jsonStr :: Maybe [Filter]
      case mfs of
        Just fs -> do
          let reportJSON =
                (encode . fromFilterResult . assignedAndUnmatched ts) fs
          BS.writeFile reportFile reportJSON
          putStrLn $ "Success: wrote '"  ++ reportFile ++ "'"
        Nothing -> putStrLn $ "Error: Could not parse '" ++ filterFile ++ "'"
    else putStrLn $ "A file in " ++ show requiredFiles
