module Main where

import Control.Monad.Extra (allM)
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Filter
import Options.Applicative
import System.Directory (doesFileExist)
import Transaction

readTransactions :: FilePath -> IO [Transaction]
readTransactions = fmap transactionsFromString . readFile

data OptSpec =
  OptSpec
    { _input :: String
    , _filterSpec :: String
    , _output :: Maybe String
    }

parseOptSpec :: Parser OptSpec
parseOptSpec =
  OptSpec <$>
  argument str
    (metavar "INPUT" <>
     help "Sparkasse Input (CSV))") <*>
  argument str
    (metavar "FILTERS" <>
     help "Filter Spec (JSON)") <*>
  optional
    (strOption
       (long "output" <> short 'o' <> metavar "FILE" <>
        help
          ("Output file (exported sparkasse csv). " ++
           "If omitted, print to stdout")))

run :: OptSpec -> IO ()
run (OptSpec input filterSpec output) = do
  let requiredFiles = [input, filterSpec]
  inputFilesExist <- allM doesFileExist requiredFiles
  if inputFilesExist
    then do
      ts <- readTransactions input
      jsonStr <- BS.readFile filterSpec
      let mfs = decode jsonStr :: Maybe [Filter]
      case mfs of
        Just fs -> do
          let reportJSON =
                (encode . assignedAndUnmatched ts) fs
          case output of
            Just outputFile -> do
              BS.writeFile outputFile reportJSON
              putStrLn $ "Success: wrote '" ++ outputFile ++ "'"
            Nothing -> BS.putStr reportJSON
        Nothing -> putStrLn $ "Error: Could not parse '" ++ filterSpec ++ "'"
    else putStrLn $ "A file in " ++ show requiredFiles ++ " was not found."

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (parseOptSpec <**> helper)
        (fullDesc <>
         progDesc "Convert a Sparkasse CSV file into something usable" <>
         header "sparkasse-report")
