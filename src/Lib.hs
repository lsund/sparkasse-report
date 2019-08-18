{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.List.Split (splitOn)

csvFile :: FilePath
csvFile =
  "/home/lsund/Documents/personal/economy/helena/bank-summary/apr/20190818-123093569-umsatz.CSV"

data TransactionInfo =
  TransactionInfo
    { _type :: String
    , _ocr :: String
    , _amount :: Maybe Double
    , _details :: String
    }
  deriving (Show)

-- Removes the first and last character of a string
peel :: String -> String
peel = tail . init

commaToPeriod :: Char -> Char
commaToPeriod ',' = '.'
commaToPeriod x = x

readMaybe :: Read a => String -> Maybe a
readMaybe s =
  case reads s of
    [(val, "")] -> Just val
    _ -> Nothing

readAmount :: [String] -> Maybe Double
readAmount xs = (readMaybe . map commaToPeriod . peel . (!!) xs) 14

readToken :: [String] -> Int -> String
readToken ss = peel . (!!) ss

fromString :: String -> TransactionInfo
fromString s =
  TransactionInfo
    (readToken split 3)
    (readToken split 4)
    (readAmount split)
    (readToken split 11)
  where
    split = splitOn ";" s

someFunc :: IO ()
someFunc = readFile csvFile >>= mapM_ (print . fromString) . lines
