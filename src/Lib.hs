{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad
import Data.List (find)
import Data.List.Split (splitOn)

import Filter
import Transaction
import Report

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
dummyFilters = [Filter "Lohn" Unknown, Filter "Kesting" Apartment]

csvFile :: FilePath
csvFile = "data/20190818-123093569-umsatz.CSV"

reportFile :: FilePath
reportFile = "data/report.txt"

--------------------------------------------------------------------------------
-- Types
charToCategory :: [(Char, Category)]
charToCategory =
  [ ('m', GroceryFarmacy)
  , ('e', EverydayLife)
  , ('r', Restaurant)
  , ('n', Nightlife)
  , ('f', FastFood)
  , ('t', Technology)
  , ('a', Apartment)
  , ('v', Vacation)
  , ('o', Auto)
  , ('s', Subscriptions)
  , ('x', Sport)
  , ('c', Cash)
  , ('k', Clothes)
  , ('h', Hairdresser)
  , ('u', Unknown)
  ]

--------------------------------------------------------------------------------
-- Functions
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

fromString :: String -> Transaction
fromString s =
  Transaction
    (readToken split 3)
    (readToken split 4)
    (readAmount split)
    (readToken split 11)
  where
    readToken ss = peel . (!!) ss
    split = splitOn ";" s

transactions :: String -> [Transaction]
transactions = map fromString . lines

balance :: [Transaction] -> Maybe Double
balance = msum . map _amount

userQuery :: Transaction -> IO (Maybe ReportRow)
userQuery t = do
  mapM_ print charToCategory
  putStrLn "q: quit"
  print t
  (x:_) <- getLine
  if x == 'q'
    then return Nothing
    else do
      let cat = maybe Unknown snd (find ((== x) . fst) charToCategory)
      return $ Just (ReportRow cat (_amount t))

readTransactions :: IO [Transaction]
readTransactions = transactions <$> readFile csvFile

interactiveGroup :: IO [Maybe ReportRow]
interactiveGroup = readTransactions >>= mapM userQuery . tail
