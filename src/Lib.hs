module Lib where

import Data.List (find)

import Transaction
import Report

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

readTransactions :: FilePath -> IO [Transaction]
readTransactions = fmap transactions . readFile

interactiveGroup :: FilePath -> IO [Maybe ReportRow]
interactiveGroup fp = readTransactions fp >>= mapM userQuery . tail
