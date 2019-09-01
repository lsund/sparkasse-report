{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List (find, groupBy, sortBy)
import Data.List.Split (splitOn)

import Transaction
import Filter

--------------------------------------------------------------------------------
-- Dummy
dummy :: [CategoryRow]
dummy =
  [ CategoryRow Nightlife (Just 10)
  , CategoryRow FastFood (Just 5)
  , CategoryRow Nightlife (Just 3)
  , CategoryRow Unknown Nothing
  ]

csvFile :: FilePath
csvFile = "data/20190818-123093569-umsatz.CSV"

reportFile :: FilePath
reportFile = "data/report.txt"

--------------------------------------------------------------------------------
-- Types
data CategoryRow =
  CategoryRow
    { _category :: Category
    , _value :: Maybe Double
    }

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

userQuery :: Transaction -> IO (Maybe CategoryRow)
userQuery t = do
  mapM_ print charToCategory
  putStrLn "q: quit"
  print t
  (x:_) <- getLine
  if x == 'q'
    then return Nothing
    else do
      let cat = maybe Unknown snd (find ((== x) . fst) charToCategory)
      return $ Just (CategoryRow cat (_amount t))

readTransactions :: IO [Transaction]
readTransactions = transactions <$> readFile csvFile

interactiveGroup :: IO [Maybe CategoryRow]
interactiveGroup = readTransactions >>= mapM userQuery . tail

foldMaybe :: (Num a) => [Maybe a] -> Maybe a
foldMaybe = foldr (liftA2 (+)) (Just 0)

categorySums :: [CategoryRow] -> [CategoryRow]
categorySums xs =
  map (\ys -> CategoryRow ((_category . head) ys) ((foldMaybe . map _value) ys)) xss
  where
    xss = groupByCategory xs

categorySumToString :: [CategoryRow] -> String
categorySumToString =
  foldr (\(CategoryRow x y) acc -> show x ++ ": " ++ show y ++ "\n" ++ acc) ""

groupByCategory :: [CategoryRow] -> [[CategoryRow]]
groupByCategory = groupBy (on (==) _category) . sortBy (on compare _category)

genReport :: [CategoryRow] -> IO ()
genReport = writeFile reportFile . categorySumToString . categorySums

filters :: [Filter]
filters = [Filter "Lohn" Unknown, Filter "Kesting" Apartment]

filtered :: [Transaction] -> [[Transaction]]
filtered xs = map (\flt -> matches flt xs) filters

matches :: Filter ->[Transaction] -> [Transaction]
matches flt = filter (Filter.any flt)
