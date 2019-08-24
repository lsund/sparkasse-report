{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad
import Data.Function
import Data.List (find, groupBy, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe

data Category
  = GroceryFarmacy
  | EverydayLife
  | Restaurant
  | Nightlife
  | FastFood
  | Technology
  | Apartment
  | SharedAccount
  | Vacation
  | Auto
  | PublicTransport
  | Subscriptions
  | Cash
  | Sport
  | Clothes
  | Hairdresser
  | Unknown
  deriving (Show, Eq, Ord)

csvFile :: FilePath
csvFile =
  "/home/lsund/Documents/personal/economy/helena/bank-summary/apr/20190818-123093569-umsatz.CSV"

data Transaction =
  Transaction
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

categoryMapping :: [(Char, Category)]
categoryMapping =
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

userQuery :: Transaction -> IO (Category, Maybe Double)
userQuery t = do
  mapM_ print categoryMapping
  print t
  (x:_) <- getLine
  let cat = fromMaybe Unknown $ snd <$> find ((== x) . fst) categoryMapping
  return (cat, _amount t)

interactiveGroup :: IO [(Category, Maybe Double)]
interactiveGroup = readFile csvFile >>= mapM userQuery . tail . transactions

dummy :: [(Category, Maybe Double)]
dummy =
  [ (Nightlife, Just 10)
  , (FastFood, Just 5)
  , (Nightlife, Just 3)
  , (Unknown, Nothing)
  ]

categorySums :: [(Category, Maybe Double)] -> [(Category, Double)]
categorySums xs =
  zip (map (fst . head) xss) (map (sum . catMaybes . map snd) xss)
  where
    xss = groupCategory xs

categorySumToString :: [(Category, Double)] -> String
categorySumToString =
  foldr (\(x, y) acc -> show x ++ "," ++ show y ++ "\n" ++ acc) ""

groupCategory :: [(Category, Maybe Double)] -> [[(Category, Maybe Double)]]
groupCategory = groupBy (on (==) fst) . sortBy (on compare fst)

genReport :: [(Category, Maybe Double)] -> IO ()
genReport = writeFile "test.txt" . categorySumToString . categorySums
