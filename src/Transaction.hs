module Transaction where

import Control.Monad
import Data.List.Split (splitOn)

data Transaction =
  Transaction
    { _ttype :: String
    , _ocr :: String
    , _amount :: Maybe Double
    , _details :: String
    }
  deriving (Show)

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

balance :: [Transaction] -> Maybe Double
balance = msum . map _amount

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
