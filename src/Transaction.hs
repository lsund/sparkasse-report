{-# Language OverloadedStrings #-}
module Transaction where

import Data.List.Split (splitOn)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

data Transaction =
  Transaction
    { _tag :: Text
    , _ocr :: Text
    , _amount :: Maybe Double
    , _transactor :: Text
    }
  deriving (Show)

instance ToJSON Transaction where
  toJSON (Transaction t o a d) =
    object ["tag" .= t, "ocr" .= o, "amount" .= a, "transactor" .= d]

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
     readToken ss = T.pack . peel . (!!) ss
     split = splitOn ";" s

transactionsFromString :: String -> [Transaction]
transactionsFromString = map fromString . lines
