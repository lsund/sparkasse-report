{-# LANGUAGE OverloadedStrings #-}
module Filter where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Report
import Util

import Transaction
  ( CategorizedTransaction(..)
  , CategorizedTransaction
  , Category
  , Transaction
  , _ocr
  , _tAmount
  , _tag
  , _transactor
  )

data Filter =
  Filter
    { _content :: Text
    , _fCategory :: Text
    , _selector :: Transaction -> Text
    }

instance FromJSON Filter where
  parseJSON (Object o) =
    o .: "content" >>=
    (\cont ->
       o .: "category" >>=
       (\cat -> o .: "selector" >>= (return . Filter cont cat . decodeSelector)))
  parseJSON _ = mzero

decodeSelector :: Text -> (Transaction -> Text)
decodeSelector "ocr" = _ocr
decodeSelector "tag" = _tag
decodeSelector _ = _transactor

type Predicate = (Filter -> Transaction -> Bool)

match :: Predicate
match (Filter cont _ selector) t = cont `T.isInfixOf` selector t

nomatch :: Predicate
nomatch flt = not . match flt

-- The list of transactions that satisfies all filters in the list
applyAnd :: Predicate -> [Transaction] -> [Filter] -> [Transaction]
applyAnd p ts flts = filter (\t -> all (`p` t) flts) ts

-- Apply a filter to a list of transactions
-- Return the list of transactions that satisfies the predicate
applyPred :: Predicate -> [Transaction] -> Filter -> [Transaction]
applyPred p ts flt = filter (p flt) ts

-- applyPred but with a list of filters
apply :: Predicate -> [Transaction] -> [Filter] -> [[Transaction]]
apply p ts = map (applyPred p ts)

categorizeTransactions :: ([Transaction], Category) -> CategorizedTransaction
categorizeTransactions (ts, c) =
  CategorizedTransaction c ((sumMaybes . map _tAmount) ts)

-- Like apply but associates each transactionlist with the corresponding category
applyAssign ::
     Predicate -> [Transaction] -> [Filter] -> [CategorizedTransaction]
applyAssign p ts =
  map
    (categorizeTransactions .
     (\flt@(Filter _ cat _) -> (applyPred p ts flt, cat)))

assignedAndUnmatched :: [Transaction] -> [Filter] -> Report
assignedAndUnmatched ts flts =
  Report (applyAssign match ts flts) (applyAnd nomatch ts flts)
