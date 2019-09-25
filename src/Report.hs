{-# LANGUAGE OverloadedStrings #-}

module Report where

import Data.Aeson

import Transaction
  ( CategorizedTransaction
  , CategorizedTransaction(..)
  , Transaction
  , _tAmount
  )

data Report =
  Report
    { _categorizedTransactions :: [CategorizedTransaction]
    , _uncategorizedTransactions :: [Transaction]
    }

instance ToJSON Report where
  toJSON (Report cs ut) =
    object ["categorizedTransactions" .= cs, "uncategorizedTransactions" .= ut]
