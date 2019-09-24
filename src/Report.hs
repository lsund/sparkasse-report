{-# Language OverloadedStrings #-}
module Report where

import Data.Aeson
import Data.Function
import Data.List (groupBy, sortBy)
import Data.Text (Text)

import Transaction
import Util

type Category = Text

data CategorySum =
  CategorySum
    { _category :: Category
    , _value :: Maybe Double
    }

instance ToJSON CategorySum where
  toJSON (CategorySum c v) =
    object [ "category" .= c, "value" .= v ]

data Report =
  Report
    { _categorySums :: [CategorySum]
    , _unassignedTransactions :: [Transaction]
    }

instance ToJSON Report where
  toJSON (Report cs ut) =
    object
        ["categorySums" .= cs, "unassignedTransactions" .= ut]

categorySums :: [(Category, Maybe Double)] -> [CategorySum]
categorySums xs =
  map (\ys -> CategorySum ((fst . head) ys) ((sumMaybes . map snd) ys)) xss
  where
    xss = groupByCategory xs

groupByCategory :: [(Category, Maybe Double)] -> [[(Category, Maybe Double)]]
groupByCategory = groupBy (on (==) fst) . sortBy (on compare fst)

fromFilterResult :: ([([Transaction], Category)], [Transaction]) -> Report
fromFilterResult (at, ut) =
  Report (map (\(ts, c) -> CategorySum c ((sumMaybes . map _amount) ts)) at) ut
