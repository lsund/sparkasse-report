module Report where

import Data.Aeson
import Data.Function
import Data.List (groupBy, sortBy)

import Transaction
import Util

type Category = String

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
        ["foo" .= cs, "bar" .= ut]

categorySums :: [(Category, Maybe Double)] -> [CategorySum]
categorySums xs =
  map (\ys -> CategorySum ((fst . head) ys) ((sumMaybes . map snd) ys)) xss
  where
    xss = groupByCategory xs

groupByCategory :: [(Category, Maybe Double)] -> [[(Category, Maybe Double)]]
groupByCategory = groupBy (on (==) fst) . sortBy (on compare fst)

-- genReportInteractive :: FilePath -> [(Category, Maybe Double)] -> IO ()
-- genReportInteractive fp = writeFile fp . toCsv . categorySums
-- TODO generate json here instead of report

fromFilterResult :: ([([Transaction], Category)], [Transaction]) -> Report
fromFilterResult (at, ut) =
  Report (map (\(ts, c) -> CategorySum c ((sumMaybes . map _amount) ts)) at) ut

-- writeToFile :: FilePath -> Report -> IO ()
-- writeToFile fp = writeFile fp . toCsv

-- serialize :: Report -> ByteString
serialize r = encode r
