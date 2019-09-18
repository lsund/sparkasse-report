module Report where

import Util
import Data.List (groupBy, sortBy)
import Data.Function

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
  | Income
  | Unknown
  deriving (Show, Eq, Ord)

data ReportRow =
  ReportRow
    { _category :: Category
    , _value :: Maybe Double
    }

categorySums :: [ReportRow] -> [ReportRow]
categorySums xs =
  map
    (\ys -> ReportRow ((_category . head) ys) ((foldMaybe . map _value) ys))
    xss
  where
    xss = groupByCategory xs

categorySumToString :: [ReportRow] -> String
categorySumToString =
  foldr (\(ReportRow x y) acc -> show x ++ ": " ++ show y ++ "\n" ++ acc) ""

groupByCategory :: [ReportRow] -> [[ReportRow]]
groupByCategory = groupBy (on (==) _category) . sortBy (on compare _category)

genReport :: FilePath -> [ReportRow] -> IO ()
genReport fp = writeFile fp . categorySumToString . categorySums
