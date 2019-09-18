module Report where

import Transaction
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

type Report = [ReportRow]

categorySums :: [(Category, Maybe Double)] -> Report
categorySums xs =
  map
    (\ys -> ReportRow ((fst . head) ys) ((sumMaybes . map snd) ys))
    xss
  where
    xss = groupByCategory xs

groupByCategory :: [(Category, Maybe Double)] -> [[(Category, Maybe Double)]]
groupByCategory = groupBy (on (==) fst) . sortBy (on compare fst)

genReportInteractive :: FilePath -> [(Category, Maybe Double)]  -> IO ()
genReportInteractive fp = writeFile fp . toString . categorySums

genReport :: [([Transaction], Category)] -> Report
genReport = map (\(ts, c) -> ReportRow c ((sumMaybes . map _amount) ts))

toString :: Report -> String
toString =
  foldr (\(ReportRow x y) acc -> show x ++ ": " ++ show y ++ "\n" ++ acc) ""

writeToFile :: FilePath -> Report -> IO ()
writeToFile fp = writeFile fp . toString
