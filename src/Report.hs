module Report where

import Data.Function
import Data.List (groupBy, sortBy)
import Transaction
import Util

type Category = String

data ReportRow =
  ReportRow
    { _category :: Category
    , _value :: Maybe Double
    }

type Report = [ReportRow]

toCsv :: Report -> String
toCsv =
  foldr
    (\(ReportRow cat val) acc -> cat <> "," <> showMaybe val <> "\n" <> acc)
    ""

categorySums :: [(Category, Maybe Double)] -> Report
categorySums xs =
  map (\ys -> ReportRow ((fst . head) ys) ((sumMaybes . map snd) ys)) xss
  where
    xss = groupByCategory xs

groupByCategory :: [(Category, Maybe Double)] -> [[(Category, Maybe Double)]]
groupByCategory = groupBy (on (==) fst) . sortBy (on compare fst)

genReportInteractive :: FilePath -> [(Category, Maybe Double)] -> IO ()
genReportInteractive fp = writeFile fp . toCsv . categorySums

genReport :: [([Transaction], Category)] -> Report
genReport = map (\(ts, c) -> ReportRow c ((sumMaybes . map _amount) ts))

writeToFile :: FilePath -> Report -> IO ()
writeToFile fp = writeFile fp . toCsv
