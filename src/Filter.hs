module Filter where

import Data.List.Split (splitOn)
import Data.List (find, isInfixOf)
import Data.Maybe (isJust, catMaybes)

import Report
import Transaction

data Filter =
  Filter
    { _content :: String
    , _selector :: Transaction -> String
    , _dest :: String
    }

match :: Filter -> Transaction -> Bool
match (Filter cont selector _) t = cont `isInfixOf` selector t

applyMatchMany :: [Transaction] -> [Filter] -> [[Transaction]]
applyMatchMany ts = map (ts `applyMatch`)

-- Apply a filter to a list of transactions
-- Return the list of transactions that matches the filter
applyMatch :: [Transaction] -> Filter -> [Transaction]
applyMatch ts flt = filter (match flt) ts

applyAssignMany :: [Transaction] -> [Filter] -> [([Transaction], Category)]
applyAssignMany ts = map (\flt -> (applyMatch ts flt, _dest flt))

applyAssign :: [Transaction] -> Filter -> ([Transaction], Category)
applyAssign ts flt = (applyMatch ts flt, _dest flt)

deserializeLine :: String -> Maybe Filter
deserializeLine s =
  case splitOn "," s of
    [cont, "ocr", cat] -> Just $ Filter cont _ocr cat
    [cont, "details", cat] -> Just $ Filter cont _details cat
    [cont, "tag", cat] -> Just $ Filter cont _tag cat
    _ -> Nothing

deserialize :: FilePath -> IO [Filter]
deserialize = fmap (catMaybes . map deserializeLine . lines) . readFile
