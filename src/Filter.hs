module Filter where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

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

applyNoMatch :: [Transaction] -> Filter -> [Transaction]
applyNoMatch = applyPred (\x y -> not $ match x y)

-- Apply a filter to a list of transactions
-- Return the list of transactions that matches the filter
applyPred ::
     (Filter -> Transaction -> Bool) -> [Transaction] -> Filter -> [Transaction]
applyPred p ts flt = filter (p flt) ts

apply ::
     (Filter -> Transaction -> Bool)
  -> [Transaction]
  -> [Filter]
  -> [[Transaction]]
apply p ts = map (applyPred p ts)

applyAssign ::
     (Filter -> Transaction -> Bool)
  -> [Transaction]
  -> [Filter]
  -> [([Transaction], Category)]
applyAssign p ts = map (\flt -> (applyPred p ts flt, _dest flt))

deserializeLine :: String -> Maybe Filter
deserializeLine s =
  case splitOn "," s of
    [cont, "ocr", cat] -> Just $ Filter cont _ocr cat
    [cont, "details", cat] -> Just $ Filter cont _details cat
    [cont, "tag", cat] -> Just $ Filter cont _tag cat
    _ -> Nothing

deserialize :: FilePath -> IO [Filter]
deserialize = fmap (mapMaybe deserializeLine . lines) . readFile
