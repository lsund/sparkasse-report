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

type Predicate = (Filter -> Transaction -> Bool)

match :: Predicate
match (Filter cont selector _) t = cont `isInfixOf` selector t

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

-- Like apply but associates each transactionlist with the corresponding category
applyAssign ::
     Predicate -> [Transaction] -> [Filter] -> [([Transaction], Category)]
applyAssign p ts = map (\flt -> (applyPred p ts flt, _dest flt))

assignedAndUnmatched ::
     [Transaction] -> [Filter] -> ([([Transaction], Category)], [Transaction])
assignedAndUnmatched ts flts =
  (applyAssign match ts flts, applyAnd nomatch ts flts)

deserialize :: FilePath -> IO [Filter]
deserialize = fmap (mapMaybe deserializeLine . lines) . readFile
  where
    deserializeLine :: String -> Maybe Filter
    deserializeLine s =
      case splitOn "," s of
        [cont, "ocr", cat] -> Just $ Filter cont _ocr cat
        [cont, "details", cat] -> Just $ Filter cont _details cat
        [cont, "tag", cat] -> Just $ Filter cont _tag cat
        _ -> Nothing
