module Filter where

import Data.List (find, isInfixOf)
import Data.Maybe (isJust)

import Report
import Transaction

data Filter =
  Filter
    { _content :: String
    , _selector :: Transaction -> String
    , _dest :: Category
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
