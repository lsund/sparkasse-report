module Filter where

import Data.List (find, isInfixOf)
import Data.Maybe (isJust)

import Report
import Transaction

data Filter =
  Filter
    { _match :: String
    , _selector :: Transaction -> String
    , _dest :: Category
    }

run :: Filter -> Transaction -> Bool
run (Filter match selector _) t = match `isInfixOf` selector t

-- run2 :: Filter -> [Transaction] -> Maybe (Transaction, Category)
-- run2 flt t =
--   if run  flt t
--     then Just (t, _dest flt)
--     else Nothing

--------------------------------------------------------------------------------
-- Filter Transaction
matchess :: [Filter] -> [Transaction] -> [[Transaction]]
matchess flts xs = map (`matches` xs) flts

matches :: Filter -> [Transaction] -> [Transaction]
matches flt = filter (run flt)

-- stove :: Filter -> [Transaction] -> Maybe Transaction
-- stove flt ts = find (\t -> isJust (run2 flt t)) ts
