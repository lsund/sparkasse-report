{-# LANGUAGE OverloadedStrings #-}

module Filter where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Report (Category)
import Transaction

data Filter =
  Filter
    { _content :: Text
    , _category :: Text
    , _selector :: Transaction -> Text
    }

instance FromJSON Filter where
  parseJSON (Object o) =
    o .: "content" >>=
    (\cont ->
       o .: "category" >>=
       (\cat -> o .: "selector" >>=
        (return . Filter cont cat . decodeSelector)))
  parseJSON _ = mzero

decodeSelector :: Text -> (Transaction -> Text)
decodeSelector "ocr" = _ocr
decodeSelector "tag" = _tag
decodeSelector _ = _transactor

type Predicate = (Filter -> Transaction -> Bool)

match :: Predicate
match (Filter cont _ selector) t = cont `T.isInfixOf` selector t

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
applyAssign p ts = map (\flt -> (applyPred p ts flt, _category flt))

assignedAndUnmatched ::
     [Transaction] -> [Filter] -> ([([Transaction], Category)], [Transaction])
assignedAndUnmatched ts flts =
  (applyAssign match ts flts, applyAnd nomatch ts flts)
