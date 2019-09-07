module Filter where

import qualified Data.List as L

import Transaction
import Util
import Report

data Filter = Filter { _from :: String
                     , _to :: Category }

run :: (Transaction -> String) -> Filter -> Transaction -> Bool
run f x t = _from x `L.isInfixOf` f t

ocr :: Filter -> Transaction -> Bool
ocr = run _ocr

details :: Filter -> Transaction -> Bool
details = run _details

ttype :: Filter -> Transaction -> Bool
ttype = run _ttype

any :: Filter -> Transaction -> Bool
any flt t = or $ flist [ocr, details, ttype] flt t

--------------------------------------------------------------------------------
-- Filter Transaction

filtered :: [Filter] -> [Transaction] -> [[Transaction]]
filtered flts xs = map (`matches` xs) flts

matches :: Filter -> [Transaction] -> [Transaction]
matches flt = filter (Filter.any flt)
