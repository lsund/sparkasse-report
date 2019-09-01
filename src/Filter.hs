module Filter where

import qualified Data.List as L

import Transaction

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
  | Unknown
  deriving (Show, Eq, Ord)

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

anyp2 :: [a -> b -> Bool] -> a -> b -> Bool
anyp2 fs x t = or $ map (\f -> f x t) fs

any :: Filter -> Transaction -> Bool
any = anyp2 [ocr, details, ttype]
