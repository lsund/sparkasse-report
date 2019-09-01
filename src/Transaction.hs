module Transaction where

data Transaction =
  Transaction
    { _ttype :: String
    , _ocr :: String
    , _amount :: Maybe Double
    , _details :: String
    }
  deriving (Show)
