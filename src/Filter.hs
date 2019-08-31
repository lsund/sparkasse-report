module Filter where

import qualified Data.List as L

import Transaction

data Filter = Filter { _contains :: String }

dummy = Transaction "Lohn"

run :: Transaction -> Filter -> Bool
run t f = _contains f `L.isInfixOf` _ocr t
