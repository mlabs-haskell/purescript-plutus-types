module Cardano.Plutus.Types.UtxoMap where

import Cardano.Plutus.Types.TransactionOutputWithRefScript
  ( TransactionOutputWithRefScript
  )
import Cardano.Types.TransactionInput (TransactionInput)
import Data.Map (Map)

type UtxoMap = Map TransactionInput TransactionOutputWithRefScript
