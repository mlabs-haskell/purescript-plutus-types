module Cardano.Plutus.Types.TokenName (TokenName, adaToken) where

import Prelude
import Data.ByteArray (ByteArray)

adaToken :: ByteArray
adaToken = mempty

type TokenName = ByteArray
