module Cardano.Plutus.Types.TransactionOutputWithRefScript where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Data.Lens (Lens')
import Cardano.FromData (class FromData, fromData)
import Cardano.Plutus.Types.TransactionOutput (TransactionOutput)
import Cardano.ToData (class ToData, toData)
import Cardano.Types.ScriptRef (ScriptRef)
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(Proxy))

newtype TransactionOutputWithRefScript = TransactionOutputWithRefScript
  { output :: TransactionOutput
  , scriptRef :: Maybe ScriptRef
  }

_output :: Lens' TransactionOutputWithRefScript TransactionOutput
_output = _Newtype <<< prop (Proxy :: Proxy "output")

_scriptRef :: Lens' TransactionOutputWithRefScript (Maybe ScriptRef)
_scriptRef = _Newtype <<< prop (Proxy :: Proxy "scriptRef")

derive instance Generic TransactionOutputWithRefScript _
derive instance Newtype TransactionOutputWithRefScript _
derive newtype instance Eq TransactionOutputWithRefScript

instance Show TransactionOutputWithRefScript where
  show = genericShow

derive newtype instance DecodeAeson TransactionOutputWithRefScript
derive newtype instance EncodeAeson TransactionOutputWithRefScript

instance FromData TransactionOutputWithRefScript where
  fromData = map (wrap <<< { output: _, scriptRef: Nothing }) <<< fromData

instance ToData TransactionOutputWithRefScript where
  toData = toData <<< _.output <<< unwrap
