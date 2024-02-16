module Cardano.Plutus.Types.PaymentPubKeyHash where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson, (.:))
import Cardano.FromData (class FromData)
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash(PubKeyHash))
import Cardano.ToData (class ToData)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- | https://github.com/IntersectMBO/plutus-apps/blob/dbafa0ffdc1babcf8e9143ca5a7adde78d021a9a/plutus-ledger/src/Ledger/Address.hs#L121
newtype PaymentPubKeyHash = PaymentPubKeyHash PubKeyHash

derive instance Generic PaymentPubKeyHash _
derive instance Newtype PaymentPubKeyHash _
derive newtype instance Eq PaymentPubKeyHash
derive newtype instance FromData PaymentPubKeyHash
derive newtype instance Ord PaymentPubKeyHash
derive newtype instance ToData PaymentPubKeyHash

instance EncodeAeson PaymentPubKeyHash where
  encodeAeson (PaymentPubKeyHash pkh) = encodeAeson
    { "unPaymentPubKeyHash": pkh }

instance DecodeAeson PaymentPubKeyHash where
  decodeAeson json = do
    obj <- decodeAeson json
    PaymentPubKeyHash <<< PubKeyHash <$> obj .: "unPaymentPubKeyHash"

instance Show PaymentPubKeyHash where
  show = genericShow
