module Cardano.Plutus.Types.StakePubKeyHash where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson, (.:))
import Cardano.FromData (class FromData)
import Cardano.FromMetadata (class FromMetadata)
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash(PubKeyHash))
import Cardano.ToData (class ToData)
import Cardano.ToMetadata (class ToMetadata)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- | https://github.com/IntersectMBO/plutus-apps/blob/dbafa0ffdc1babcf8e9143ca5a7adde78d021a9a/plutus-ledger/src/Ledger/Address.hs#L141
newtype StakePubKeyHash = StakePubKeyHash PubKeyHash

derive instance Generic StakePubKeyHash _
derive instance Newtype StakePubKeyHash _
derive newtype instance Eq StakePubKeyHash
derive newtype instance FromData StakePubKeyHash
derive newtype instance FromMetadata StakePubKeyHash
derive newtype instance Ord StakePubKeyHash
derive newtype instance ToData StakePubKeyHash
derive newtype instance ToMetadata StakePubKeyHash

instance Show StakePubKeyHash where
  show = genericShow

instance EncodeAeson StakePubKeyHash where
  encodeAeson (StakePubKeyHash pkh) = encodeAeson
    { "unStakePubKeyHash": pkh }

instance DecodeAeson StakePubKeyHash where
  decodeAeson json = do
    obj <- decodeAeson json
    StakePubKeyHash <<< PubKeyHash <$> obj .: "unStakePubKeyHash"
