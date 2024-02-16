module Cardano.Plutus.Types.PubKeyHash
  ( PubKeyHash(PubKeyHash)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , encodeAeson
  , (.:)
  )
import Cardano.FromData (class FromData)
import Cardano.FromMetadata (class FromMetadata)
import Cardano.ToData (class ToData)
import Cardano.ToMetadata (class ToMetadata)
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

-- | https://github.com/IntersectMBO/plutus/blob/e159ddc2f89e24e20318ed7b46b8591f6759aac4/plutus-ledger-api/src/PlutusLedgerApi/V1/Crypto.hs#L29
newtype PubKeyHash = PubKeyHash Ed25519KeyHash

derive instance Generic PubKeyHash _
derive instance Newtype PubKeyHash _
derive newtype instance Eq PubKeyHash
derive newtype instance FromData PubKeyHash
derive newtype instance FromMetadata PubKeyHash
derive newtype instance Ord PubKeyHash
derive newtype instance ToData PubKeyHash
derive newtype instance ToMetadata PubKeyHash

instance Show PubKeyHash where
  show = genericShow

instance EncodeAeson PubKeyHash where
  encodeAeson x = encodeAeson { getPubKeyHash: unwrap x }

instance DecodeAeson PubKeyHash where
  decodeAeson a = do
    obj <- decodeAeson a
    wrap <$> obj .: "getPubKeyHash"
