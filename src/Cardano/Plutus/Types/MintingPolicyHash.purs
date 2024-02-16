module Cardano.Plutus.Types.MintingPolicyHash
  ( MintingPolicyHash(MintingPolicyHash)
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getField
  )
import Cardano.FromData (class FromData)
import Cardano.FromMetadata (class FromMetadata)
import Cardano.ToData (class ToData)
import Cardano.ToMetadata (class ToMetadata)
import Cardano.Types.ScriptHash (ScriptHash(ScriptHash))
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- | From `Plutus.V1.Ledger.Api`
newtype MintingPolicyHash = MintingPolicyHash ScriptHash

derive instance Generic MintingPolicyHash _
derive instance Newtype MintingPolicyHash _
derive newtype instance Eq MintingPolicyHash
derive newtype instance Ord MintingPolicyHash
derive newtype instance FromData MintingPolicyHash
derive newtype instance ToData MintingPolicyHash
derive newtype instance FromMetadata MintingPolicyHash
derive newtype instance ToMetadata MintingPolicyHash

instance DecodeAeson MintingPolicyHash where
  decodeAeson = decodeAesonHelper "getMintingPolicyHash" MintingPolicyHash

instance EncodeAeson MintingPolicyHash where
  encodeAeson (MintingPolicyHash hash) =
    encodeAeson { "getMintingPolicyHash": hash }

instance Show MintingPolicyHash where
  show = genericShow

decodeAesonHelper
  :: ∀ (a :: Type) (b :: Type)
   . DecodeAeson a
  => String
  -> (a -> b)
  -> Aeson
  -> Either JsonDecodeError b
decodeAesonHelper constrName constr = caseAesonObject
  (Left $ TypeMismatch "Expected object")
  (flip getField constrName >=> decodeAeson >>> map constr)
