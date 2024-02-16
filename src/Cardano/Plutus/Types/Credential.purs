module Cardano.Plutus.Types.Credential where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(AtKey, Named, UnexpectedValue)
  , decodeAeson
  , (.:)
  )
import Cardano.FromData (class FromData, genericFromData)
import Cardano.Plutus.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PNil
  , S
  , Z
  )
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash)
import Cardano.Plutus.Types.ValidatorHash (ValidatorHash)
import Cardano.ToData (class ToData, genericToData)
import Cardano.Types.Internal.Helpers (encodeTagged')
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import JS.BigInt (BigInt)

--------------------------------------------------------------------------------
-- Credential
--------------------------------------------------------------------------------

-- | Credential required to unlock a transaction output.
-- |
-- | https://github.com/IntersectMBO/plutus/blob/eceae8831b8186655535dee587486dbd3fd037f4/plutus-ledger-api/src/PlutusLedgerApi/V1/Credential.hs#L57
data Credential
  = PubKeyCredential PubKeyHash
  | ScriptCredential ValidatorHash

derive instance Eq Credential
derive instance Ord Credential
derive instance Generic Credential _

instance Show Credential where
  show = genericShow

instance
  HasPlutusSchema
    Credential
    ( "PubKeyCredential" := PNil @@ Z
        :+ "ScriptCredential"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance EncodeAeson Credential where
  encodeAeson = case _ of
    PubKeyCredential a -> encodeTagged' "PubKeyCredential" a
    ScriptCredential a -> encodeTagged' "ScriptCredential" a

instance DecodeAeson Credential where
  decodeAeson a = lmap (Named "Credential") do
    obj <- decodeAeson a
    tag <- obj .: "tag"
    case tag of
      "PubKeyCredential" -> PubKeyCredential <$> obj .: "contents"
      "ScriptCredential" -> ScriptCredential <$> obj .: "contents"
      _ -> Left $ AtKey "tag" $ UnexpectedValue $ encodeString tag

instance ToData Credential where
  toData = genericToData

instance FromData Credential where
  fromData = genericFromData

--------------------------------------------------------------------------------
-- StakingCredential
--------------------------------------------------------------------------------

-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Api.html#t:StakingCredential
-- Plutus rev: dbefda30be6490c758aa88b600f5874f12712b3a
-- | Staking credential used to assign rewards.
data StakingCredential
  = StakingHash Credential
  | StakingPtr
      { slot :: BigInt
      , txIx :: BigInt
      , certIx :: BigInt
      }

derive instance Eq StakingCredential
derive instance Ord StakingCredential
derive instance Generic StakingCredential _

instance Show StakingCredential where
  show = genericShow

instance
  HasPlutusSchema
    StakingCredential
    ( "StakingHash" := PNil @@ Z
        :+ "StakingPtr"
        :=
          ( "slot" := I BigInt :+ "txIx" := I BigInt :+ "certIx"
              := I BigInt
              :+ PNil
          )
        @@ (S Z)
        :+ PNil
    )

instance ToData StakingCredential where
  toData = genericToData

instance FromData StakingCredential where
  fromData = genericFromData

instance EncodeAeson StakingCredential where
  encodeAeson = case _ of
    StakingHash a -> encodeTagged' "StakingHash" a
    StakingPtr ptr -> encodeTagged' "StakingPtr"
      (ptr.slot /\ ptr.txIx /\ ptr.certIx)

instance DecodeAeson StakingCredential where
  decodeAeson a = lmap (Named "StakingCredential") do
    obj <- decodeAeson a
    tag <- obj .: "tag"
    case tag of
      "StakingHash" -> StakingHash <$> obj .: "contents"
      "StakingPtr" -> toStakingPtr <$> obj .: "contents"
      _ -> Left $ AtKey "tag" $ UnexpectedValue $ encodeString tag
    where
    toStakingPtr
      :: (BigInt /\ BigInt /\ BigInt) -> StakingCredential
    toStakingPtr (slot /\ txIx /\ certIx) = StakingPtr { slot, txIx, certIx }
