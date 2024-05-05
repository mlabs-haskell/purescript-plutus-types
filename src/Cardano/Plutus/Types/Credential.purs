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
  , PNil
  , S
  , Z
  )
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash)
import Cardano.Plutus.Types.ValidatorHash (ValidatorHash)
import Cardano.ToData (class ToData, genericToData)
import Cardano.Types.Credential as Cardano
import Cardano.Types.Internal.Helpers (encodeTagged')
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)

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

toCardano :: Credential -> Cardano.Credential
toCardano (PubKeyCredential pubKeyHash) =
  Cardano.PubKeyHashCredential (unwrap pubKeyHash)
toCardano (ScriptCredential validatorHash) =
  Cardano.ScriptHashCredential (unwrap validatorHash)

fromCardano :: Cardano.Credential -> Credential
fromCardano (Cardano.PubKeyHashCredential pubKeyHash) =
  PubKeyCredential $ wrap pubKeyHash
fromCardano (Cardano.ScriptHashCredential scriptHash) =
  ScriptCredential $ wrap scriptHash
