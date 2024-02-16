module Cardano.Plutus.Types.Address where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , encodeAeson
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
  , Z
  )
import Cardano.Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  )
import Cardano.Plutus.Types.PaymentPubKeyHash
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  )
import Cardano.Plutus.Types.StakingCredential
  ( StakingCredential(StakingHash)
  )
import Cardano.Plutus.Types.ValidatorHash (ValidatorHash)
import Cardano.ToData (class ToData, genericToData)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Show.Generic (genericShow)

-- | An address may contain two credentials, the payment credential and optionally a 'StakingCredential'.
-- |
-- | https://github.com/IntersectMBO/plutus/blob/eceae8831b8186655535dee587486dbd3fd037f4/plutus-ledger-api/src/PlutusLedgerApi/V1/Address.hs#L32
newtype Address = Address
  { addressCredential :: Credential
  , addressStakingCredential :: Maybe StakingCredential
  }

derive instance Eq Address
derive instance Ord Address
derive instance Newtype Address _
derive instance Generic Address _

instance Show Address where
  show = genericShow

instance
  HasPlutusSchema
    Address
    ( "Address"
        :=
          ( "addressCredential" := I Credential :+ "addressStakingCredential"
              := I (Maybe StakingCredential)
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData Address where
  toData = genericToData

instance FromData Address where
  fromData = genericFromData

instance DecodeAeson Address where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \obj -> do
      addressCredential <- obj .: "addressCredential"
      addressStakingCredential <- obj .: "addressStakingCredential"
      pure $ Address { addressCredential, addressStakingCredential }

instance EncodeAeson Address where
  encodeAeson (Address addr) = encodeAeson addr

-- | The address that should be targeted by a transaction output locked
-- | by the public key with the given hash.
pubKeyHashAddress :: PaymentPubKeyHash -> Maybe Credential -> Address
pubKeyHashAddress (PaymentPubKeyHash pkh) mbStakeCredential = wrap
  { addressCredential: PubKeyCredential pkh
  , addressStakingCredential:
      map StakingHash mbStakeCredential
  }

-- | The address that should be used by a transaction output locked
-- | by the given validator script hash.
scriptHashAddress :: ValidatorHash -> Maybe Credential -> Address
scriptHashAddress vh mbStakeCredential = wrap
  { addressCredential: ScriptCredential vh
  , addressStakingCredential: map StakingHash mbStakeCredential
  }

-- | The payment validator hash of the address (if any).
-- |
-- | https://github.com/IntersectMBO/plutus/blob/eceae8831b8186655535dee587486dbd3fd037f4/plutus-ledger-api/src/PlutusLedgerApi/V1/Address.hs#L63
getValidatorHash :: Address -> Maybe ValidatorHash
getValidatorHash addr =
  case (unwrap addr).addressCredential of
    ScriptCredential k -> Just k
    _ -> Nothing

-- | The staking credential of an address (if any).
-- |
-- | https://github.com/IntersectMBO/plutus/blob/eceae8831b8186655535dee587486dbd3fd037f4/plutus-ledger-api/src/PlutusLedgerApi/V1/Address.hs#L74
getStakingCredential :: Address -> Maybe StakingCredential
getStakingCredential = _.addressStakingCredential <<< unwrap
