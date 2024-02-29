module Cardano.Plutus.Types.TransactionOutput where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (encodeCbor)
import Cardano.FromData (class FromData, fromData)
import Cardano.Plutus.Types.Address (Address)
import Cardano.Plutus.Types.Value (Value, pprintValue)
import Cardano.ToData (class ToData, toData)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.OutputDatum (OutputDatum, pprintOutputDatum)
import Cardano.Types.PlutusData (PlutusData(Constr))
import Cardano.Types.ScriptHash (ScriptHash)
import Data.ByteArray (byteArrayToHex)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(Proxy))

-- https://github.com/input-output-hk/plutus/blob/c8d4364d0e639fef4d5b93f7d6c0912d992b54f9/plutus-ledger-api/src/PlutusLedgerApi/V2/Tx.hs#L80
newtype TransactionOutput = TransactionOutput
  { address :: Address
  , amount :: Value
  , datum :: OutputDatum
  , referenceScript :: Maybe ScriptHash
  }

_amount :: Lens' TransactionOutput Value
_amount = _Newtype <<< prop (Proxy :: Proxy "amount")

_datum :: Lens' TransactionOutput OutputDatum
_datum = _Newtype <<< prop (Proxy :: Proxy "datum")

derive instance Generic TransactionOutput _
derive instance Newtype TransactionOutput _
derive newtype instance Eq TransactionOutput

instance Show TransactionOutput where
  show = genericShow

derive newtype instance DecodeAeson TransactionOutput
derive newtype instance EncodeAeson TransactionOutput

instance FromData TransactionOutput where
  fromData (Constr n [ addr, amt, datum, referenceScript ])
    | n == BigNum.zero =
        TransactionOutput <$>
          ( { address: _, amount: _, datum: _, referenceScript: _ }
              <$> fromData addr
              <*> fromData amt
              <*> fromData datum
              <*> fromData referenceScript
          )
  fromData _ = Nothing

instance ToData TransactionOutput where
  toData (TransactionOutput { address, amount, datum, referenceScript }) =
    Constr BigNum.zero
      [ toData address, toData amount, toData datum, toData referenceScript ]

pprintTransactionOutput :: TransactionOutput -> TagSet
pprintTransactionOutput
  (TransactionOutput { address, amount, datum, referenceScript }) =
  TagSet.fromArray $
    [ "address" `tag` show address
    , "amount" `tagSetTag` pprintValue amount
    , pprintOutputDatum datum
    ] <> referenceScriptTagSet
  where
  referenceScriptTagSet = maybe []
    ( pure <<< tag "referenceScript" <<< byteArrayToHex <<< unwrap <<<
        encodeCbor
    )
    referenceScript
