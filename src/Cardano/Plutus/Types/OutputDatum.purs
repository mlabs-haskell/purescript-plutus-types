module Cardano.Plutus.Types.OutputDatum where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch, UnexpectedValue)
  , caseAesonObject
  , toStringifiedNumbersJson
  , (.:)
  )
import Aeson as Aeson
import Cardano.AsCbor (encodeCbor)
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
import Cardano.ToData (class ToData, genericToData)
import Cardano.Types.DataHash (DataHash)
import Cardano.Types.Internal.Helpers (encodeTagged')
import Cardano.Types.OutputDatum as Cardano
import Cardano.Types.PlutusData (PlutusData, pprintPlutusData)
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)

-- https://github.com/IntersectMBO/plutus/blob/c8d4364d0e639fef4d5b93f7d6c0912d992b54f9/plutus-ledger-api/src/PlutusLedgerApi/V2/Tx.hs#L61
data OutputDatum = NoOutputDatum | OutputDatumHash DataHash | OutputDatum PlutusData

derive instance Generic OutputDatum _
derive instance Eq OutputDatum

instance Show OutputDatum where
  show = genericShow

instance
  HasPlutusSchema OutputDatum
    ( "NoOutputDatum" := PNil @@ Z
        :+ "OutputDatumHash"
        := PNil
        @@ (S Z)
        :+ "OutputDatum"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData OutputDatum where
  toData = genericToData

instance FromData OutputDatum where
  fromData = genericFromData

instance EncodeAeson OutputDatum where
  encodeAeson = case _ of
    NoOutputDatum -> encodeTagged' "NoOutputDatum" {}
    OutputDatumHash r -> encodeTagged' "OutputDatumHash" r
    OutputDatum r -> encodeTagged' "OutputDatum" r

instance DecodeAeson OutputDatum where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \obj -> do
      tag <- obj .: "tag"
      case tag of
        "NoOutputDatum" ->
          pure NoOutputDatum
        "OutputDatumHash" -> do
          dataHash <- obj .: "contents"
          pure $ OutputDatumHash dataHash
        "OutputDatum" -> do
          datum <- obj .: "contents"
          pure $ OutputDatum datum
        tagValue -> do
          Left $ UnexpectedValue $ toStringifiedNumbersJson $ Aeson.fromString
            tagValue

pprintOutputDatum :: OutputDatum -> TagSet
pprintOutputDatum = TagSet.fromArray <<< case _ of
  NoOutputDatum -> [ "datum" `tag` "none" ]
  OutputDatumHash hash ->
    [ "datumHash" `tag` byteArrayToHex (unwrap $ encodeCbor hash) ]
  OutputDatum d ->
    [ "datum" `tagSetTag` pprintPlutusData d ]

fromCardano :: Cardano.OutputDatum -> OutputDatum
fromCardano (Cardano.OutputDatum d) = OutputDatum d
fromCardano (Cardano.OutputDatumHash d) = OutputDatumHash d

toCardano :: OutputDatum -> Maybe Cardano.OutputDatum
toCardano (OutputDatum d) = Just $ Cardano.OutputDatum d
toCardano (OutputDatumHash d) = Just $ Cardano.OutputDatumHash d
toCardano NoOutputDatum = Nothing
