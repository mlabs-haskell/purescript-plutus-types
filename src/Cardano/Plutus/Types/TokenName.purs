module Cardano.Plutus.Types.TokenName
  ( TokenName(TokenName)
  , adaToken
  , mkTokenName
  , fromText
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , encodeAeson
  , getField
  )
import Cardano.FromData (class FromData)
import Cardano.FromMetadata (class FromMetadata)
import Cardano.Serialization.Lib (assetName_new)
import Cardano.ToData (class ToData)
import Cardano.ToMetadata (class ToMetadata)
import Cardano.Types.AssetName
  ( AssetName(AssetName)
  , fromAssetName
  , mkAssetName
  )
import Data.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Data.Either (Either(Left, Right), note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.TextEncoder (encodeUtf8)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)

adaToken :: TokenName
adaToken = wrap $ unsafePartial $ fromJust $ mkAssetName mempty

newtype TokenName = TokenName AssetName

mkTokenName :: ByteArray -> Maybe TokenName
mkTokenName = mkAssetName >>> map wrap

fromText :: String -> Maybe TokenName
fromText = encodeUtf8 >>> wrap >>> mkTokenName

derive instance Newtype TokenName _
derive instance Generic TokenName _
derive newtype instance Eq TokenName
derive newtype instance Ord TokenName
derive newtype instance FromData TokenName
derive newtype instance FromMetadata TokenName
derive newtype instance ToData TokenName
derive newtype instance ToMetadata TokenName
derive newtype instance Arbitrary TokenName

instance Show TokenName where
  show = genericShow

-- | Corresponds to the Haskell instance at https://github.com/input-output-hk/plutus/blob/4fd86930f1dc628a816adf5f5d854b3fec578312/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs#L155:
instance DecodeAeson TokenName where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \aes -> do
      tkstr <- getField aes "unTokenName"
      case String.take 3 tkstr of
        "\x0000000x" -> do -- this is 3 characters '\NUL' '0' 'x'
          let stripped = String.drop 3 tkstr -- strip the \NUL followed by "0x"
          ba <-
            note
              (TypeMismatch $ "Expected base16 encoded string got " <> stripped)
              $ hexToByteArray stripped
          pure $ TokenName $ AssetName $ assetName_new ba
        "\x0\x0\x0" -> Right $ tkFromStr (String.drop 2 tkstr) -- if the original started with \NUL, we prepended 2 additional \NULs
        _ -> Right $ tkFromStr tkstr
    where
    tkFromStr :: String -> TokenName
    tkFromStr = TokenName <<< AssetName <<< assetName_new <<< wrap <<< encodeUtf8

instance EncodeAeson TokenName where
  encodeAeson = encodeAeson <<< { "unTokenName": _ }
    <<< fromAssetName
      (\ba -> "\x0" <> "0x" <> byteArrayToHex ba)
      ( \s -> case String.take 1 s of
          "\x0" -> "\x0\x0" <> s
          _ -> s
      )
    <<< unwrap
