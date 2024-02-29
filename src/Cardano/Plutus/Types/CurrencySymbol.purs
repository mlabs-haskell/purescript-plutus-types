module Cardano.Plutus.Types.CurrencySymbol
  ( CurrencySymbol
  , adaSymbol
  , mkCurrencySymbol
  , fromMintingPolicyHash
  , fromScriptHash
  , unCurrencySymbol
  , pprintCurrencySymbol
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getField
  )
import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.FromData (class FromData)
import Cardano.FromMetadata (class FromMetadata)
import Cardano.Plutus.Types.MintingPolicyHash
  ( MintingPolicyHash(MintingPolicyHash)
  )
import Cardano.ToData (class ToData)
import Cardano.ToMetadata (class ToMetadata)
import Cardano.Types.ScriptHash (ScriptHash)
import Control.Monad.Gen as Gen
import Data.Array.NonEmpty (fromArray)
import Data.ByteArray (ByteArray, byteArrayToHex, hexToByteArrayUnsafe)
import Data.Either (Either(Left))
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap, wrap)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary)

-- | https://github.com/IntersectMBO/plutus/blob/eceae8831b8186655535dee587486dbd3fd037f4/plutus-ledger-api/src/PlutusLedgerApi/V1/Value.hs#L88
newtype CurrencySymbol = CurrencySymbol ByteArray

derive newtype instance Eq CurrencySymbol
derive newtype instance Ord CurrencySymbol
derive newtype instance FromData CurrencySymbol
derive newtype instance FromMetadata CurrencySymbol
derive newtype instance ToData CurrencySymbol
derive newtype instance ToMetadata CurrencySymbol

instance DecodeAeson CurrencySymbol where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    (flip getField "unCurrencySymbol" >=> decodeAeson >>> map CurrencySymbol)

instance EncodeAeson CurrencySymbol where
  encodeAeson (CurrencySymbol mph) = encodeAeson { "unCurrencySymbol": mph }

instance Show CurrencySymbol where
  show (CurrencySymbol cs) = "(CurrencySymbol " <> show cs <> ")"

instance Arbitrary CurrencySymbol where
  arbitrary = Gen.elements $ map translate $ unsafeNonEmpty
    [ "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65"
    , "92c4f22371bd453aec9fe19ccebfbc88211ae854b5eab424bcd4c26d"
    , "c9e9c15d4f16a7948d3736c93aa79034621d51dccc4df5d31c7d34aa"
    ]
    where
    unsafeNonEmpty x = unsafePartial $ fromJust $ fromArray x

    translate :: String -> CurrencySymbol
    translate x =
      fromScriptHash $ unsafePartial $ fromJust
        $ decodeCbor
        $ wrap
        $ hexToByteArrayUnsafe x

mkCurrencySymbol :: ByteArray -> Maybe CurrencySymbol
mkCurrencySymbol byteArr
  | byteArr == mempty =
      pure adaSymbol
  | otherwise =
      (decodeCbor (wrap byteArr) :: Maybe ScriptHash) $> CurrencySymbol
        byteArr

adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol mempty

fromScriptHash :: ScriptHash -> CurrencySymbol
fromScriptHash = CurrencySymbol <<< unwrap <<< encodeCbor

-- | The currency symbol of a monetary policy hash.
fromMintingPolicyHash :: MintingPolicyHash -> CurrencySymbol
fromMintingPolicyHash (MintingPolicyHash h) = CurrencySymbol $ unwrap $ encodeCbor h

unCurrencySymbol :: CurrencySymbol -> ByteArray
unCurrencySymbol (CurrencySymbol curSymbol) = curSymbol

pprintCurrencySymbol :: CurrencySymbol -> String
pprintCurrencySymbol cs
  | cs == adaSymbol = "Lovelace"
  | otherwise = byteArrayToHex $ unCurrencySymbol cs
