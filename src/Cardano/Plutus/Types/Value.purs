module Cardano.Plutus.Types.Value
  ( Coin(Coin)
  , Value
  , coinToValue
  , flattenMultiAssets
  , flattenValue
  , geq
  , getLovelace
  , getValue
  , gt
  , isCoinZero
  , isZero
  , leq
  , lovelaceValueOf
  , lt
  , negation
  , scale
  , singleton
  , split
  , symbols
  , unionWith
  , valueOf
  , valueToCoin
  , valueToCoin'
  , pprintValue
  , toCardano
  , fromCardano
  ) where

import Prelude hiding (eq)

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , decodeAeson
  , encodeAeson
  , getField
  )
import Cardano.FromData (class FromData)
import Cardano.Plutus.Types.CurrencySymbol
  ( CurrencySymbol
  , adaSymbol
  , pprintCurrencySymbol
  )
import Cardano.Plutus.Types.CurrencySymbol as CurrencySymbol
import Cardano.Plutus.Types.Map (Map(Map)) as Plutus
import Cardano.Plutus.Types.Map
  ( keys
  , lookup
  , mapThese
  , singleton
  , union
  ) as Plutus.Map
import Cardano.Plutus.Types.Map as PlutusMap
import Cardano.Plutus.Types.TokenName (TokenName, adaToken)
import Cardano.Plutus.Types.TokenName as TokenName
import Cardano.ToData (class ToData)
import Cardano.Types.Asset (Asset(Asset, AdaAsset))
import Cardano.Types.AssetName (unAssetName)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Value as Cardano
import Cardano.Types.Value as Value
import Data.Array (concatMap, filter, replicate)
import Data.Array as Array
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either(Left))
import Data.Foldable (all, fold)
import Data.Generic.Rep (class Generic)
import Data.Lattice (class JoinSemilattice, class MeetSemilattice)
import Data.Log.Tag (TagSet, tag)
import Data.Log.Tag as TagSet
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord (abs)
import Data.Show.Generic (genericShow)
import Data.These (These(Both, That, This), these)
import Data.Traversable (for, sequence)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt)

newtype Value = Value (Plutus.Map CurrencySymbol (Plutus.Map TokenName BigInt))

derive newtype instance ToData Value
derive newtype instance FromData Value

instance DecodeAeson Value where
  decodeAeson = caseAesonObject
    (Left $ TypeMismatch "Expected object")
    (flip getField "getValue" >=> decodeAeson >>> map Value)

instance EncodeAeson Value where
  encodeAeson (Value mph) = encodeAeson $ encodeAeson
    { "getValue": encodeAeson mph }

arbitrarySingletonValue :: Gen Value
arbitrarySingletonValue = do
  currencySymbol <- arbitrary
  tokenName <- arbitrary
  num <- BigInt.fromInt <<< abs <$> arbitrary
  pure $ singleton currencySymbol tokenName num

instance Arbitrary Value where
  arbitrary = do
    tokenNum <- chooseInt 0 10
    fold <$> (sequence $ replicate tokenNum arbitrarySingletonValue)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#eq
instance Eq Value where
  eq = checkBinRel (==)

instance Show Value where
  show (Value mp) = "(PlutusValue " <> show mp <> ")"

instance Semigroup Value where
  append = unionWith add

instance Monoid Value where
  mempty = Value (Plutus.Map [])

instance JoinSemilattice Value where
  join = unionWith max

instance MeetSemilattice Value where
  meet = unionWith min

--------------------------------------------------------------------------------
-- Coin (Ada Lovelaces)
--------------------------------------------------------------------------------
newtype Coin = Coin BigInt

derive instance Generic Coin _
derive instance Newtype Coin _
derive newtype instance Eq Coin

instance Show Coin where
  show = genericShow

instance Semigroup Coin where
  append (Coin c1) (Coin c2) = Coin (c1 + c2)

instance Monoid Coin where
  mempty = Coin zero

instance JoinSemilattice Coin where
  join (Coin c1) (Coin c2) = Coin (max c1 c2)

instance MeetSemilattice Coin where
  meet (Coin c1) (Coin c2) = Coin (min c1 c2)

-- | Get the amount of lovelaces in Ada `Coin`.
getLovelace :: Coin -> BigInt
getLovelace (Coin l) = l

-- | Create a `Value` containing only the given `Coin`.
coinToValue :: Coin -> Value
coinToValue (Coin i) = lovelaceValueOf i

-- | Get the `Coin` in the given `Value`.
valueToCoin :: Value -> Coin
valueToCoin v = Coin $ valueOf v adaSymbol adaToken

-- | Get the `Coin` in the given `Value` as a `BigInt`.
valueToCoin' :: Value -> BigInt
valueToCoin' = getLovelace <<< valueToCoin

-- | Check whether an 'Ada' value is zero.
isCoinZero :: Coin -> Boolean
isCoinZero (Coin i) = i == zero

--------------------------------------------------------------------------------
-- Public
--------------------------------------------------------------------------------

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#Value
-- | Gets the underlying `Plutus.Types.AssocMap.Map`.
getValue :: Value -> Plutus.Map CurrencySymbol (Plutus.Map TokenName BigInt)
getValue (Value mp) = mp

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#singleton
-- | Makes a `Value` containing only the given quantity of the given currency.
singleton :: CurrencySymbol -> TokenName -> BigInt -> Value
singleton cs tn = Value <<< Plutus.Map.singleton cs <<< Plutus.Map.singleton tn

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#valueOf
-- | Gets the quantity of the given currency in the `Value`.
valueOf :: Value -> CurrencySymbol -> TokenName -> BigInt
valueOf (Value mp) cs tn = fromMaybe zero $
  Plutus.Map.lookup cs mp >>= Plutus.Map.lookup tn

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Ada.html#lovelaceValueOf
-- | A Value with the given amount of Lovelace (the currency unit).
lovelaceValueOf :: BigInt -> Value
lovelaceValueOf = singleton adaSymbol adaToken

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#local-6989586621679887124
-- | Returns a new `Value` with all amounts multiplied by `s`.
scale :: BigInt -> Value -> Value
scale s (Value mp) = Value (map (map (mul s)) mp)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#symbols
-- | The list of `CurrencySymbol`s of a `Value`.
symbols :: Value -> Array CurrencySymbol
symbols (Value mp) = Plutus.Map.keys mp

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#isZero
-- | Checks whether a `Value` is zero.
isZero :: Value -> Boolean
isZero = all (all ((==) zero)) <<< getValue

negation :: Value -> Value
negation (Value mp) = Value (map (map negate) mp)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#split
-- | Splits a value into its positive and non-positive parts. The first element of
-- | the tuple contains the non-positive parts of the value, the second element
-- | contains the positive parts. The convention is non-positive parts are
-- | negated to make them positive in the output.
split :: Value -> Value /\ Value
split (Value mp) =
  let
    neg /\ pos = Plutus.Map.mapThese worker mp
  in
    negation (Value neg) /\ Value pos
  where
  worker
    :: Plutus.Map TokenName BigInt
    -> These (Plutus.Map TokenName BigInt) (Plutus.Map TokenName BigInt)
  worker mp' = Both l r
    where
    l /\ r =
      Plutus.Map.mapThese (\a -> if a <= zero then This a else That a) mp'

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionWith
-- | Combines `Value` with a binary function on `BigInt`s.
unionWith :: (BigInt -> BigInt -> BigInt) -> Value -> Value -> Value
unionWith f lhs =
  Value <<< map (map (these identity identity f)) <<< unionVal lhs

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#flattenValue
-- | Converts a value to a simple list, keeping only the non-zero amounts.
flattenValue :: Value -> Array (CurrencySymbol /\ TokenName /\ BigInt)
flattenValue (Value (Plutus.Map arr)) =
  Array.filter (\(_ /\ _ /\ amount) -> amount /= zero) $
    flip concatMap arr \(cs /\ (Plutus.Map tokens)) ->
      tokens <#> \(tn /\ value) ->
        cs /\ tn /\ value

-- | Converts a value to a simple list, keeping only the non-Ada assets
-- | with non-zero amounts.
flattenMultiAssets :: Value -> Array (CurrencySymbol /\ TokenName /\ BigInt)
flattenMultiAssets = filter (notEq adaSymbol <<< fst) <<< flattenValue

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#geq
-- | Checks whether one `Value` is greater than or equal to another.
geq :: Value -> Value -> Boolean
geq = checkBinRel (>=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#gt
-- | Checks whether one `Value` is strictly greater than another.
gt :: Value -> Value -> Boolean
gt l r = not (isZero l && isZero r) && checkBinRel (>) l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#leq
-- | Checks whether one `Value` is less than or equal to another.
leq :: Value -> Value -> Boolean
leq = checkBinRel (<=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#lt
-- | Checks whether one `Value` is strictly less than another.
lt :: Value -> Value -> Boolean
lt l r = not (isZero l && isZero r) && checkBinRel (<) l r

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionVal
-- Combines two 'Value' maps.
unionVal
  :: Value
  -> Value
  -> Plutus.Map CurrencySymbol (Plutus.Map TokenName (These BigInt BigInt))
unionVal (Value lhs') (Value rhs) =
  these (map This) (map That) Plutus.Map.union <$>
    Plutus.Map.union lhs' rhs

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkPred
checkPred :: (These BigInt BigInt -> Boolean) -> Value -> Value -> Boolean
checkPred f l r = all (all f) (unionVal l r)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkBinRel
-- Check whether a binary relation holds for value pairs of two `Value` maps,
-- supplying 0 where a key is only present in one of them.
checkBinRel :: (BigInt -> BigInt -> Boolean) -> Value -> Value -> Boolean
checkBinRel f l r = checkPred (these (flip f zero) (f zero) f) l r

-- | Pretty-print a Plutus domain `Value`
pprintValue :: Value -> TagSet
pprintValue (Value (Plutus.Map arr)) = TagSet.fromArray $
  arr <#> \(currency /\ Plutus.Map assets) ->
    TagSet.tagSetTag (pprintCurrencySymbol currency)
      $ TagSet.fromArray
      $ assets <#> \(tokenName /\ amount) ->
          byteArrayToHex (unAssetName $ unwrap tokenName) `tag` BigInt.toString amount

toCardano :: Value -> Maybe Cardano.Value
toCardano (Value mp) = do
  psMap <- PlutusMap.toCardano mp
  arrs <- Array.concat <$> for (Map.toUnfoldable psMap :: Array _) \(currencySymbol /\ assets) -> do
    psAssets <- PlutusMap.toCardano assets
    for (Map.toUnfoldable psAssets :: Array _) \(tokenName /\ quantity) -> do
      amount <- BigNum.fromBigInt quantity
      if currencySymbol == CurrencySymbol.adaSymbol then pure (AdaAsset /\ amount)
      else do
        cCurrencySymbol <- CurrencySymbol.toCardano currencySymbol
        pure $ Asset cCurrencySymbol (unwrap tokenName) /\ amount
  Value.unflatten arrs

fromCardano :: Cardano.Value -> Value
fromCardano cv = fold $
  Value.flatten cv <#> \(asset /\ amount) ->
    case asset of
      AdaAsset -> singleton CurrencySymbol.adaSymbol TokenName.adaToken $ BigNum.toBigInt amount
      Asset scriptHash assetName ->
        singleton (CurrencySymbol.fromCardano scriptHash) (wrap assetName) $ BigNum.toBigInt amount
