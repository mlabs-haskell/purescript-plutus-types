# purescript-plutus-types

This project aims to build compatibility layer for Cardano Plutus in PureScript.

It is mainly used in [`cardano-transaction-lib`](https://github.com/Plutonomicon/cardano-transaction-lib/) to provide [PlutusData](https://github.com/mlabs-haskell/purescript-cardano-types/blob/e8375180b16b0730659725c42682ad0c916e3cb3/src/Cardano/Types/PlutusData.purs#L67) and JSON encodings for Plutus types the same way it is done in Plutus.

If you are building a Cardano dApp, the main use case of this package is to convert script arguments to `PlutusData` before applying them to parametrized scripts.

There are types from `plutus` and from `plutus-apps` mixed in this package for convenience.
