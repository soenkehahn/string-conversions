{-# LANGUAGE FlexibleContexts #-}

-- | Monomorphic conversion functions that are useful when you wish to
-- be explicit about the conversion without writing a type signature
-- or when 'Data.String.Conversions.cs' is ambiguous due to
-- polymorphic inputs or outputs.
--
-- All functions in this module have 'Data.String.Conversions.cs' as
-- their implementation.
module Data.String.Conversions.Monomorphic (
    -- * From a polymorphic input to a monomorphic output
    toString,
    toStrictByteString,
    toSBS,
    toLazyByteString,
    toLBS,
    toByteStringBuilder,
    toBSB,
    toStrictText,
    toST,
    toLazyText,
    toLT,

    -- * From a monomorphic input to a polymorphic output

    fromString,
    fromStrictByteString,
    fromSBS,
    fromLazyByteString,
    fromLBS,
    fromByteStringBuilder,
    fromBSB,
    fromStrictText,
    fromST,
    fromLazyText,
    fromLT
  ) where

import Prelude (String)

import Data.String.Conversions

-- to a monomorphic type

toString :: ConvertibleStrings a String => a -> String
toString = cs

toStrictByteString :: ConvertibleStrings a StrictByteString => a -> StrictByteString
toStrictByteString = cs

toSBS :: ConvertibleStrings a SBS => a -> SBS
toSBS = cs

toLazyByteString :: ConvertibleStrings a LazyByteString => a -> LazyByteString
toLazyByteString = cs

toLBS :: ConvertibleStrings a LBS => a -> LBS
toLBS = cs

toByteStringBuilder :: ConvertibleStrings a ByteStringBuilder => a -> ByteStringBuilder
toByteStringBuilder = cs

toBSB :: ConvertibleStrings a BSB => a -> BSB
toBSB = cs

toStrictText :: ConvertibleStrings a StrictText => a -> StrictText
toStrictText = cs

toST :: ConvertibleStrings a ST => a -> ST
toST = cs

toLazyText :: ConvertibleStrings a LazyText => a -> LazyText
toLazyText = cs

toLT :: ConvertibleStrings a LT => a -> LT
toLT = cs

-- from a monomorphic type

fromString :: ConvertibleStrings String a => String -> a
fromString = cs

fromStrictByteString :: ConvertibleStrings StrictByteString a => StrictByteString -> a
fromStrictByteString = cs

fromSBS :: ConvertibleStrings SBS a => SBS -> a
fromSBS = cs

fromLazyByteString :: ConvertibleStrings LazyByteString a => LazyByteString -> a
fromLazyByteString = cs

fromLBS :: ConvertibleStrings LBS a => LBS -> a
fromLBS = cs

fromByteStringBuilder :: ConvertibleStrings ByteStringBuilder a => ByteStringBuilder -> a
fromByteStringBuilder = cs

fromBSB :: ConvertibleStrings BSB a => BSB -> a
fromBSB = cs

fromStrictText :: ConvertibleStrings StrictText a => StrictText -> a
fromStrictText = cs

fromST :: ConvertibleStrings ST a => ST -> a
fromST = cs

fromLazyText :: ConvertibleStrings LazyText a => LazyText -> a
fromLazyText = cs

fromLT :: ConvertibleStrings LT a => LT -> a
fromLT = cs
