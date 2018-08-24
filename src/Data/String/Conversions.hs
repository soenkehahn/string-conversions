{-# language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, CPP #-}

-- | A type class for converting strings.
-- Supported types are:
--
--     * 'String'
--
--     * strict 'Data.ByteString.ByteString'
--
--     * lazy 'Data.ByteString.Lazy.ByteString'
--
--     * strict 'Data.Text.Text'
--
--     * lazy 'Data.Text.Lazy.Text'
--
--     * 'Data.ByteString.Short.ShortByteString`
--
-- Assumes UTF-8 encoding for both types of ByteStrings.

module Data.String.Conversions (
    -- * class and conversions
    ConvertibleStrings(..),
    cs,

    -- * type synonyms
    StrictByteString,
    SBS,
    LazyByteString,
    LBS,
    StrictText,
    ST,
    LazyText,
    LT,

    -- | Generic string concatenation (with ghc >= 7.4 this is a re-export from Data.Monoid
    -- to avoid clashes.)
    (<>),
  ) where


import Data.Monoid

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- string imports

import qualified Data.ByteString
import qualified Data.ByteString.UTF8

import qualified Data.ByteString.Short

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.UTF8

import qualified Data.Text
import qualified Data.Text.Encoding hiding (decodeUtf8)
import qualified Data.Text.Encoding.Error

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding hiding (decodeUtf8)


-- * class and conversions

class ConvertibleStrings a b where
    convertString :: a -> b

cs :: ConvertibleStrings a b => a -> b
cs = convertString

#if !MIN_VERSION_base(4,5,0)
(<>) :: Monoid s => s -> s -> s
(<>) = mappend
#endif

-- * type synonyms

type StrictByteString = Data.ByteString.ByteString
type SBS              = Data.ByteString.ByteString

type LazyByteString = Data.ByteString.Lazy.ByteString
type LBS            = Data.ByteString.Lazy.ByteString

type StrictText = Data.Text.Text
type ST         = Data.Text.Text

type LazyText = Data.Text.Lazy.Text
type LT       = Data.Text.Lazy.Text

type ShortBS  = Data.ByteString.Short.ShortByteString 

-- instances
-- ---------

-- from String

instance ConvertibleStrings String String where
    convertString = id

instance ConvertibleStrings String StrictByteString where
    convertString = Data.ByteString.UTF8.fromString

instance ConvertibleStrings String LazyByteString where
    convertString = Data.ByteString.Lazy.UTF8.fromString

instance ConvertibleStrings String StrictText where
    convertString = Data.Text.pack

instance ConvertibleStrings String LazyText where
    convertString = Data.Text.Lazy.pack

instance ConvertibleStrings String ShortBS where
    convertString = Data.ByteString.Short.toShort . Data.ByteString.UTF8.fromString 

-- from StrictByteString

instance ConvertibleStrings StrictByteString StrictByteString where
    convertString = id

instance ConvertibleStrings StrictByteString String where
    convertString = Data.ByteString.UTF8.toString

instance ConvertibleStrings StrictByteString LazyByteString where
    convertString = Data.ByteString.Lazy.fromChunks . pure

instance ConvertibleStrings StrictByteString StrictText where
    convertString = Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode

instance ConvertibleStrings StrictByteString LazyText where
    convertString =
        Data.Text.Lazy.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode .
        Data.ByteString.Lazy.fromChunks . pure

instance ConvertibleStrings StrictByteString ShortBS where
    convertString = Data.ByteString.Short.toShort

-- from LazyByteString

instance ConvertibleStrings LazyByteString LazyByteString where
    convertString = id

instance ConvertibleStrings LazyByteString String where
    convertString = Data.ByteString.Lazy.UTF8.toString

instance ConvertibleStrings LazyByteString StrictByteString where
    convertString = mconcat . Data.ByteString.Lazy.toChunks

instance ConvertibleStrings LazyByteString StrictText where
    convertString =
        Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode .
        mconcat . Data.ByteString.Lazy.toChunks

instance ConvertibleStrings LazyByteString LazyText where
    convertString = Data.Text.Lazy.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode

instance ConvertibleStrings LazyByteString ShortBS where
    convertString = Data.ByteString.Short.toShort . mconcat . Data.ByteString.Lazy.toChunks

-- from StrictText

instance ConvertibleStrings StrictText StrictText where
    convertString = id

instance ConvertibleStrings StrictText String where
    convertString = Data.Text.unpack

instance ConvertibleStrings StrictText StrictByteString where
    convertString = Data.Text.Encoding.encodeUtf8

instance ConvertibleStrings StrictText LazyByteString where
    convertString = Data.ByteString.Lazy.fromChunks . pure . Data.Text.Encoding.encodeUtf8

instance ConvertibleStrings StrictText LazyText where
    convertString = Data.Text.Lazy.fromChunks . pure

instance ConvertibleStrings StrictText ShortBS where
    convertString = Data.ByteString.Short.toShort . Data.Text.Encoding.encodeUtf8

-- from LazyText

instance ConvertibleStrings LazyText LazyText where
    convertString = id

instance ConvertibleStrings LazyText String where
    convertString = Data.Text.Lazy.unpack

instance ConvertibleStrings LazyText StrictByteString where
    convertString =
        mconcat . Data.ByteString.Lazy.toChunks . Data.Text.Lazy.Encoding.encodeUtf8

instance ConvertibleStrings LazyText LazyByteString where
    convertString = Data.Text.Lazy.Encoding.encodeUtf8

instance ConvertibleStrings LazyText StrictText where
    convertString = mconcat . Data.Text.Lazy.toChunks

instance ConvertibleStrings LazyText ShortBS where
    convertString = Data.ByteString.Short.toShort .
        mconcat . Data.ByteString.Lazy.toChunks . Data.Text.Lazy.Encoding.encodeUtf8

-- from ShortBS

instance ConvertibleStrings ShortBS ShortBS where
    convertString = id

instance ConvertibleStrings ShortBS StrictByteString where
    convertString = Data.ByteString.Short.fromShort

instance ConvertibleStrings ShortBS String where
    convertString = Data.ByteString.UTF8.toString . Data.ByteString.Short.fromShort

instance ConvertibleStrings ShortBS LazyByteString where
    convertString = Data.ByteString.Lazy.fromChunks . pure . Data.ByteString.Short.fromShort

instance ConvertibleStrings ShortBS StrictText where
    convertString = 
        Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode . 
        Data.ByteString.Short.fromShort

instance ConvertibleStrings ShortBS LazyText where
    convertString =
        Data.Text.Lazy.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode .
        Data.ByteString.Lazy.fromChunks . pure . Data.ByteString.Short.fromShort
