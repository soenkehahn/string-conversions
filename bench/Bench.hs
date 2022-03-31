module Main (main) where
import           Data.String.Conversions
import           Data.String.Conversions.Monomorphic
import           Criterion.Main
import           Test.QuickCheck

import qualified Data.ByteString.Lazy

import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified Data.Text.Lazy


main :: IO ()
main = defaultMain
  [ env genStrings $ \ ~(short, medium, large) ->
      bgroup "conv"
        [ bgroup "cs"
            [ bgroup "short" $ benchConvertString short
            , bgroup "medium" $ benchConvertString medium
            , bgroup "large" $ benchConvertString large
            ]
        , bgroup "alt"
            [ bgroup "short" $ benchAlt short
            , bgroup "medium" $ benchAlt medium
            , bgroup "large" $ benchAlt large
            ]
        ]
  ]


genStrings :: IO ([String], [String], [String])
genStrings = do
  short  <- generate (vectorOf 100 $ resize 30 $ listOf arbitraryUnicodeChar)
  medium <- generate (vectorOf 100 $ resize 100 $ listOf arbitraryUnicodeChar)
  large  <- generate (vectorOf 100 $ resize 8000 $ listOf arbitraryUnicodeChar)
  return (short, medium, large)


benchConvertString :: [String] -> [Benchmark]
benchConvertString strings =
  [ bench "StrictByteString-to-LazyByteString" $
      nf (map toLazyByteString) (map toStrictByteString strings)
  , bench "StrictByteString-to-StrictText" $
      nf (map toStrictText) (map toStrictByteString strings)
  , bench "StrictByteString-to-LazyText" $
      nf (map toLazyText) (map toStrictByteString strings)
  , bench "LazyByteString-to-StrictByteString" $
      nf (map toStrictByteString) (map toLazyByteString strings)
  , bench "LazyByteString-to-StrictText" $
      nf (map toStrictText) (map toLazyByteString strings)
  , bench "LazyByteString-to-LazyText" $
      nf (map toLazyText) (map toLazyByteString strings)
  , bench "StrictText-to-StrictByteString" $
      nf (map toStrictByteString) (map toStrictText strings)
  , bench "StrictText-to-LazyByteString" $
      nf (map toLazyByteString) (map toStrictText strings)
  , bench "StrictText-to-LazyText" $
      nf (map toLazyText) (map toStrictText strings)
  , bench "LazyText-to-StrictByteString" $
      nf (map toStrictByteString) (map toLazyText strings)
  , bench "LazyText-to-LazyByteString" $
      nf (map toLazyByteString) (map toLazyText strings)
  , bench "LazyText-to-StrictText" $
      nf (map toStrictText) (map toLazyText strings)
  ]


benchAlt :: [String] -> [Benchmark]
benchAlt strings =
  [ bench "StrictByteString-to-LazyText" $
      nf (map altSBStoLT) (map toSBS strings)

  , bench "LazyByteString-to-LazyText" $
      nf (map altLBStoLT) (map toLBS strings)
  ]


altSBStoLT :: StrictByteString -> LazyText
altSBStoLT =
  Data.Text.Lazy.fromChunks . pure .
  Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode


altLBStoLT :: LazyByteString -> LazyText
altLBStoLT =
  Data.Text.Lazy.fromChunks .
  map (Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode) .
  Data.ByteString.Lazy.toChunks
