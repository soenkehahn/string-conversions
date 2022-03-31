{-# LANGUAGE ScopedTypeVariables #-}
module Data.String.ConversionsSpec where

import           Control.DeepSeq
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Data.String.Conversions
import           Data.String.Conversions.Monomorphic

spec :: Spec
spec = do
  describe "cs" $ do
    context "identity" $ do
      it "correct for String" $ do
        property $ \(s :: String) ->
          s `shouldBe` toString s

      it "correct for StrictText" $ do
        property $ \(s :: StrictText) ->
          s `shouldBe` toStrictText s

      it "correct for LazyText" $ do
        property $ \(s :: LazyText) ->
          s `shouldBe` toLazyText s

      it "correct for StrictByteString" $ do
        property $ \(s :: StrictByteString) ->
          s `shouldBe` toStrictByteString s

      it "correct for LazyByteString" $ do
        property $ \(s :: LazyByteString) ->
          s `shouldBe` toLazyByteString s

    context "round trip" $ do
      context "String" $ do
        it "correct for String -> StrictText -> String" $ do
          roundTripTest toStrictText toString

        it "correct for String -> LazyText -> String" $ do
          roundTripTest toLazyText toString

        it "correct for String -> StrictByteString -> String" $ do
          roundTripTest toStrictByteString toString

        it "correct for String -> LazyByteString -> String" $ do
          roundTripTest toLazyByteString toString

      context "StrictText" $ do
        it "correct for StrictText -> String -> StrictText" $ do
          roundTripTest toString toStrictText

        it "correct for StrictText -> LazyText -> StrictText" $ do
          roundTripTest toLazyText toStrictText

        it "correct for StrictText -> StrictByteString -> StrictText" $ do
          roundTripTest toStrictByteString toStrictText

        it "correct for StrictText -> LazyByteString -> StrictText" $ do
          roundTripTest toLazyByteString toStrictText

      context "LazyText" $ do
        it "correct for LazyText -> String -> LazyText" $ do
          roundTripTest toString toLazyText

        it "correct for LazyText -> StrictText -> LazyText" $ do
          roundTripTest toStrictText toLazyText

        it "correct for LazyText -> StrictByteString -> LazyText" $ do
          roundTripTest toStrictByteString toLazyText

        it "correct for LazyText -> LazyByteString -> LazyText" $ do
          roundTripTest toLazyByteString toLazyText

      context "byte strings" $ do
        it "correct for StrictByteString -> LazyByteString -> StrictByteString" $ do
          roundTripTest toLazyByteString toStrictByteString

        it "correct for LazyByteString -> StrictByteString -> LazyByteString" $ do
          roundTripTest toStrictByteString toLazyByteString

    context "unsafe byte sequence to unicode conversion" $ do
      context "StrictByteString" $ do
        it "completes for StrictByteString -> String" $ do
          smokeTest (cs :: StrictByteString -> String)

        it "completes for StrictByteString -> StrictText" $ do
          smokeTest (cs :: StrictByteString -> StrictText)

        it "completes for StrictByteString -> LazyText" $ do
          smokeTest (cs :: StrictByteString -> LazyText)

      context "LazyByteString" $ do
        it "completes for LazyByteString -> String" $ do
          smokeTest (cs :: LazyByteString -> String)

        it "completes for LazyByteString -> StrictText" $ do
          smokeTest (cs :: LazyByteString -> StrictText)

        it "completes for LazyByteString -> LazyText" $ do
          smokeTest (cs :: LazyByteString -> LazyText)


smokeTest :: (Arbitrary a, Show a, NFData b) => (a -> b) -> Property
smokeTest convert = do
  property $ \(s :: a) ->
    deepseq (convert s) (return () :: IO ())


roundTripTest :: (Arbitrary a, Show a, Eq a) => (a -> b) -> (b -> a) -> Property
roundTripTest from to = do
  property $ \(s :: a) ->
    (to . from $ s) `shouldBe` s
