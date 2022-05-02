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
    context "is identity for" $ do
      it "String" $ do
        property $ \(s :: String) ->
          s `shouldBe` toString s

      it "StrictText" $ do
        property $ \(s :: StrictText) ->
          s `shouldBe` toStrictText s

      it "LazyText" $ do
        property $ \(s :: LazyText) ->
          s `shouldBe` toLazyText s

      it "StrictByteString" $ do
        property $ \(s :: StrictByteString) ->
          s `shouldBe` toStrictByteString s

      it "LazyByteString" $ do
        property $ \(s :: LazyByteString) ->
          s `shouldBe` toLazyByteString s

    context "round-trips" $ do
      context "from String" $ do
        it "via StrictText" $ do
          roundTripTest toStrictText toString

        it "via LazyText" $ do
          roundTripTest toLazyText toString

        it "via StrictByteString" $ do
          roundTripTest toStrictByteString toString

        it "via LazyByteString" $ do
          roundTripTest toLazyByteString toString

      context "from StrictText" $ do
        it "via String" $ do
          roundTripTest toString toStrictText

        it "via LazyText" $ do
          roundTripTest toLazyText toStrictText

        it "via StrictByteString" $ do
          roundTripTest toStrictByteString toStrictText

        it "via LazyByteString" $ do
          roundTripTest toLazyByteString toStrictText

      context "from LazyText" $ do
        it "via String" $ do
          roundTripTest toString toLazyText

        it "via StrictText" $ do
          roundTripTest toStrictText toLazyText

        it "via StrictByteString" $ do
          roundTripTest toStrictByteString toLazyText

        it "via LazyByteString" $ do
          roundTripTest toLazyByteString toLazyText

      context "from StrictByteString" $ do
        it "via LazyByteString" $ do
          roundTripTest toLazyByteString toStrictByteString

      context "from LazyByteString" $ do
        it "via StrictByteString" $ do
          roundTripTest toStrictByteString toLazyByteString

    context "doesn't crash when converting (possibly invalid) byte sequences to unicode" $ do
      context "from StrictByteString" $ do
        it "to String" $ do
          smokeTest (cs :: StrictByteString -> String)

        it "to StrictText" $ do
          smokeTest (cs :: StrictByteString -> StrictText)

        it "to LazyText" $ do
          smokeTest (cs :: StrictByteString -> LazyText)

      context "from LazyByteString" $ do
        it "to String" $ do
          smokeTest (cs :: LazyByteString -> String)

        it "to StrictText" $ do
          smokeTest (cs :: LazyByteString -> StrictText)

        it "to LazyText" $ do
          smokeTest (cs :: LazyByteString -> LazyText)


smokeTest :: (Arbitrary a, Show a, NFData b) => (a -> b) -> Property
smokeTest convert = do
  property $ \(s :: a) ->
    deepseq (convert s) (return () :: IO ())


roundTripTest :: (Arbitrary a, Show a, Eq a) => (a -> b) -> (b -> a) -> Property
roundTripTest from to = do
  property $ \(s :: a) ->
    (to . from $ s) `shouldBe` s
