{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.String.ConversionsSpec where

import           Control.DeepSeq
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Data.String.Conversions
import           Data.String.Conversions.Monomorphic

import qualified Data.ByteString.Builder


instance NFData Data.ByteString.Builder.Builder where
  rnf = rnf . toLBS

instance Arbitrary Data.ByteString.Builder.Builder where
  arbitrary = fmap fromSBS arbitrary


spec :: Spec
spec = do
  describe "cs" $ do
    context "losslessly" $ do
      it "converts String -> String -> String" $ do
        property $ \(s :: String) ->
          s `shouldBe` cs (toString s)

      it "converts String -> StrictText -> String" $ do
        property $ \(s :: String) ->
          s `shouldBe` cs (toStrictText s)

      it "converts String -> LazyText -> String" $ do
        property $ \(s :: String) ->
          s `shouldBe` cs (toLazyText s)

      it "converts String -> StrictByteString -> String" $ do
        property $ \(s :: String) ->
          s `shouldBe` cs (toStrictByteString s)

      it "converts String -> LazyByteString -> String" $ do
        property $ \(s :: String) ->
          s `shouldBe` cs (toLazyByteString s)

      it "converts String -> ByteStringBuilder -> String" $ do
        property $ \(s :: String) ->
          s `shouldBe` cs (toByteStringBuilder s)

      it "converts StrictText -> String -> StrictText" $ do
        property $ \(s :: StrictText) ->
          s `shouldBe` cs (toString s)

      it "converts StrictText -> StrictText -> StrictText" $ do
        property $ \(s :: StrictText) ->
          s `shouldBe` cs (toStrictText s)

      it "converts StrictText -> LazyText -> StrictText" $ do
        property $ \(s :: StrictText) ->
          s `shouldBe` cs (toLazyText s)

      it "converts StrictText -> StrictByteString -> StrictText" $ do
        property $ \(s :: StrictText) ->
          s `shouldBe` cs (toStrictByteString s)

      it "converts StrictText -> LazyByteString -> StrictText" $ do
        property $ \(s :: StrictText) ->
          s `shouldBe` cs (toLazyByteString s)

      it "converts StrictText -> ByteStringBuilder -> StrictText" $ do
        property $ \(s :: StrictText) ->
          s `shouldBe` cs (toByteStringBuilder s)

      it "converts LazyText -> String -> LazyText" $ do
        property $ \(s :: LazyText) ->
          s `shouldBe` cs (toString s)

      it "converts LazyText -> StrictText -> LazyText" $ do
        property $ \(s :: LazyText) ->
          s `shouldBe` cs (toStrictText s)

      it "converts LazyText -> LazyText -> LazyText" $ do
        property $ \(s :: LazyText) ->
          s `shouldBe` cs (toLazyText s)

      it "converts LazyText -> StrictByteString -> LazyText" $ do
        property $ \(s :: LazyText) ->
          s `shouldBe` cs (toStrictByteString s)

      it "converts LazyText -> LazyByteString -> LazyText" $ do
        property $ \(s :: LazyText) ->
          s `shouldBe` cs (toLazyByteString s)

      it "converts LazyText -> ByteStringBuilder -> LazyText" $ do
        property $ \(s :: LazyText) ->
          s `shouldBe` cs (toByteStringBuilder s)

      it "converts StrictByteString -> StrictByteString -> StrictByteString" $ do
        property $ \(s :: StrictByteString) ->
          s `shouldBe` cs (toStrictByteString s)

      it "converts StrictByteString -> LazyByteString -> StrictByteString" $ do
        property $ \(s :: StrictByteString) ->
          s `shouldBe` cs (toLazyByteString s)

      it "converts StrictByteString -> ByteStringBuilder -> StrictByteString" $ do
        property $ \(s :: StrictByteString) ->
          s `shouldBe` cs (toByteStringBuilder s)

      it "converts LazyByteString -> StrictByteString -> LazyByteString" $ do
        property $ \(s :: LazyByteString) ->
          s `shouldBe` cs (toStrictByteString s)

      it "converts LazyByteString -> LazyByteString -> LazyByteString" $ do
        property $ \(s :: LazyByteString) ->
          s `shouldBe` cs (toLazyByteString s)

      it "converts LazyByteString -> ByteStringBuilder -> LazyByteString" $ do
        property $ \(s :: LazyByteString) ->
          s `shouldBe` cs (toByteStringBuilder s)

      it "converts ByteStringBuilder -> StrictByteString -> ByteStringBuilder" $ do
        property $ \(s :: ByteStringBuilder) ->
          (toLazyByteString s) `shouldBe` (toLazyByteString (toByteStringBuilder s))

      it "converts ByteStringBuilder -> LazyByteString -> ByteStringBuilder" $ do
        property $ \(s :: ByteStringBuilder) ->
          (toStrictByteString s) `shouldBe` (toStrictByteString (toByteStringBuilder s))

      it "converts ByteStringBuilder -> ByteStringBuilder -> ByteStringBuilder" $ do
        property $ \(s :: ByteStringBuilder) ->
          (toLazyByteString s) `shouldBe` (toLazyByteString (toByteStringBuilder s))

    context "without crashing" $ do
      it "converts String -> String" $ do
        property $ \(s :: String) ->
          deepseq (toString s) pass

      it "converts String -> StrictText" $ do
        property $ \(s :: String) ->
          deepseq (toStrictText s) pass

      it "converts String -> LazyText" $ do
        property $ \(s :: String) ->
          deepseq (toLazyText s) pass

      it "converts String -> StrictByteString" $ do
        property $ \(s :: String) ->
          deepseq (toStrictByteString s) pass

      it "converts String -> LazyByteString" $ do
        property $ \(s :: String) ->
          deepseq (toLazyByteString s) pass

      it "converts String -> ByteStringBuilder" $ do
        property $ \(s :: String) ->
          deepseq (toByteStringBuilder s) pass

      it "converts StrictText -> String" $ do
        property $ \(s :: StrictText) ->
          deepseq (toString s) pass

      it "converts StrictText -> StrictText" $ do
        property $ \(s :: StrictText) ->
          deepseq (toStrictText s) pass

      it "converts StrictText -> LazyText" $ do
        property $ \(s :: StrictText) ->
          deepseq (toLazyText s) pass

      it "converts StrictText -> StrictByteString" $ do
        property $ \(s :: StrictText) ->
          deepseq (toStrictByteString s) pass

      it "converts StrictText -> LazyByteString" $ do
        property $ \(s :: StrictText) ->
          deepseq (toLazyByteString s) pass

      it "converts StrictText -> ByteStringBuilder" $ do
        property $ \(s :: StrictText) ->
          deepseq (toByteStringBuilder s) pass

      it "converts LazyText -> String" $ do
        property $ \(s :: LazyText) ->
          deepseq (toString s) pass

      it "converts LazyText -> StrictText" $ do
        property $ \(s :: LazyText) ->
          deepseq (toStrictText s) pass

      it "converts LazyText -> LazyText" $ do
        property $ \(s :: LazyText) ->
          deepseq (toLazyText s) pass

      it "converts LazyText -> StrictByteString" $ do
        property $ \(s :: LazyText) ->
          deepseq (toStrictByteString s) pass

      it "converts LazyText -> LazyByteString" $ do
        property $ \(s :: LazyText) ->
          deepseq (toLazyByteString s) pass

      it "converts LazyText -> ByteStringBuilder" $ do
        property $ \(s :: LazyText) ->
          deepseq (toByteStringBuilder s) pass

      it "converts StrictByteString -> String" $ do
        property $ \(s :: StrictByteString) ->
          deepseq (toString s) pass

      it "converts StrictByteString -> StrictText" $ do
        property $ \(s :: StrictByteString) ->
          deepseq (toStrictText s) pass

      it "converts StrictByteString -> LazyText" $ do
        property $ \(s :: StrictByteString) ->
          deepseq (toLazyText s) pass

      it "converts StrictByteString -> StrictByteString" $ do
        property $ \(s :: StrictByteString) ->
          deepseq (toStrictByteString s) pass

      it "converts StrictByteString -> LazyByteString" $ do
        property $ \(s :: StrictByteString) ->
          deepseq (toLazyByteString s) pass

      it "converts StrictByteString -> ByteStringBuilder" $ do
        property $ \(s :: StrictByteString) ->
          deepseq (toByteStringBuilder s) pass

      it "converts LazyByteString -> String" $ do
        property $ \(s :: LazyByteString) ->
          deepseq (toString s) pass

      it "converts LazyByteString -> StrictText" $ do
        property $ \(s :: LazyByteString) ->
          deepseq (toStrictText s) pass

      it "converts LazyByteString -> LazyText" $ do
        property $ \(s :: LazyByteString) ->
          deepseq (toLazyText s) pass

      it "converts LazyByteString -> StrictByteString" $ do
        property $ \(s :: LazyByteString) ->
          deepseq (toStrictByteString s) pass

      it "converts LazyByteString -> LazyByteString" $ do
        property $ \(s :: LazyByteString) ->
          deepseq (toLazyByteString s) pass

      it "converts LazyByteString -> ByteStringBuilder" $ do
        property $ \(s :: LazyByteString) ->
          deepseq (toByteStringBuilder s) pass

      it "converts ByteStringBuilder -> String" $ do
        property $ \(s :: ByteStringBuilder) ->
          deepseq (toString s) pass

      it "converts ByteStringBuilder -> StrictText" $ do
        property $ \(s :: ByteStringBuilder) ->
          deepseq (toStrictText s) pass

      it "converts ByteStringBuilder -> LazyText" $ do
        property $ \(s :: ByteStringBuilder) ->
          deepseq (toLazyText s) pass

      it "converts ByteStringBuilder -> StrictByteString" $ do
        property $ \(s :: ByteStringBuilder) ->
          deepseq (toStrictByteString s) pass

      it "converts ByteStringBuilder -> LazyByteString" $ do
        property $ \(s :: ByteStringBuilder) ->
          deepseq (toLazyByteString s) pass

      it "converts ByteStringBuilder -> ByteStringBuilder" $ do
        property $ \(s :: ByteStringBuilder) ->
          deepseq (toByteStringBuilder s) pass


pass :: IO ()
pass = return ()
