module Data.String.ConversionsSpec where

import           Control.DeepSeq
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Data.String.Conversions

spec :: Spec
spec = do
  describe "cs" $ do
    it "allows to convert from String to strict Text" $ do
      property $ \ s ->
        show (s :: String) `shouldBe` show (cs s :: ST)

    context "when converting from strict ByteString to strict Text" $ do
      it "never crashes" $ do
        property $ \ s ->
          show (s :: SBS) `shouldBe` show (cs s :: ST)
