{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Data.String.ConversionsSpec where

import           Control.DeepSeq
import           Safe
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
          deepseq (cs (s :: SBS) :: ST) (return () :: IO ())

    it "returns the same result, no matter which conversion path you take" $ do
      property performTest

  describe "convertTo" $ do
    it "always produces a value of the correct type" $
      property $ \ typ input ->
        typeOf (convertTo typ input) === typ

performTest :: ConversionTest -> Property
performTest test = directly test === indirectly test

directly :: ConversionTest -> TestString
directly (ConversionTest input conversions) = case lastMay conversions of
  Nothing -> input
  Just last -> convertTo last input

indirectly :: ConversionTest -> TestString
indirectly = \ case
  ConversionTest input [] -> input
  ConversionTest input (a : r) ->
    indirectly $ ConversionTest (convertTo a input) r

convertTo :: Type -> TestString -> TestString
convertTo typ input = case typ of
  StringType -> case input of
    String s -> String $ cs s
    StrictText s -> String $ cs s
    LazyText s -> String $ cs s
    StrictByteString s -> String $ cs s
    LazyByteString s -> String $ cs s
  StrictTextType -> case input of
    String s -> StrictText $ cs s
    StrictText s -> StrictText $ cs s
    LazyText s -> StrictText $ cs s
    StrictByteString s -> StrictText $ cs s
    LazyByteString s -> StrictText $ cs s
  LazyTextType -> case input of
    String s -> LazyText $ cs s
    StrictText s -> LazyText $ cs s
    LazyText s -> LazyText $ cs s
    StrictByteString s -> LazyText $ cs s
    LazyByteString s -> LazyText $ cs s
  StrictByteStringType -> case input of
    String s -> StrictByteString $ cs s
    StrictText s -> StrictByteString $ cs s
    LazyText s -> StrictByteString $ cs s
    StrictByteString s -> StrictByteString $ cs s
    LazyByteString s -> StrictByteString $ cs s
  LazyByteStringType -> case input of
    String s -> LazyByteString $ cs s
    StrictText s -> LazyByteString $ cs s
    LazyText s -> LazyByteString $ cs s
    StrictByteString s -> LazyByteString $ cs s
    LazyByteString s -> LazyByteString $ cs s

data ConversionTest
  = ConversionTest {
    input :: TestString,
    conversions :: [Type]
  }
  deriving (Show)

instance Arbitrary ConversionTest where
  arbitrary = ConversionTest <$> arbitrary <*> arbitrary
  shrink (ConversionTest input conversions) =
    map (\ new -> ConversionTest new conversions) (shrink input) ++
    map (ConversionTest input) (shrink conversions)

data TestString
  = String String
  | StrictText ST
  | LazyText LT
  | StrictByteString SBS
  | LazyByteString LBS
  deriving (Show, Eq)

instance Arbitrary TestString where
  arbitrary = oneof $
    (String <$> arbitrary) :
    (StrictText <$> arbitrary) :
    (LazyText <$> arbitrary) :
    (StrictByteString <$> arbitrary) :
    (LazyByteString <$> arbitrary) :
    []
  shrink = \ case
    String s -> map String $ shrink s
    StrictText s -> map StrictText $ shrink s
    LazyText s -> map LazyText $ shrink s
    StrictByteString s -> map StrictByteString $ shrink s
    LazyByteString s -> map LazyByteString $ shrink s

data Type
  = StringType
  | StrictTextType
  | LazyTextType
  | StrictByteStringType
  | LazyByteStringType
  deriving (Show, Eq, Bounded, Enum)

instance Arbitrary Type where
  arbitrary = elements [minBound .. maxBound]

typeOf :: TestString -> Type
typeOf = \ case
  String _ -> StringType
  StrictText _ -> StrictTextType
  LazyText _ -> LazyTextType
  StrictByteString _ -> StrictByteStringType
  LazyByteString _ -> LazyByteStringType
