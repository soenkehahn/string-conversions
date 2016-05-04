{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Data.String.ConversionsSpec where

import           Control.DeepSeq
import           Data.List
import           Data.Proxy
import           Data.Typeable
import           Data.Typeable.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Data.String.Conversions
import           Data.String.Conversions.Monomorphic

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

{-
    context "SBS -> String" $ do
      it "has the same behavior as SBS -> ST (even on decoding failures)" $ do
        property $ \ (sbs :: SBS) ->
          counterexample (toString sbs) $
          counterexample (toString $ toStrictText sbs) $
          toString sbs === toString (toStrictText sbs)
-}

    modifyMaxSize (const 100) $ do
      it "" $ do
        property $ \ x -> execute x

execute :: ConversionTest -> Property
execute ct@(ConversionTest w) = ioProperty $ do
  print ct
  return $ property $ \ input ->
    applyDirectly w input === applyIndirectly w input

data ConversionTest where
  ConversionTest :: (Conv from, Conv to) =>
    WithDirect from to -> ConversionTest

instance Show ConversionTest where
  show (ConversionTest withDirect) = show withDirect

instance Arbitrary ConversionTest where
  arbitrary = do
    Positive n <- arbitrary
    arbitraryConversionTest n

arbitraryConversionTest :: Int -> Gen ConversionTest
arbitraryConversionTest = \ case
  n | n <= 0 -> oneof $
    pure (ConversionTest $ WithDirect (cs :: String -> String) M) :
    pure (ConversionTest $ WithDirect (cs :: ST -> ST) M) :
    pure (ConversionTest $ WithDirect (cs :: SBS -> SBS) M) :
    []
  n -> do
    (ConversionTest (WithDirect direct indirect)) <- arbitraryConversionTest (pred n)
    oneof $
      return (ConversionTest $ WithDirect cs (WithDirect direct indirect :# withResultType (Proxy :: Proxy String) cs)) :
      return (ConversionTest $ WithDirect cs (WithDirect direct indirect :# withResultType (Proxy :: Proxy ST) cs)) :
      return (ConversionTest $ WithDirect cs (WithDirect direct indirect :# withResultType (Proxy :: Proxy SBS) cs)) :
      []
  where
    withResultType :: Proxy b -> (a -> b) -> (a -> b)
    withResultType _ = id

data a :# b where
  M :: a :# a
  (:#) ::
    (Conv a, Conv b, Conv c, Typeable (b -> c)) =>
    {start :: WithDirect a b, next :: (b -> c)} -> (a :# c)

type Conv a =
  (Eq a, Show a, Typeable a, Arbitrary a,
   ConvertibleStrings a String,
   ConvertibleStrings a ST,
   ConvertibleStrings a SBS)

data WithDirect a b where
  WithDirect ::
    (Conv a, Conv b) => {
      direct :: a -> b,
      indirect :: a :# b
    } -> WithDirect a b

instance Typeable a => Show (a :# b) where
  show M = showType (Proxy :: Proxy a)
  show (WithDirect _ start :# next) = show start ++ " :# " ++ showType (Proxy :: Proxy b)

showType :: Typeable t => Proxy t -> String
showType p = case cast p :: Maybe (Proxy String) of
  Just _ -> "String"
  Nothing -> case cast p :: Maybe (Proxy ST) of
    Just _ -> "ST"
    Nothing -> case cast p :: Maybe (Proxy SBS) of
      Just _ -> "SBS"
      Nothing -> show (typeRep p)

instance (Typeable from) => Show (WithDirect from to) where
  show (WithDirect _ indirect) = show indirect

applyIndirectly :: WithDirect a b -> (a -> b)
applyIndirectly (WithDirect _ M) a = a
applyIndirectly (WithDirect _ (start :# next)) a = next $ applyIndirectly start a

applyDirectly :: WithDirect a b -> (a -> b)
applyDirectly (WithDirect direct _) a = direct a
