module Properties where

import Test.QuickCheck (Property, Arbitrary, arbitrary)
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.Framework.Providers.API (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Foreign.C.Types (CChar, CInt, CDouble)
import Foreign.Storable (Storable, peek)
import Foreign.Marshal.Utils (new)
import Foreign.Marshal.Alloc (free)
import Foreign.C.Structs (Struct2(..), Struct3(..), Struct4(..))

tests = testGroup "Properties" [test_idStruct2, test_idStruct3, test_idStruct4]

test_idStruct2 = testGroup "Identity Struct2" [
      testProperty "Double Double"  (prop_idStruct2 :: Struct2 Double Double -> Property)
    , testProperty "Int Int"        (prop_idStruct2 :: Struct2 Int Int       -> Property)
    , testProperty "Int CInt"       (prop_idStruct2 :: Struct2 Int CInt      -> Property)
    , testProperty "CChar CInt"     (prop_idStruct2 :: Struct2 CChar CInt    -> Property)
    ]

test_idStruct3 = testGroup "Identity Struct3" [
      testProperty "Double Int CChar" (prop_idStruct3 :: Struct3 Double Int CChar -> Property)
    , testProperty "CChar Double Int" (prop_idStruct3 :: Struct3 CChar Double Int -> Property)
    , testProperty "Int Double CChar" (prop_idStruct3 :: Struct3 Int Double CChar -> Property)
    , testProperty "CChar CChar CChar" (prop_idStruct3 :: Struct3 CChar CChar CChar -> Property)
    ]

test_idStruct4 = testGroup "Identity Struct4" [
      testProperty "Int Double Int CChar" (prop_idStruct4 :: Struct4 Int Double Int CChar -> Property)
    , testProperty "Int Int Double CChar" (prop_idStruct4 :: Struct4 Int Int Double CChar -> Property)
    , testProperty "Int Double Int Double" (prop_idStruct4 :: Struct4 Int Double Int Double -> Property)
    , testProperty "Double Int Char Char" (prop_idStruct4 :: Struct4 Double Int Char Char -> Property)
    , testProperty "CChar Int CChar Double" (prop_idStruct4 :: Struct4 CChar Int CChar Double -> Property)
    , testProperty "Double Double CChar CInt" (prop_idStruct4 :: Struct4 Double Double CChar CInt -> Property)
    , testProperty "CChar CInt Double CChar" (prop_idStruct4 :: Struct4 CChar CInt Double CChar -> Property)
    , testProperty "CChar Double CInt Double" (prop_idStruct4 :: Struct4 CChar Double CInt Double -> Property)
    , testProperty "CChar CChar CChar CChar" (prop_idStruct4 :: Struct4 CChar CChar CChar CChar -> Property)
    ]

instance ( Storable a, Arbitrary a
         , Storable b, Arbitrary b
         ) => Arbitrary (Struct2 a b) where
    arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return $ Struct2 a b

instance ( Storable a, Arbitrary a
         , Storable b, Arbitrary b
         , Storable c, Arbitrary c
         ) => Arbitrary (Struct3 a b c) where
    arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return $ Struct3 a b c

instance ( Storable a, Arbitrary a
         , Storable b, Arbitrary b
         , Storable c, Arbitrary c
         , Storable d, Arbitrary d
         ) => Arbitrary (Struct4 a b c d) where
    arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return $ Struct4 a b c d

identity2 struct = do
    ptr_struct <- new struct
    struct'    <- peek ptr_struct
    free ptr_struct
    return struct'

prop_idStruct2 :: (Storable a, Storable b, Eq a, Eq b) => Struct2 a b -> Property
prop_idStruct2 struct = monadicIO $ do
    struct' <- run (identity2 struct)
    assert (struct' == struct)

identity3 struct = do
    ptr_struct <- new struct
    struct'    <- peek ptr_struct
    free ptr_struct
    return struct'

prop_idStruct3 :: (Storable a, Storable b, Storable c, Eq a, Eq b, Eq c) => Struct3 a b c -> Property
prop_idStruct3 struct = monadicIO $ do
    struct' <- run (identity3 struct)
    assert (struct' == struct)

identity4 struct = do
    ptr_struct <- new struct
    struct'    <- peek ptr_struct
    free ptr_struct
    return struct'

prop_idStruct4 :: ( Storable a, Storable b, Storable c, Storable d
                  , Eq a, Eq b, Eq c, Eq d
                  ) => Struct4 a b c d -> Property
prop_idStruct4 struct = monadicIO $ do
    struct' <- run (identity4 struct)
    assert (struct' == struct)

