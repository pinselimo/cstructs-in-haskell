module Properties where

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.Framework.Providers.API (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Foreign.Storable (Storable, peek)
import Foreign.Marshal.Utils (new)
import Foreign.Marshal.Alloc (free)
import Foreign.C.Structs (Struct2(..), Struct3(..), Struct4(..))

tests = testGroup "Properties" [
      testProperty prop_idStruct2
    , testProperty prop_idStruct3
    , testProperty prop_idStruct4
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

