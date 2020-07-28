{-# LANGUAGE TemplateHaskell #-}
module UnitTests (
    tests
) where

import Test.Framework.Providers.API (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

import Foreign.C.Structs (Struct2(Struct2), Struct3(Struct3), Struct4(Struct4), structT)
import Foreign.Storable (sizeOf)
import Foreign.C.Types (CInt, CChar, CFloat, CDouble)

i = i :: CInt
d = d :: CDouble
f = 0.2 :: CFloat
c = 8 :: CChar

sizeOfStruct2 = testGroup "sizeOfStruct2" [
      testCase "DoubleChar" $ 16 @=? (sizeOf $ Struct2 d c)
    , testCase "CharDouble" $ 16 @=? (sizeOf $ Struct2 c d)
    , testCase "IntInt"     $  8 @=? (sizeOf $ Struct2 i i)
    ]

sizeOfStruct3 = testGroup "sizeOfStruct3" [
      testCase "intDoubleChar" $ 24 @=? (sizeOf $ Struct3 i d c)
    , testCase "intCharDouble" $ 16 @=? (sizeOf $ Struct3 i c d)
    , testCase "doubleIntChar" $ 16 @=? (sizeOf $ Struct3 d i c)
    , testCase "doubleCharInt" $ 16 @=? (sizeOf $ Struct3 d c i)
    , testCase "charIntDouble" $ 16 @=? (sizeOf $ Struct3 c i d)
    , testCase "charDoubleInt" $ 24 @=? (sizeOf $ Struct3 c d i)
    , testCase "doubleDoubleDouble" $ 24 @=? (sizeOf $ Struct3 d d d)
    , testCase "intIntInt" $ 12 @=? (sizeOf $ Struct3 i i i)
    , testCase "charCharChar" $ 3 @=? (sizeOf $ Struct3 c c c)
    ]

sizeOfStruct4 = testGroup "sizeOfStruct4" [
      testCase "intIntDoubleChar" $ 24 @=? (sizeOf $ Struct4 i i d c)
    , testCase "charIntCharDouble" $ 24 @=? (sizeOf $ Struct4 c i c d)
    , testCase "intDoubleIntChar" $ 24 @=? (sizeOf $ Struct4 i d i c)
    , testCase "doubleDoubleCharInt" $ 24 @=? (sizeOf $ Struct4 d d c i)
    , testCase "charIntDoubleChar" $ 24 @=? (sizeOf $ Struct4 c i d c)
    , testCase "charDoubleIntDouble" $ 32 @=? (sizeOf $ Struct4 c d i d)
    , testCase "doubleDoubleDoubleDouble" $ 32 @=? (sizeOf $ Struct4 d d d d)
    , testCase "intIntIntInt" $ 16 @=? (sizeOf $ Struct4 i i i i)
    , testCase "charCharCharChar" $ 4 @=? (sizeOf $ Struct4 c c c c)
    ]

structT 8

sizeOfStruct8 = testGroup "sizeOfStruct8" [
      testCase "s8ccccdcci" $ 24 @=? (sizeOf $ Struct8 c c c c d c c i)
    , testCase "s8ccccdcic" $ 32 @=? (sizeOf $ Struct8 c c c c d c i c)
    ]

tests = testGroup "UnitTests" [
      sizeOfStruct2
    , sizeOfStruct3
    , sizeOfStruct4
    , sizeOfStruct8
    ]

