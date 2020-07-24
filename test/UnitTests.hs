module UnitTests (
    tests
) where

import Test.Framework.Providers.API (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

import Foreign.C.Structs (Struct2(Struct2), Struct3(Struct3), Struct4(Struct4))
import Foreign.Storable (sizeOf)
import Foreign.C.Types (CInt, CChar)

tests = testGroup "UnitTests" [
      sizeOfStruct2
    , sizeOfStruct3
    , sizeOfStruct4
    ]

doubleChar = Struct2 0.5 1 :: Struct2 Double CChar
charDouble = Struct2 1 0.5 :: Struct2 CChar  Double
intInt     = Struct2 63 63 :: Struct2 CInt   CInt

sizeOfStruct2 = testGroup "sizeOfStruct2" [
      testCase "DoubleChar" $ 16 @=? sizeOf doubleChar
    , testCase "CharDouble" $ 16 @=? sizeOf charDouble
    , testCase "IntInt"     $  8 @=? sizeOf intInt
    ]

intDoubleChar = Struct3 63 0.1 1 :: Struct3 CInt Double CChar
intCharDouble = Struct3 63 1 0.1 :: Struct3 CInt CChar Double
doubleIntChar = Struct3 0.1 63 1 :: Struct3 Double CInt CChar
doubleCharInt = Struct3 0.1 1 63 :: Struct3 Double CChar CInt
charIntDouble = Struct3 1 63 0.1 :: Struct3 CChar CInt Double
charDoubleInt = Struct3 1 0.1 63 :: Struct3 CChar Double CInt
doubleDoubleDouble = Struct3 0.1 0.1 0.1 :: Struct3 Double Double Double
intIntInt = Struct3 636363 636363 636363 :: Struct3 CInt CInt CInt
charCharChar = Struct3 1 1 1 :: Struct3 CChar CChar CChar

sizeOfStruct3 = testGroup "sizeOfStruct3" [
      testCase "intDoubleChar" $ 24 @=? sizeOf intDoubleChar
    , testCase "intCharDouble" $ 16 @=? sizeOf intCharDouble
    , testCase "doubleIntChar" $ 16 @=? sizeOf doubleIntChar
    , testCase "doubleCharInt" $ 16 @=? sizeOf doubleCharInt
    , testCase "charIntDouble" $ 16 @=? sizeOf charIntDouble
    , testCase "charDoubleInt" $ 24 @=? sizeOf charDoubleInt
    , testCase "doubleDoubleDouble" $ 24 @=? sizeOf doubleDoubleDouble
    , testCase "intIntInt" $ 12 @=? sizeOf intIntInt
    , testCase "charCharChar" $ 3 @=? sizeOf charCharChar
    ]

intIntDoubleChar  = Struct4 63 63 0.1 1 :: Struct4 CInt CInt Double CChar
charIntCharDouble = Struct4 1  63 1 0.1 :: Struct4 CChar CInt CChar Double
intDoubleIntChar  = Struct4 63 0.1 63 1 :: Struct4 CInt Double CInt CChar
doubleDoubleCharInt = Struct4 0.1 0.1 1 63 :: Struct4 Double Double CChar CInt
charIntDoubleChar = Struct4 1 63 0.1 1 :: Struct4 CChar CInt Double CChar
charDoubleIntDouble = Struct4 1 0.1 63 0.1 :: Struct4 CChar Double CInt Double
doubleDoubleDoubleDouble = Struct4 0.1 0.1 0.1 0.1 :: Struct4 Double Double Double Double
intIntIntInt = Struct4 636363 636363 636363 636363 :: Struct4 CInt CInt CInt CInt
charCharCharChar = Struct4 1 1 1 1 :: Struct4 CChar CChar CChar CChar

sizeOfStruct4 = testGroup "sizeOfStruct4" [
      testCase "intIntDoubleChar" $ 24 @=? sizeOf intIntDoubleChar
    , testCase "charIntCharDouble" $ 24 @=? sizeOf charIntCharDouble
    , testCase "intDoubleIntChar" $ 24 @=? sizeOf intDoubleIntChar
    , testCase "doubleDoubleCharInt" $ 24 @=? sizeOf doubleDoubleCharInt
    , testCase "charIntDoubleChar" $ 24 @=? sizeOf charIntDoubleChar
    , testCase "charDoubleIntDouble" $ 32 @=? sizeOf charDoubleIntDouble
    , testCase "doubleDoubleDoubleDouble" $ 32 @=? sizeOf doubleDoubleDoubleDouble
    , testCase "intIntIntInt" $ 16 @=? sizeOf intIntIntInt
    , testCase "charCharCharChar" $ 4 @=? sizeOf charCharCharChar
    ]

