{-# LANGUAGE ForeignFunctionInterface #-}
module CTest where

import Test.Framework.Providers.API (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

import System.IO.Unsafe (unsafePerformIO)

import Foreign.C.Types
import Foreign.C.Structs
import Foreign.Ptr
import Foreign.Storable

quick = unsafePerformIO

foreign import ccall "sIntDouble" sIntDouble :: Ptr (Struct2 CInt CDouble)
foreign import ccall "sIntInt" sIntInt :: Ptr (Struct2 CInt CInt)
foreign import ccall "sDoubleFloat" sDoubleFloat :: Ptr (Struct2 CDouble CFloat)
foreign import ccall "sIntCharDouble" sIntCharDouble :: Ptr (Struct3 CInt CChar CDouble)
foreign import ccall "sIntDoubleChar" sIntDoubleChar :: Ptr (Struct3 CInt CDouble CChar)
foreign import ccall "sIntCharDoubleDouble" sIntCharDoubleDouble :: Ptr (Struct4 CInt CChar CDouble CDouble)
foreign import ccall "sIntDoubleDoubleInt" sIntDoubleDoubleInt :: Ptr (Struct4 CInt CDouble CDouble CInt)
foreign import ccall "sDoubleIntCharChar" sDoubleIntCharChar :: Ptr (Struct4 CDouble CInt CChar CChar)

tests = testGroup "Foreign Imports" [
        testCase "sIntDouble" $ (quick $ peek sIntDouble) @?= Struct2 63 63.63,
        testCase "sIntInt" $ (quick $ peek sIntInt) @?= Struct2 63 63,
        testCase "sDoubleFloat" $ (quick $ peek sDoubleFloat) @?= Struct2 63.63 42.42,
        testCase "sIntCharDouble" $ (quick $ peek sIntCharDouble) @?= Struct3 63 1 63.63,
        testCase "sIntDoubleChar" $ (quick $ peek sIntDoubleChar) @?= Struct3 63 63.63 1,
        testCase "sIntCharDoubleDouble" $ (quick $ peek sIntCharDoubleDouble) @?= Struct4 63 1 63.63 63.63,
        testCase "sIntDoubleDoubleInt" $ (quick $ peek sIntDoubleDoubleInt) @?= Struct4 63 63.63 63.63 63,
        testCase "sDoubleIntCharChar" $ (quick $ peek sDoubleIntCharChar) @?= Struct4 63.63 63 1 1
    ]
