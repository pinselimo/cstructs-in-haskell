module UnitTests where

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Foreign.C.Structs
import Foreign.Storable
import Foreign.C.Types

tests = testGroup "sizeOfStruct2" [
    testCase "DoubleChar" $ 16 @?= sizeOf $ Struct2 0.5 1 :: Struct2 CDouble CChar
]
