module Main where

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.Framework.Runners.Console (defaultMain)
import Test.HUnit

import Foreign.C.Structs
import Foreign.Storable
import Foreign.C.Types

main = defaultMain $ [sizeOfStruct2]

sizeOfStruct2 = testGroup "sizeOfStruct2" [
    testCase "DoubleChar" $ 16 @?= sizeOf $ Struct2 0.5 1 :: Struct2 CDouble CChar
]
