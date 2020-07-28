{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module CTest where

import Test.Framework.Providers.API (testGroup)
import Templates

import Foreign.C.Types
import Foreign.C.Structs

i = 63 :: CInt
d = 63.63 :: CDouble
f = 42.42 :: CFloat
ch = 1 :: CChar

s2id = Struct2 i d
s2ii = Struct2 i i
s2df = Struct2 d f

s3icd = Struct3 i ch d
s3idc = Struct3 i d ch

s4icdd = Struct4 i ch d d
s4iddi = Struct4 i d  d i
s4dicc = Struct4 d i ch ch

c "sIntDouble"   's2id
c "sIntInt"      's2ii
c "sDoubleFloat" 's2df

c "sIntCharDouble" 's3icd
c "sIntDoubleChar" 's3idc

c "sIntCharDoubleDouble" 's4icdd
c "sIntDoubleDoubleInt"  's4iddi
c "sDoubleIntCharChar"   's4dicc

tests = testGroup "ForeignImports" [
      case_sIntDouble, case_sIntInt, case_sDoubleFloat
    , case_sIntCharDouble, case_sIntDoubleChar
    , case_sIntCharDoubleDouble, case_sIntDoubleDoubleInt, case_sDoubleIntCharChar
    ]
