{-# LANGUAGE TemplateHaskell #-}
module CTestTemplate where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (free)
import Foreign.Storable (peek)
import Test.HUnit ((@?=))
import Test.Framework.Providers.HUnit (testCase)

peek' ptr = do
      val <- peek ptr
      free ptr
      return val

c :: String -> Name -> DecsQ
c cname res = do
    VarI n t d <- reify res
    typ <- ptr $ return t
    let name = mkName cname
        deq  = ForeignD $ ImportF CCall Safe cname name typ
        body = normalB [| testCase cname $ (peek' $(varE name)) >>= (@?= $(varE n)) |]
        test = funD (mkName $ "case_" ++ cname) [clause [] body []]
    sequence [return deq, test]

ptr t = [t| Ptr $(t) |]

to_struct_type :: [TypeQ] -> TypeQ
to_struct_type ts = [t| Ptr $(mk_struct) |]
    where struct = conT (mkName $ "Struct" ++ (show $ length ts))
          mk_struct :: TypeQ
          mk_struct = foldr appT struct ts


cint = conT (mkName "Struct CInt CInt")
cdouble = conT (mkName "CDouble")

