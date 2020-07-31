{-# LANGUAGE TemplateHaskell, CPP #-}
module Templates where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (free)
import Foreign.Storable (peek, sizeOf)
import Test.HUnit ((@?=))
import Test.Framework.Providers.HUnit (testCase)

peek' ptr = do
      val <- peek ptr
      free ptr
      return val

c :: String -> Name -> DecsQ
c cname res = do
#if __GLASGOW_HASKELL__ < 800
    VarI n t _ _ <- reify res
#else
    VarI n t _ <- reify res
#endif
    typ <- ptr $ return t
    let name = mkName cname
        deq  = ForeignD $ ImportF CCall Safe cname name typ
        body = normalB [| testCase cname $ (peek' $(varE name)) >>= (@?= $(varE n)) |]
        test = funD (mkName $ "case_" ++ cname) [clause [] body []]
    sequence [return deq, test]

ptr t = [t| Ptr $(t) |]

size :: Name -> Int -> DecsQ
size uname res = let
                  sname = show uname
                  body = normalB [| testCase sname $ sizeOf $(varE uname) @?= res |]
                  clauses = [clause [] body []]
    in sequence [funD (mkName $ "case_" ++ sname) clauses]

to_struct_type :: [TypeQ] -> TypeQ
to_struct_type ts = [t| Ptr $(mk_struct) |]
    where struct = conT (mkName $ "Struct" ++ (show $ length ts))
          mk_struct :: TypeQ
          mk_struct = foldr appT struct ts

