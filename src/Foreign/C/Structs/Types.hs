{-# LANGUAGE TemplateHaskell #-}
module Foreign.C.Structs.Types (
    Struct2(..), Struct3(..), Struct4(..)
) where

import Foreign.Storable (Storable, peek, poke, alignment, sizeOf)
import Foreign.Ptr (Ptr, castPtr)

import Foreign.C.Structs.Utils
import Foreign.C.Structs.Templates (structT)

structT 2
structT 3
structT 4

