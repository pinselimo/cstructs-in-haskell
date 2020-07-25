{- |
Module          : Foreign.C.Structs
Description     : Create C structs from Haskell
Copyright       : (c) Simon Plakolb, 2020
License         : MIT
Maintainer      : s.plakolb@gmail.com
Stability       : beta

The @Foreign.C.Structs@ module allows you to construct C structs of arbitrary @Storable@ types.
It also defined them as instances of the Storable type-class. You can thus create pointers
to an instance of such a struct and interface with another language.

Currently up to four records are supported. Each number of records needs its own type.
The types are named after the number of records they support: 'Struct2', 'Struct3', 'Struct4'.
If you'd like to request a type with more records, feel free to issue a PR or contact the maintainer.
-}
module Foreign.C.Structs (
      Struct2(..)
    , Struct3(..)
    , Struct4(..)
    , Struct5(..)
    , Struct6(..)
    , StructT
    -- Exports for Template Haskell usage
    , next, sizeof, fmax
    -- Reexports for Template Haskell
    , Storable, peek, poke, sizeOf, alignment, castPtr
    ) where

import Foreign.C.Structs.Types (
     Struct2(..)
    ,Struct3(..)
    ,Struct4(..)
    )

import Foreign.C.Structs.Template (
     StructT
    )

import Foreign.Storable (
     Storable, peek, poke, sizeOf, alignment
    )
import Foreign.Ptr (
     castPtr
    )
import Foreign.C.Structs.Utils (
     next
    ,sizeof
    ,fmax
    )

