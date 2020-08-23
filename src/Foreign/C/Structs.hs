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

Currently up to six records are supported. Each number of records needs its own type.
The types are named after the number of records they support: 'Struct2', 'Struct3' .. @StructN@

If a Struct type with more fields is required, it can be created using Template Haskell and the 'structT' function:

> structT 8 -- creates a Struct with 8 fields

Field access is provided threefold:

 * Record syntax

> 2nd :: Struct2 a b -> b

 * Pattern matching

> (Struct2 a b)

 * Template Haskell 'acs' function.

> $(acs 2 2) :: Struct2 a b -> b

-}
module Foreign.C.Structs (
      Struct2(..)
    , Struct3(..)
    , Struct4(..)
    , Struct5(..)
    , Struct6(..)
    , structT
    , acs
    -- Exports for Template Haskell usage
    , next, sizeof, fmax
    ) where

import Foreign.C.Structs.Types (
      Struct2(..)
    , Struct3(..)
    , Struct4(..)
    , Struct5(..)
    , Struct6(..)
    )

import Foreign.C.Structs.Templates (
      structT
    , acs
    )

import Foreign.C.Structs.Utils (
      next
    , sizeof
    , fmax
    )

