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
The types are named after the number of records they support: @Struct2@, @Struct3@, @Struct4@.
If you'd like to request a type with more records, feel free to issue a PR or contact the maintainer.
-}
module Foreign.C.Structs (
      Struct2( Struct2, s2fst, s2snd)
    , Struct3( Struct3, s3fst, s3snd, s3trd)
    , Struct4( Struct4, s4fst, s4snd, s4trd, s4fth)
    ) where

import Foreign.C.Structs.Types (
     Struct2(..)
    ,Struct3(..)
    ,Struct4(..)
    )

