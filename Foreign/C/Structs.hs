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
    -- | A @Struct2@ can hold two records of any @Storable@ types @a@ and @b@.
    -- It is itself an instance of @Storable@ and can be used inside a @Foreign.Ptr@.
    Struct2(
        Struct2 -- ^ Constructor for structs with two fields. Both arguments must be instances of @Storable@.
    ,   s2fst -- ^ Accesses the first field of a @Struct2@
    ,   s2snd -- ^ Accesses the second field of a @Struct2@
    ),
    -- | A @Struct3@ can hold three records of any @Storable@ types @a@, @b@ and @c@.
    -- It is itself an instance of @Storable@ and can be used inside a @Foreign.Ptr@.
    Struct3(
        Struct3 -- ^ Constructor for structs with three fields. All three arguments must be instances of @Storable@.
    ,   s3fst -- ^ Accesses the first field of a @Struct3@
    ,   s3snd -- ^ Accesses the second field of a @Struct3@
    ,   s3trd -- ^ Accesses the third field of a @Struct3@
    )
) where

import Foreign.C.Structs.Types (
    Struct2(..),
    Struct3(..))
