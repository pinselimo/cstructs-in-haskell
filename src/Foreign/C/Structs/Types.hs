{-# LANGUAGE TemplateHaskell #-}
{- |
Module          : Foreign.C.Structs.Types
Description     : Create C structs from Haskell
Copyright       : (c) Simon Plakolb, 2020
License         : MIT
Maintainer      : s.plakolb@gmail.com
Stability       : beta

This module creates tuples with up to 6 fields.
-}
module Foreign.C.Structs.Types (
    Struct2(..), Struct3(..), Struct4(..), Struct5(..), Struct6(..)
) where

import Foreign.Storable (Storable, peek, poke, alignment, sizeOf)
import Foreign.Ptr (Ptr, castPtr)

import Foreign.C.Structs.Utils (next, fmax, sizeof)
import Foreign.C.Structs.Templates (structT)

-- | A 'Struct2' can hold two records of any 'Storable' types @a@ and @b@.
-- It is itself an instance of 'Storable' and can be used inside a 'Ptr'.
-- The 'Struct2' constructor takes two arguments.
-- The record functions 's21st' and 's22nd' provide access to the fields values.
structT 2
-- | A 'Struct3' can hold three records of any 'Storable' types @a@, @b@ and @c@.
-- It is itself an instance of 'Storable' and can be used inside a 'Ptr'.
-- The 'Struct3' constructor takes three arguments.
-- The record functions 's31st', 's32nd' and 's33rd' provide access to the fields values.
structT 3
-- | A 'Struct4' can hold four records of any 'Storable' types @a@, @b@, @c@ and @d@.
-- It is itself an instance of 'Storable' and can be used inside a 'Ptr'.
-- The 'Struct4' constructor takes four arguments.
-- The record functions 's41st', 's42nd', 's43rd' and 's44th' provide access to the fields values.
structT 4
-- | A 'Struct5' can hold five records of any 'Storable' types @a@, @b@, @c@, @d@ and @e@.
-- It is itself an instance of 'Storable' and can be used inside a 'Ptr'.
-- The 'Struct5' constructor takes five arguments.
-- The record functions 's51st', 's52nd', 's53rd', 's54th' and 's55th' provide access to the fields values.
structT 5
-- | A 'Struct6' can hold six records of any 'Storable' types @a@, @b@, @c@, @d@, @e@ and @f@.
-- It is itself an instance of 'Storable' and can be used inside a 'Ptr'.
-- The 'Struct6' constructor takes six arguments.
-- The record functions 's61st', 's62nd', 's63rd', 's64th', 's65th' and 's66th' provide access to the fields values.
structT 6

