module Foreign.C.Structs.Types (
    Struct2(..), Struct3(..), Struct4(..)
) where

import Foreign.Storable (Storable, peek, poke, alignment, sizeOf)
import Foreign.Ptr (castPtr)

import Foreign.C.Structs.Utils

-- | A @Struct2@ can hold two records of any @Storable@ types @a@ and @b@.
-- It is itself an instance of @Storable@ and can be used inside a @Foreign.Ptr@.
data Struct2 a b = Struct2 -- ^ Constructor for structs with two fields. Both arguments must be instances of @Storable@.
    { s2fst :: a -- ^ Accesses the first field of a @Struct2@
    , s2snd :: b -- ^ Accesses the second field of a @Struct2@
    } deriving (Show, Eq)

s2Size :: (Storable a, Storable b) => Struct2 a b -> Int
s2Size = (2*) . s2Alignment

s2Alignment :: (Storable a, Storable b) => Struct2 a b -> Int
s2Alignment cs = fmax [alignment $ s2fst cs, alignment $ s2snd cs]

instance (Storable a, Storable b) => Storable (Struct2 a b) where
    sizeOf    = s2Size
    alignment = s2Alignment
    peek ptr  = do
        a <- peek (castPtr ptr)
        b <- peek =<< next ptr a
        return $ Struct2 a b
    poke ptr (Struct2 a b) = do
        poke (castPtr ptr) a
        ptr_b <- next ptr a
        poke ptr_b b

-- | A @Struct3@ can hold three records of any @Storable@ types @a@, @b@ and @c@.
-- It is itself an instance of @Storable@ and can be used inside a @Foreign.Ptr@.
data Struct3 a b c = Struct3 -- ^ Constructor for structs with three fields. All three arguments must be instances of @Storable@.
    { s3fst :: a -- ^ Accesses the first field of a @Struct3@
    , s3snd :: b -- ^ Accesses the second field of a @Struct3@
    , s3trd :: c -- ^ Accesses the third field of a @Struct3@
    } deriving (Show, Eq)

s3Size :: (Storable a, Storable b, Storable c) => Struct3 a b c -> Int
s3Size cs
    |  sum_bc <= amax = 2 * amax
    |  sum_ab <= amax = 2 * amax
    |  otherwise      = 3 * amax
    where amax = s3Alignment cs
          size_a  = sizeOf $ s3fst cs
          size_b  = sizeOf $ s3snd cs
          size_c  = sizeOf $ s3trd cs
          sum_ab  = size_a + size_b
          sum_bc  = size_b + size_c

s3Alignment :: (Storable a, Storable b, Storable c) => Struct3 a b c -> Int
s3Alignment cs = fmax [a,b,c]
    where a = alignment $ s3fst cs
          b = alignment $ s3snd cs
          c = alignment $ s3trd cs

instance (Storable a, Storable b, Storable c) => Storable (Struct3 a b c) where
    sizeOf    = s3Size
    alignment = s3Alignment
    peek ptr  = do
        a <- peek (castPtr ptr)
        ptr_b <- next ptr a
        b <- peek ptr_b
        ptr_c <- next ptr_b b
        c <- peek ptr_c
        return $ Struct3 a b c
    poke ptr (Struct3 a b c) = do
        poke (castPtr ptr) a
        ptr_b <- next ptr a
        poke ptr_b b
        ptr_c <- next ptr_b b
        poke ptr_c c

-- | A @Struct4@ can hold four records of any @Storable@ types @a@, @b@, @c@ and @d@.
-- It is itself an instance of @Storable@ and can be used inside a @Foreign.Ptr@.
data Struct4 a b c d = Struct4 -- ^ Constructor for structs with four fields. All four arguments must be instances of @Storable@.
    { s4fst :: a -- ^ Accesses the first field of a @Struct4@
    , s4snd :: b -- ^ Accesses the second field of a @Struct4@
    , s4trd :: c -- ^ Accesses the third field of a @Struct4@
    , s4fth :: d -- ^ Accesses the fourth field of a @Struct4@
    } deriving (Show, Eq)

s4Size :: (Storable a, Storable b, Storable c, Storable d) => Struct4 a b c d -> Int
s4Size cs
    |  sum_abc <= amax
    || sum_bcd <= amax = 2 * amax
    |  sum_ab  <= amax
    && size_d  <= amax = 3 * amax
    |  size_a  <= amax
    && sum_cd  <= amax = 3 * amax
    |  sum_bc  <= amax = 3 * amax
    |  otherwise       = 4 * amax
    where amax    = s4Alignment cs
          size_a  = sizeOf $ s4fst cs
          size_b  = sizeOf $ s4snd cs
          size_c  = sizeOf $ s4trd cs
          size_d  = sizeOf $ s4fth cs
          sum_ab  = size_a + size_b
          sum_bc  = size_b + size_c
          sum_cd  = size_c + size_d
          sum_abc = sum_ab + size_c
          sum_bcd = size_b + sum_cd

s4Alignment :: (Storable a, Storable b, Storable c, Storable d) => Struct4 a b c d -> Int
s4Alignment cs = fmax [a, b, c, d]
    where a = alignment $ s4fst cs
          b = alignment $ s4snd cs
          c = alignment $ s4trd cs
          d = alignment $ s4fth cs

instance (Storable a, Storable b, Storable c, Storable d) => Storable (Struct4 a b c d) where
    sizeOf    = s4Size
    alignment = s4Alignment
    peek ptr  = do
        a <- peek (castPtr ptr)
        ptr_b <- next ptr a
        b <- peek ptr_b
        ptr_c <- next ptr_b b
        c <- peek ptr_c
        ptr_d <- next ptr_c c
        d <- peek ptr_d
        return $ Struct4 a b c d
    poke ptr (Struct4 a b c d) = do
        poke (castPtr ptr) a
        ptr_b <- next ptr a
        poke ptr_b b
        ptr_c <- next ptr_b b
        poke ptr_c c
        ptr_d <- next ptr_c c
        poke ptr_d d
