module Foreign.C.Structs.Types (
    Struct2(..), Struct3(..)
) where

import Foreign.Storable (Storable, peek, poke, alignment, sizeOf)
import Foreign.Ptr (castPtr)

import Foreign.C.Structs.Utils

data Struct2 a b = Struct2
    { s2fst :: a
    , s2snd :: b
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

data Struct3 a b c = Struct3
    { s3fst :: a
    , s3snd :: b
    , s3trd :: c
    } deriving (Show, Eq)

s3Size :: (Storable a, Storable b, Storable c) => Struct3 a b c -> Int
s3Size cs
    | align_a >= sum_bc  = 2 * align_a
    | align_b >= size_a
    && align_b >= size_c = 3 * align_b
    | align_c >= sum_ab  = 2 * align_c
    | otherwise          = size_a + size_b + size_c -- Fallback
    where align_a = alignment $ s3fst cs
          align_b = alignment $ s3snd cs
          align_c = alignment $ s3trd cs
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
        