{- |
Module          : Foreign.C.Structs.Utils
Description     : Create C structs from Haskell
Copyright       : (c) Simon Plakolb, 2020
License         : MIT
Maintainer      : s.plakolb@gmail.com
Stability       : beta

This module defined some utility functions for Storable instance declarations.
-}
module Foreign.C.Structs.Utils (
      next, fmax, sizeof
) where

import Foreign.Storable (Storable, peek, sizeOf, alignment)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, plusPtr, alignPtr)

-- | Due to alignment constraints the size of C structs is dependent on the order of fields and their respectible sizes. The function 'sizeof' can calculate the resulting size given a list of all 'alignments' and 'sizes'.
sizeof :: [Int] -> [Int] -> Int
sizeof as@(_:alignments) (s:sizes) = sizeof' s alignments sizes
    where
          sizeof' s [] [] = s `pad` fmax as
          sizeof' x (a:as) (s:ss) = let
                               s' = x+s
                               in sizeof' (s' `pad` a) as ss

          pad x a
              | x `mod` a == 0 = x
              | otherwise      = pad (x+1) a

-- | Jumps to the next pointer location in the struct.
next :: (Storable a, Storable b, Storable c) => Ptr a -> b -> IO (Ptr c)
next ptr x = alloca $ next' ptr x
    where
          next' :: (Storable a, Storable b, Storable c) => Ptr a -> b -> Ptr c -> IO (Ptr c)
          next' ptr x ptr_x = do
                let ptr_y = plusPtr ptr $ sizeOf x
                y <- peek ptr_x
                return $ alignPtr ptr_y $ alignment y

-- | Alias for @foldr max 0@.
fmax :: Integral a => [a] -> a
fmax = foldr max 0

