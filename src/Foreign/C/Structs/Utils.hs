module Foreign.C.Structs.Utils (
      next, fmax, sizeof
) where

import Foreign.Storable (Storable, peek, sizeOf, alignment)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, plusPtr, alignPtr)

sizeof :: [Int] -> [Int] -> Int
sizeof alignments sizes = sizeof' 0 alignments sizes
    where
          sizeof' 0 (a:as) (s:ss) = sizeof' s as ss
          sizeof' s [] [] = s `pad` foldr max 0 alignments
          sizeof' x (a:as) (s:ss) = let
                            s' = x+s
                            in sizeof' (s' `pad` a) as ss

pad x a
  | x `mod` a == 0 = x
  | otherwise      = pad (x+1) a

next :: (Storable a, Storable b, Storable c) => Ptr a -> b -> IO (Ptr c)
next ptr x = alloca $ next' ptr x
    where next' :: (Storable a, Storable b, Storable c) => Ptr a -> b -> Ptr c -> IO (Ptr c)
          next' ptr x ptr_x = do
                let ptr_y = plusPtr ptr $ sizeOf x
                y <- peek ptr_x
                return $ alignPtr ptr_y $ alignment y

fmax :: Integral a => [a] -> a
fmax = foldr max 0
