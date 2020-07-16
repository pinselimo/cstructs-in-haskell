module Foreign.C.Structs.Utils (
      next, fmax
) where

import Foreign.Storable (peek, sizeOf, alignment)
import Foreign.Marshal (alloca)
import Foreign.Ptr (plusPtr, alignPtr)

next :: (Storable a, Storable b, Storable c) => Ptr a -> b -> IO (Ptr c)
next ptr x = alloca $ next' ptr x
    where next' :: (Storable a, Storable b, Storable c) => Ptr a -> b -> Ptr c -> IO (Ptr c)
          next' ptr x ptr_x = do
                let ptr_y = plusPtr ptr $ sizeOf x
                y <- peek ptr_x
                return $ alignPtr ptr_y $ alignment y

fmax = Integral a => [a] -> a
fmax = foldr max 0
