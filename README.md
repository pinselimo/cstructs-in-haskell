# C-Structs in Haskell

This repository ispart of the development of [```Hasky```](https://github.com/pinselimo/Hasky/). It defines types for ```C``` ```struct```s in ```Haskell``` and their instance declarations as Storable.

## Usage

You can use these types as a classic ```hackage``` package. It has no other dependencies than some the ```Foreign.*``` contained in the standard library of ```GHC```.

~~~haskell
λ> import Foreign.C.Structs
λ> s = Struct2 1 2 :: Struct2 Int Int
~~~

For a more elaborated usage example checkout [```Hasky```](https://github.com/pinselimo/Hasky/hasky/haskell/res/HaskyTuple.hs/). It uses ```Foreign.C.Structs``` to declare its storage functions for ```Haskell``` tuples.

## License

This part of Hasky is licensed under the ```MIT``` License. Please be aware that the full ```Hasky``` package is under ```LGPLv3```. Refer to the accompanying LICENSE or COPYING files for details.
