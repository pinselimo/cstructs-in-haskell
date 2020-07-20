# C-Structs in Haskell

This repository ispart of the development of [```Hasky```](https://github.com/pinselimo/Hasky/). It defines types for ```C``` ```struct```s in ```Haskell``` and their instance declarations as Storable.

## Usage

You can use these types as a classic ```hackage``` package. It has no other dependencies than some of the ```Foreign.*``` modules contained in the standard library of ```GHC```.

~~~haskell
λ> import Foreign.C.Structs
λ> s = Struct2 1 2 :: Struct2 Int Int
~~~

For a more elaborated usage example checkout [```Hasky```](https://github.com/pinselimo/Hasky/hasky/haskell/res/HaskyTuple.hs/). It uses ```Foreign.C.Structs``` to declare its storage functions for ```Haskell``` tuples.

## Testing

[![Build](https://img.shields.io/travis/pinselimo/cstructs-in-haskell.svg)](https://travis-ci.org/pinselimo/cstructs-in-haskell)

Identity properties are tested with QuickCheck to ensure that peek and poke are reversible. The result of ```sizeOf``` is dependent on the order of types. Its correctness can only be tested with HUnit. The ```alignment``` function is trivial and only tested implicitly through ```sizeOf```.
All tests are performed for all available GHC versions through [haskell-ci](https://github.com/haskell-CI/haskell-ci) to ensure maximum compatibility.

## Contributing

Currently only structs with up to four fields are supported. If your use case demands more, feel free to contribute or raise an issue on GitHub. Due to the individuality of the ```sizeOf``` function, an implementation in Template Haskell currently doesn't seem feasible. Thus, instances have to be written one by one.

## License

This part of Hasky is licensed under the ```MIT``` License. Please be aware that the full ```Hasky``` package is under ```LGPLv3```. Refer to the accompanying LICENSE or COPYING files for details.

> (c) 2020 Simon Plakolb

