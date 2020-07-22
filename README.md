# C-Structs in Haskell [![Build](https://secure.travis-ci.org/pinselimo/cstructs-in-haskell.svg)](https://travis-ci.org/pinselimo/cstructs-in-haskell) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/C-structs/badge)](https://matrix.hackage.haskell.org/package/C-structs) [![Hackage Version](https://img.shields.io/hackage/v/C-structs.svg?label=Hackage)](http://hackage.haskell.org/package/C-structs) [![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/C-structs.svg)](https://hackage.haskell.org/package/C-structs)

C-structs lets you create dynamically typed and correctly padded C structs in Haskell.
These can be used for FFI calls, imports and exports.
This package is part of the development efforts for the Python library [```Hasky```](https://github.com/pinselimo/Hasky/).
Hasky provides an interface to import Haskell modules.

Note: As of GHC 8.10 structs cannot be passed by value, [only by reference](https://wiki.haskell.org/Foreign_Function_Interface#Foreign_types).


## Usage

You can use these types as a classic ```hackage``` package.
The library has no other dependencies than some of the ```Foreign.*``` modules contained in ```base```.

### Basics

~~~haskell
λ> import Foreign.C.Structs
λ> s = Struct2 1 2 :: Struct2 Int Int
~~~

can be interpreted as an equivalent to:

~~~C
struct Struct2 {
    int s2fst;
    int s2snd;
};

struct Struct2 s;
s.s2fst = 1;
s.s2snd = 2;
~~~

or with Python's ```ctypes```:

~~~python
>>> from ctypes import Structure, c_int
>>> class Struct2( Structure ):
...     _fields_ = [("s2fst", c_int), ("s2snd", c_int)]
...
>>> s = Struct2(1,2)
~~~

On memory all of these examples should have the exact same representation.
A pointer to either ```s``` can then be exchanged with the other and used in a ```foreign``` call.

### FFI usage

The following shows an example of a foreign import of a ```struct Struct2``` as defined above:

~~~C
struct Struct2 *foo (void) {
    struct Struct2 *val;
    val = malloc (sizeof (struct Struct2));
    val->s2fst = 42;
    val->s2snd = 63;
    return val;
}
~~~

can be imported in a Haskell module as follows:

~~~haskell
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Foreign.Marshal.Alloc (free)
import Foreign.C.Types (CInt)
import Foreign.C.Structs (Struct2)

foreign import ccall "foo" foo :: Ptr (Struct2 CInt CInt)

main = do
    putStrLn "Reading values from C.."
    s <- peek foo
    free foo
    putStrLn "Received:"
    putStrLn $ show s
~~~

For a more elaborated usage examples checkout [```Hasky```](https://github.com/pinselimo/Hasky) in conjunction with [```Hasky-Types```](https://github.com/pinselimo/Hasky-Types).
It uses ```Foreign.C.Structs``` to declare its storage functions for ```Haskell``` tuples. In addition, its Array and Linked List instances are based on this library.

## Testing

Identity properties are tested with QuickCheck to ensure that peek and poke are reversible.
The result of ```sizeOf``` is dependent on the order of types. Its correctness can only be tested with HUnit.
The ```alignment``` function is trivial and only tested implicitly through ```sizeOf```.

Imports from C are tested in ```CTest.hs``` and together with the identity tests form the guarantee that also exports to C are consistent.
All tests are performed for all available GHC versions through [haskell-ci](https://github.com/haskell-CI/haskell-ci) to ensure maximum compatibility.

## Contributing

Currently only structs with up to four fields are supported. If your use case demands more, feel free to contribute or raise an issue on GitHub. Due to the individuality of the ```sizeOf``` function, an implementation in Template Haskell currently doesn't seem feasible. Thus, instances have to be written one by one.

## License

This part of Hasky is licensed under the ```MIT``` License. Please be aware that the full ```Hasky``` package is under ```LGPLv3```. Refer to the accompanying LICENSE or COPYING files for details.
