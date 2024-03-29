# C-Structs in Haskell [![Haskell CI](https://github.com/pinselimo/cstructs-in-haskell/actions/workflows/haskell.yml/badge.svg)](https://github.com/pinselimo/cstructs-in-haskell/actions/workflows/haskell.yml) <!-- [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/C-structs/badge)](https://matrix.hackage.haskell.org/package/C-structs) --> [![Hackage Version](https://img.shields.io/hackage/v/C-structs.svg?label=Hackage)](http://hackage.haskell.org/package/C-structs) [![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/C-structs.svg)](https://hackage.haskell.org/package/C-structs)

C-structs lets you create dynamically typed and correctly padded C structs in Haskell.
These can be used for FFI calls, imports and exports.
This package is part of the development efforts for the Python library [```Pythas```](https://github.com/pinselimo/Pythas/).
Pythas provides an interface to import Haskell modules.

Note: As of GHC 9.2 structs cannot be passed by value, [only by reference](https://wiki.haskell.org/Foreign_Function_Interface#Foreign_types).

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
    int s21st;
    int s22nd;
};

struct Struct2 s;
s.s21st = 1;
s.s22nd = 2;
~~~

or with Python's ```ctypes```:

~~~python
>>> from ctypes import Structure, c_int
>>> class Struct2( Structure ):
...     _fields_ = [("s21st", c_int), ("s22nd", c_int)]
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
    val->s21st = 42;
    val->s22nd = 63;
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

For a more elaborated usage examples checkout [```Pythas```](https://github.com/pinselimo/Pythas) in conjunction with [```Pythas-Types```](https://github.com/pinselimo/Pythas-Types).
It uses ```Foreign.C.Structs``` to declare its storage functions for ```Haskell``` tuples. In addition, its Array and Linked List instances are based on this library.

### More fields

Currently ```C-structs``` exports types featuring up to six fields. If you require more, you can easily create them using Template Haskell and the ```structT``` function:

~~~haskell
structT 8
~~~

will create:

~~~haskell
data Struct8 = Struct8
    { s81st :: a
    , s82nd :: b
    , s83rd :: c
    , s84th :: d
    , s85th :: e
    ...
    } deriving (Show, Eq)

instance Storable Struct8 ...
~~~

### Accessors

The naming scheme of the accessor functions follows the names of the ordinal numbers. This can be inconvenient in a Template Haskell context. For these situations ```Foreign.C.Structs``` exposes the ```acs``` function:

~~~haskell
$(acs 8 2)
~~~

This expression will be spliced into a function taking a ```Struct8``` and extracting its second field.

## Testing

Identity properties are tested with QuickCheck to ensure that peek and poke are reversible.
The result of ```sizeOf``` is dependent on the order of types. Its correctness can only be tested with HUnit.
The ```alignment``` function is trivial and only tested implicitly through ```sizeOf```.

Imports from C are tested in ```CTest.hs``` and together with the identity tests form the guarantee that also exports to C are consistent.
Until Travis CI became unusable for FOSS projects all tests were performed for all available GHC/CABAL/Stack versions through the [Stack CI script](https://docs.haskellstack.org/en/stable/travis_ci/) on both Linux and OSX to ensure maximum compatibility. Now only the latest GHC versions are checked on Linux, macOS and Windows using cabal through GitHub Actions. Compatibility with older versions of GHC should however be kept.

## License

This part of Pythas is licensed under the ```MIT``` License. Please be aware that the full ```Pythas``` package is under ```LGPLv3```. Refer to the accompanying LICENSE or COPYING files for details.

