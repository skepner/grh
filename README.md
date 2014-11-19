gr
==

Simple recursive grep substitute written in Haskell.

This version of the program always exits with code 0.  It looks like
to be a limitation of pipes giving no information where pipe was
broken. Program needs to be re-written using conduits.

__MacOS X__

text-icu on which gr depends requires icu4c library. Install it on MacOS X using brew:

> `$ brew install icu4c`

Paths to the library is in cabal.config
