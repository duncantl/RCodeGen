RCodeGen
========

Functions to aid generating R and C/C++ code

These functions have grown out of the RGCCTranslationUnit and RCIndex
packages and I am now putting them in their own package, with a focus
on the RCIndex data types.

The idea underlying the package is that we have some native code -
either C or C++ - and we obtain a description of the elements of that
code. We then generate wrapper functions and routines to interface R
to that code.  This package provides functionality to help generate
these wrapper functions and routines.  Often, one will use contextual
information to customize these functions. There are several hooks and
parameters to enable such customization. In other cases, one builds
new/alternative functions that build on the lower-level functions.

We can obtain a description of the native code elements in various different ways.
* RCIndex uses the libclang API (and is now my preferred approach).
* RGCCTranslationUnit uses gcc -fdump-translation-unit.
* gcc-xml emits descriptions of code in an XML format. (The rdyncall package
uses this.)
* The SWinTypeLibs package reads information from Windows type libraries.


This code is used to create code for other packages and these serve as
examples. See 
* [RGraphicsDevice](http://github.com/omegahat/RGraphicsDevice), 
* [RCIndex](http://github.com/omegahat/RClangSimple.git) (named RClangSimple on github), and
* [RCUDA](http://github.com/duncantl/RCUDA.git)
packages. See the TU/ directories in those packages.

