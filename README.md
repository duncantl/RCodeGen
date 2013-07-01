RCodeGen
========

Functions to aid generating R and C/C++ code

These functions have grown out of the RGCCTranslationUnit
and RCIndex packages and I am now putting them in their own package.

The idea underlying the package is that we have some native code -
either C or C++ - and we obtain a description of the elements of that
code. We then generate wrapper functions and routines to interface R
to that code.  This package provides functionality to help generate
these wrapper functions and routines.

We can obtain a description of the native code elements in various different ways.
* RCIndex uses the libclang API (and is now my preferred approach).
* RGCCTranslationUnit uses gcc -fdump-translation-unit.
* gcc-xml emits descriptions of code in an XML format. (The rdyncall package
uses this.)
* The SWinTypeLibs package reads information from Windows type libraries.