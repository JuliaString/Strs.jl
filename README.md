# Strs

[![Build Status](https://travis-ci.org/JuliaString/Strs.jl.svg?branch=master)](https://travis-ci.org/JuliaString/Strs.jl)

[![Coverage Status](https://coveralls.io/repos/JuliaString/Strs.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/JuliaString/Strs.jl?branch=master)

[![codecov.io](http://codecov.io/github/JuliaString/Strs.jl/coverage.svg?branch=master)](http://codecov.io/github/JuliaString/Strs.jl?branch=master)

The `Strs` package is very WIP at the moment (having been written over two weeks during Xmas break), and represents an attempt to give Julia better string handling than possible with Base `String` and `Char`.

Please understand that the code is being rapidly rewritten, as I come up with new ideas and learn better ways of using traits and making the code more generic, without losing any of the performance gains.

I would very much appreciate any constructive criticism, help implementing some of the ideas, ideas on how to make it perform better, bikeshedding on names and API, etc.
Also, I'd love contributions of benchmark code and/or samples for different use cases of strings,
or pointers to such (such as a way to get lots of tweets, to test mixed text and emojis, for example).

The general philosophy of the architecture is as follows: have a single easy to use type that can replace `String` that conforms to the recommendations of the Unicode Organization (which internally uses 4 types and is implemented currently as a Julia Union, and has O(1) indexing to characters, not just code units), as well as types to represent binary strings, raw unvalidated strings (made up of 1, 2, or 4 byte codepoints), as well as types for validated ASCII, Latin1, UCS2 (16-bit, BMP [Basic Multilingual Plane]), UTF-8, UTF-16, and UTF-32 encoded strings.

Operations on multi code unit encodings such as UTF-8 & UTF-16 will be moved to a `UnicodeStr` package.
Extensions such as converting to and from non-Unicode encodings, such as Windows CP-1252 or China's official character set, GB18030,  will be done in another package, `StrEncodings`.

Subtypes that directly support things like substrings, caching hash values, and caching one or more versions of the string (such as the originally unmodified byte, 16-bit or 32-bit word stream, in the case where the input was not valid, or a valid UTF-8 (similar to the way Python can cache a UTF-8 version of a string) and/or UTF-16 encoded version, for better performance when interoperating with other languages such as JavaScript, Swift, Java, or OS APIs like Windows that expect UTF-16).

There are iterators `codeunits` and `codepoints` to return those (although I may get rid of
`codepoints`, and have the default iteration return the codepoints, and have a `chars` iterator instead to produce the `Char` type [which is not really what you'd want in most all cases anyway])

Also in the works is using the new ability to add properties, in order to allow for different "views" of a string, no matter how it is stored internally, for example a `mystring.utf8` view, or a `mystring.utf16` view (that can use the internal cached copy if available, as an optimization).

Currently, there are the following types:

* `Str`, which is the general, parameterized type.

* `BinaryStr` for storing non-textual data as a sequence of bytes.

* `ASCIIStr`   an ASCII string, composed of `ASCIIChr` 1-byte codepoints
* `LatinStr`   a string using the Latin1 subset of Unicode, composed of `LatinChr` 1-byte codepoints.
* `UCS2Str`    a string composed of characters (`UCS2Chr`s) only in the Unicode BMP, stored as 2 byte code units (that each store a single codepoint)
* `UTF32Str`   a string with only valid Unicode characters, 0-0xd7ff, 0xe000-0x10ffff, stored as 4 byte code units.

* `UTF8Str`    a string with only valid Unicode characters, the same as `UTF32Str`, however encoded using UTF-8, conforming to the Unicode Organization's standard, i.e. no long encodings, surrogates, or invalid bytes.
* `UTF16Str`   a string similar to `UTF8Str, encoded via UTF-16, also conforming to the Unicode standard, i.e. no out of order or isolated surrogates.

* `RawByteStr` a text string that may contain any sequence of bytes
* `RawWordStr` a text string that may contain any sequence of 16-bit words
* `RawCharStr` a text string that may contain any sequence of 32-bit words

* `UniStr`     a Union type, that can be one of the following 4 types, `ASCIIStr`, and 3 internal types:
* `_LatinStr`  a byte string that must contain at least one character > 0x7f
* `_UCS2Str`   a word string that must contain at least one character > 0xff
* `_UTF32Str`  a 32-bit word string that must contain at least one character > 0xffff

The only real difference in handling `LatinStr` and `_LatinStr`, is that uppercasing the characters `'µ': (Unicode U+00b5 (category Ll: Letter, lowercase)` and `'ÿ': Unicode U+00ff (category Ll: Letter, lowercase)` produces the BMP characters `'Μ': Unicode U+039c (category Lu: Letter, uppercase)` and `'Ÿ': Unicode U+0178 (category Lu: Letter, uppercase)` respectively in `_LatinStr`, because it is just an optimization for storing the full Unicode character set, not the ANSI/ISO 8859-1 character set that ise used for the first 256 code points of Unicode.
Those three internal types should never be used directly, as indicated by the leading `_` in the name.
