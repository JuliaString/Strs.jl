[pkg-url]:  https://juliaci.github.io/NanosoldierReports/pkgeval_badges/report.html
[strs-pkg]: https://juliaci.github.io/NanosoldierReports/pkgeval_badges/S/Strs.svg
[contrib]:    https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat
[codecov-url]:  https://codecov.io/gh/JuliaString/Strs.jl
[codecov-img]:  https://codecov.io/gh/JuliaString/Strs.jl/branch/master/graph/badge.svg

[![Logo](Logo.gif)](https://github.com/cormullion)

# Strs

[![contributions welcome][contrib]](https://github.com/JuliaString/Strs.jl/issues)
[![][strs-pkg]][pkg-url]
[![][codecov-img]][codecov-url]

Strs.jl is a container for a number of different packages from [JuliaString](https://github.com/JuliaString)
It has two main goals:
1) To be a drop-in replacement for the built-in `String` and `Char` types, adding types that are both faster and easier to use,
   that are also using for interfacing with other languages, and are safer to use.
2) To have a better option than the built-in string literal syntax and the `@printf`/`@sprintf` macros for formatted output.

It brings together the following:

1) A better type of string literal, using [StrLiterals](https://github.com/JuliaString/StrLiterals.jl), [StrFormat](https://github.com/JuliaString/StrFormat.jl), and [StrEntities](https:://github.com/JUliaString/StrEntities.jl)

   This is of the form `f"..."` or `F"..."`.

   This uses Swift-style `\` escape sequences, such as `\u{xxxx}` for Unicode constants, instead of `\uXXXX` and `\UXXXXXXXX`, which have the advantage of not having to worry about some digit or letter A-F or a-f occurring after the last hex digit of the Unicode constant.

   It also means that `$`, a very common character for LaTeX strings or output of currencies, does not need to be in a string quoted as '\$'

   It uses `\(expr)` for interpolation like Swift, instead of `$name` or `$(expr)`, which also has the advantage of not having to worry about the next character in the string someday being allowed in a name.

   It allows for embedding Unicode characters using a variety of easy to remember names, instead of hex codes:
   `\:emojiname:`
   `\<latexname>`
   `\N{unicodename}`
   `\&htmlname;`
   Examples of this are:
   `f"\<dagger> \&yen; \N{ACCOUNT OF} \:snake:"`, which returns the string: `"† ¥ ℀ 🐍   "`

   Default formatting based on the type of the argument: i.e. `f"\(1.23)"` returns `"1.23"`, but `f"\%(1.23)"` returns `"1.230000"` based on the default format set up for AbstractFloat types.
   See [Format](https://github.com/JuliaString/Format.jl) for more information on how to set up defaults for your own types, or to change the defaults for floats, strings, etc.

   It also supports C-style formatting, but without having to count the position of the argument: i.e. instead of `@sprintf("$name has \$%10.8f", 1.23)"`, `f"\(name) has $\%10.8f(1.23)"`

2) Faster and more flexible set of string and character types,
   such as `ASCIIStr`, `Latin1Str`, `UCS2Str`, `UTF32Str`, that are indexed by character,
   which can be much easier to use than to have deal with `nextind` and `prevind`, etc.
   This alleviates a common source of bugs in Julia, where people are unfamiliar with the difference between indexing by the byte or codeunit offset, instead of by the codepoints (characters).
   Using these types in your code can help speed things up.

3) Faster and VALIDATED `UTF8Str` type.
   Julia's built-in `String` type allows storing invalid sequences.
   (Strs provides a `Text1Str` type for holding strings that might be invalid UTF-8, or might be
   some other encoding, such as Microsoft's CP1252)
   This is especially a problem because the built-in Regex support in Julia incorrectly passes a flag saying that the argument has already been checked and is a valid UTF-8 sequence.  Skipping that check in PCRE2 does make regex matching much faster, however it leaves you open to attacks if your are using regex on unchecked string input.

4) Types for Binary strings, as well as strings that are known to be text strings, but whose encoding is not known (might be UTF-8 with certain commonly accepted but invalid sequence, such as representing characters > uFFFF in 6 bytes, as two 16 bit surrogate characters, or encoding a null byte as `\0xc0\0x80` instead of `\0`, or S-JIS, CP1252, etc.)

5) Highly optimized string functions, operating on 2, 4, or 8 characters at a time
   (I do intend to optimize these further, by using vector instructions on Intel, ARM, and POWER architectures, to process up to 64 characters at a time).

6) Thread-safe Regex support (it was not thread-safe in the LTS (long term support) version of Julia, currently v1.05, but that has been fixed as of the v1.3 release)

7) Regex support that doesn't assume that `String` values are valid UTF-8, so that it can't be used as a way of attacking programs written in Julia by passing certain unvalidated strings to the PCRE2 library. For speed, one can use the `UTF8Str` type instead of `String` using `R"..."` instead of the `r"..."`.

I would very much appreciate any constructive criticism, help implementing some of the ideas, ideas on how to make it perform better, bikeshedding on names and API, etc.
Also, I'd love contributions of benchmark code and/or samples for different use cases of strings,
or pointers to such (such as a way to get lots of tweets, to test mixed text and emojis, for example).


| **Package** | **Release** | **Release Date** | **Package Evaluator** | **Unit Tests** | **Description** |
|:-------------:|:-------------:|:-------------:|:-------------:|:-----------:|:-------------------|
| [ModuleInterfaceTools][mit-loc] | [![][mit-rel]][mit-loc] | [![][mit-dat]][mit-loc] | [![][mit-pkg]][pkg-url] | [![][mit-chk]][mit-loc] | Tools to create a common API for all of these packages
| [StrAPI][strapi-loc] | [![][strapi-rel]][strapi-loc] | [![][strapi-dat]][strapi-loc] | [![][strapi-pkg]][pkg-url] | [![][strapi-chk]][strapi-loc] | Common API for string/character functionality
| [CharSetEncodings][cse-loc] | [![][cse-rel]][cse-loc] | [![][cse-dat]][cse-loc] | [![][cse-pkg]][pkg-url] | [![][cse-chk]][cse-loc] | Basic types/support for Character Sets, Encodings, and Character Set Encodings
| [ChrBase][chrbase-loc] | [![][chrbase-rel]][chrbase-loc] | [![][chrbase-dat]][chrbase-loc] | [![][chrbase-pkg]][pkg-url] | [![][chrbase-chk]][chrbase-loc] | `Chr{CharSet,CodeUnitType}` type and support
| [MurmurHash3][mh3-loc] | [![][mh3-rel]][mh3-loc] | [![][mh3-dat]][mh3-loc] | [![][mh3-pkg]][pkg-url] | [![][mh3-chk]][mh3-loc] | Pure Julia implementation of MurmurHash3
| [PCRE2][pcre2-loc] | [![][pcre2-rel]][pcre2-loc] | [![][pcre2-dat]][pcre2-loc] | [![][pcre2-pkg]][pkg-url] | [![][pcre2-chk]][pcre2-loc] | `PCRE2` library support
| [Format][format-loc] | [![][format-rel]][format-loc] | [![][format-dat]][format-loc] | [![][format-pkg]][pkg-url] | [![][format-chk]][format-loc] | Python/C style formatting (based on [Formatting](https://github.com/JuliaIO/Formatting.jl))
| [StrBase][strbase-loc] | [![][strbase-rel]][strbase-loc] | [![][strbase-dat]][strbase-loc] | [![][strbase-pkg]][pkg-url] | [![][strbase-chk]][strbase-loc] | `Str{CSE, Hash, SubSet, Cache}` type
| [StrRegex][strregex-loc] | [![][strregex-rel]][strregex-loc] | [![][strregex-dat]][strregex-loc] | [![][strregex-pkg]][pkg-url] | [![][strregex-chk]][strregex-loc] | `Regex` support for all string types
| [StrLiterals][strliterals-loc] | [![][strliterals-rel]][strliterals-loc] | [![][strliterals-dat]][strliterals-loc] | [![][strliterals-pkg]][pkg-url] | [![][strliterals-chk]][strliterals-loc] | Extensible string literal support
| [StrFormat][strformat-loc] | [![][strformat-rel]][strformat-loc] | [![][strformat-dat]][strformat-loc] | [![][strformat-pkg]][pkg-url] | [![][strformat-chk]][strformat-loc] | Formatting extensions for literals
| [StrTables][strtables-loc] | [![][strtables-rel]][strtables-loc] | [![][strtables-dat]][strtables-loc] | [![][strtables-pkg]][pkg-url] | [![][strtables-chk]][strtables-loc] | Low-level support for entity tables
| [HTML_Entities][html-loc] | [![][html-rel]][html-loc] | [![][html-dat]][html-loc] | [![][html-pkg]][pkg-url] | [![][html-chk]][html-loc] | HTML character sequences
| [Emoji_Entities][emoji-loc] | [![][emoji-rel]][emoji-loc] | [![][emoji-dat]][emoji-loc] | [![][emoji-pkg]][pkg-url] | [![][emoji-chk]][emoji-loc] | Emoji names (including composite ones)
| [LaTeX_Entities][latex-loc] | [![][latex-rel]][latex-loc] | [![][latex-dat]][latex-loc] | [![][latex-pkg]][pkg-url] | [![][latex-chk]][latex-loc] | Julia LaTeX character names
| [Unicode_Entities][unicode-loc] | [![][unicode-rel]][unicode-loc] | [![][unicode-dat]][unicode-loc] | [![][unicode-pkg]][pkg-url] | [![][unicode-chk]][unicode-loc] | Unicode standard character names
| [StrEntities][strentities-loc] | [![][strentities-rel]][strentities-loc] | [![][strentities-dat]][strentities-loc] | [![][strentities-pkg]][pkg-url] | [![][strentities-chk]][strentities-loc] | Entity extensions for literals
| [InternedStrings][int-loc] | [![][int-rel]][int-loc] | [![][int-dat]][int-loc] | [![][int-pkg]][pkg-url] | [![][int-chk]][int-loc] | Save space by interning strings (by @oxinabox!)


[mit-loc]: https://github.com/JuliaString/ModuleInterfaceTools.jl
[mit-pkg]: https://juliaci.github.io/NanosoldierReports/pkgeval_badges/M/ModuleInterfaceTools.svg
[mit-rel]: https://img.shields.io/github/release/JuliaString/ModuleInterfaceTools.jl.svg?label="."
[mit-dat]: https://img.shields.io/github/release-date/JuliaString/ModuleInterfaceTools.jl.svg?label="."
[mit-chk]: https://img.shields.io/github/checks-status/JuliaString/ModuleInterfaceTools.jl/master

[mh3-loc]:     https://github.com/JuliaString/MurmurHash3.jl
[mh3-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/M/MurmurHash3.svg
[mh3-rel]:     https://img.shields.io/github/release/JuliaString/MurmurHash3.jl.svg?label="."
[mh3-dat]:     https://img.shields.io/github/release-date/JuliaString/MurmurHash3.jl.svg?label="."
[mh3-chk]:     https://img.shields.io/github/checks-status/JuliaString/MurmurHash3.jl/master

[pcre2-loc]:     https://github.com/JuliaString/PCRE2.jl
[pcre2-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/P/PCRE2.svg
[pcre2-rel]:     https://img.shields.io/github/release/JuliaString/PCRE2.jl.svg?label="."
[pcre2-dat]:     https://img.shields.io/github/release-date/JuliaString/PCRE2.jl.svg?label="."
[pcre2-chk]:     https://img.shields.io/github/checks-status/JuliaString/PCRE2.jl/master

[format-loc]:     https://github.com/JuliaString/Format.jl
[format-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/F/Format.svg
[format-rel]:     https://img.shields.io/github/release/JuliaString/Format.jl.svg?label="."
[format-dat]:     https://img.shields.io/github/release-date/JuliaString/Format.jl.svg?label="."
[format-chk]:     https://img.shields.io/github/checks-status/JuliaString/Format.jl/master

[strapi-loc]:     https://github.com/JuliaString/StrAPI.jl
[strapi-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/S/StrAPI.svg
[strapi-rel]:     https://img.shields.io/github/release/JuliaString/StrAPI.jl.svg?label="."
[strapi-dat]:     https://img.shields.io/github/release-date/JuliaString/StrAPI.jl.svg?label="."
[strapi-chk]:     https://img.shields.io/github/checks-status/JuliaString/StrAPI.jl/master

[cse-loc]:     https://github.com/JuliaString/CharSetEncodings.jl
[cse-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/C/CharSetEncodings.svg
[cse-rel]:     https://img.shields.io/github/release/JuliaString/CharSetEncodings.jl.svg?label="."
[cse-dat]:     https://img.shields.io/github/release-date/JuliaString/CharSetEncodings.jl.svg?label="."
[cse-chk]:     https://img.shields.io/github/checks-status/JuliaString/CharSetEncodings.jl/master

[chrbase-loc]:     https://github.com/JuliaString/ChrBase.jl
[chrbase-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/C/ChrBase.svg
[chrbase-rel]:     https://img.shields.io/github/release/JuliaString/ChrBase.jl.svg?label="."
[chrbase-dat]:     https://img.shields.io/github/release-date/JuliaString/ChrBase.jl.svg?label="."
[chrbase-chk]:     https://img.shields.io/github/checks-status/JuliaString/ChrBase.jl/master

[strbase-loc]:     https://github.com/JuliaString/StrBase.jl
[strbase-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/S/StrBase.svg
[strbase-rel]:     https://img.shields.io/github/release/JuliaString/StrBase.jl.svg?label="."
[strbase-dat]:     https://img.shields.io/github/release-date/JuliaString/StrBase.jl.svg?label="."
[strbase-chk]:     https://img.shields.io/github/checks-status/JuliaString/StrBase.jl/master

[strregex-loc]:     https://github.com/JuliaString/StrRegex.jl
[strregex-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/S/StrRegex.svg
[strregex-rel]:     https://img.shields.io/github/release/JuliaString/StrRegex.jl.svg?label="."
[strregex-dat]:     https://img.shields.io/github/release-date/JuliaString/StrRegex.jl.svg?label="."
[strregex-chk]:     https://img.shields.io/github/checks-status/JuliaString/StrRegex.jl/master

[strliterals-loc]:     https://github.com/JuliaString/StrLiterals.jl
[strliterals-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/S/StrLiterals.svg
[strliterals-rel]:     https://img.shields.io/github/release/JuliaString/StrLiterals.jl.svg?label="."
[strliterals-dat]:     https://img.shields.io/github/release-date/JuliaString/StrLiterals.jl.svg?label="."
[strliterals-chk]:     https://img.shields.io/github/checks-status/JuliaString/StrLiterals.jl/master

[strformat-loc]:     https://github.com/JuliaString/StrFormat.jl
[strformat-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/S/StrFormat.svg
[strformat-rel]:     https://img.shields.io/github/release/JuliaString/StrFormat.jl.svg?label="."
[strformat-dat]:     https://img.shields.io/github/release-date/JuliaString/StrFormat.jl.svg?label="."
[strformat-chk]:     https://img.shields.io/github/checks-status/JuliaString/StrFormat.jl/master

[strtables-loc]:     https://github.com/JuliaString/StrTables.jl
[strtables-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/S/StrTables.svg
[strtables-rel]:     https://img.shields.io/github/release/JuliaString/StrTables.jl.svg?label="."
[strtables-dat]:     https://img.shields.io/github/release-date/JuliaString/StrTables.jl.svg?label="."
[strtables-chk]:     https://img.shields.io/github/checks-status/JuliaString/StrTables.jl/master

[html-loc]:     https://github.com/JuliaString/HTML_Entities.jl
[html-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/H/HTML_Entities.svg
[html-rel]:     https://img.shields.io/github/release/JuliaString/HTML_Entities.jl.svg?label="."
[html-dat]:     https://img.shields.io/github/release-date/JuliaString/HTML_Entities.jl.svg?label="."
[html-chk]:     https://img.shields.io/github/checks-status/JuliaString/HTML_Entities.jl/master

[emoji-loc]:     https://github.com/JuliaString/Emoji_Entities.jl
[emoji-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/E/Emoji_Entities.svg
[emoji-rel]:     https://img.shields.io/github/release/JuliaString/Emoji_Entities.jl.svg?label="."
[emoji-dat]:     https://img.shields.io/github/release-date/JuliaString/Emoji_Entities.jl.svg?label="."
[emoji-chk]:     https://img.shields.io/github/checks-status/JuliaString/Emoji_Entities.jl/master

[latex-loc]:     https://github.com/JuliaString/LaTeX_Entities.jl
[latex-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/L/LaTeX_Entities.svg
[latex-rel]:     https://img.shields.io/github/release/JuliaString/LaTeX_Entities.jl.svg?label="."
[latex-dat]:     https://img.shields.io/github/release-date/JuliaString/LaTeX_Entities.jl.svg?label="."
[latex-chk]:     https://img.shields.io/github/checks-status/JuliaString/LaTeX_Entities.jl/master

[unicode-loc]:     https://github.com/JuliaString/Unicode_Entities.jl
[unicode-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/U/Unicode_Entitites.svg
[unicode-rel]:     https://img.shields.io/github/release/JuliaString/Unicode_Entities.jl.svg?label="."
[unicode-dat]:     https://img.shields.io/github/release-date/JuliaString/Unicode_Entities.jl.svg?label="."
[unicode-chk]:     https://img.shields.io/github/checks-status/JuliaString/Unicode_Entities.jl/master

[strentities-loc]:     https://github.com/JuliaString/StrEntities.jl
[strentities-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/S/StrEntities.svg
[strentities-rel]:     https://img.shields.io/github/release/JuliaString/StrEntities.jl.svg?label="."
[strentities-dat]:     https://img.shields.io/github/release-date/JuliaString/StrEntities.jl.svg?label="."
[strentities-chk]:     https://img.shields.io/github/checks-status/JuliaString/StrEntities.jl/master

[int-loc]:     https://github.com/JuliaString/InternedStrings.jl
[int-pkg]:     https://juliaci.github.io/NanosoldierReports/pkgeval_badges/I/InternedStrings.svg
[int-rel]:     https://img.shields.io/github/release/JuliaString/InternedStrings.jl.svg?label="."
[int-dat]:     https://img.shields.io/github/release-date/JuliaString/InternedStrings.jl.svg?label="."
[int-chk]:     https://img.shields.io/github/checks-status/JuliaString/InternedStrings.jl/master


The package [ModuleInterfaceTools](https://github.com/JuliaString/ModuleInterfaceTools.jl) is used to set up a consistent and easy to use API for most of the cooperating packages, without having to worry too much about imports, exports, using, and what functions are part of a public API, and which ones are part of the internal development API for other packages to extend.

## Architecture and Operations

The general philosophy of the architecture is as follows: have a single easy to use type that can replace `String` that conforms to the recommendations of the Unicode Organization (which internally uses 4 types and is implemented currently as a Julia Union, and has O(1) indexing to characters, not just code units), as well as types to represent binary strings, raw unvalidated strings (made up of 1, 2, or 4 byte codepoints), as well as types for validated ASCII, Latin1, UCS2 (16-bit, BMP [Basic Multilingual Plane]), UTF-8, UTF-16, and UTF-32 encoded strings.

Optimizations for multi code unit encodings such as UTF-8 & UTF-16 will be moved to `StrUTF8` and `StrUTF16` packages (before splitting them out, I'll make sure that the functionality still works, only with slower generic methods, so that you only take up the extra space if you need the faster speed).
Extensions such as converting to and from non-Unicode encodings, such as Windows CP-1252 or China's official character set, GB18030,  will be done in another package, `StrEncodings`.

Subtypes that directly support things like substrings, caching hash values, and caching one or more versions of the string (such as the originally unmodified byte, 16-bit or 32-bit word stream, in the case where the input was not valid, or a valid UTF-8 (similar to the way Python can cache a UTF-8 version of a string) and/or UTF-16 encoded version, for better performance when interoperating with other languages such as JavaScript, Swift, Java, or OS APIs like Windows that expect UTF-16).

Since things like `AbstractChar` and `CodeUnits` which I originally implemented for this package have now been added to `Base` (in master), I have moved support for those to a file to provide them for v0.6.2.
There is a `codepoints` iterator, which iterates over the unsigned integer codepoints in a string
(for strings with the `CodeUnitSingle` trait, it is basically the same as the `codeunits`
iterator).

Also in the works is using the new ability to add properties, in order to allow for different "views" of a string, no matter how it is stored internally, for example a `mystring.utf8` view, or a `mystring.utf16` view (that can use the internal cached copy if available, as an optimization).


## Types
Currently, there are the following types:

* `Str`, which is the general, parameterized string type, and
* `Chr`, the general, parameterized character type.

* `BinaryStr` for storing non-textual data as a sequence of bytes.

* `ASCIIStr`   an ASCII string, composed of `ASCIIChr` 1-byte codepoints
* `LatinStr`   a string using the Latin1 subset of Unicode, composed of `LatinChr` 1-byte codepoints.
* `UCS2Str`    a string composed of characters (`UCS2Chr`s) only in the Unicode BMP, stored as 2 byte code units (that each store a single codepoint)
* `UTF32Str`   a string with only valid Unicode characters, 0-0xd7ff, 0xe000-0x10ffff, stored as 4 byte code units.

* `UTF8Str`    a string with only valid Unicode characters, the same as `UTF32Str`, however encoded using UTF-8, conforming to the Unicode Organization's standard, i.e. no long encodings, surrogates, or invalid bytes.
* `UTF16Str`   a string similar to `UTF8Str, encoded via UTF-16, also conforming to the Unicode standard, i.e. no out of order or isolated surrogates.

* `Text1Str`   a text string that may contain any sequence of bytes
* `Text2Str`   a text string that may contain any sequence of 16-bit words
* `Text4Str`   a text string that may contain any sequence of 32-bit words

* `UniStr`     a Union type, that can be one of the following 4 types, `ASCIIStr`, and 3 internal types:
* `_LatinStr`  a byte string that must contain at least one character > 0x7f
* `_UCS2Str`   a word string that must contain at least one character > 0xff
* `_UTF32Str`  a 32-bit word string that must contain at least one character > 0xffff

The only real difference in handling `LatinStr` and `_LatinStr`, is that uppercasing the characters `'µ': (Unicode U+00b5 (category Ll: Letter, lowercase)` and `'ÿ': Unicode U+00ff (category Ll: Letter, lowercase)` produces the BMP characters `'Μ': Unicode U+039c (category Lu: Letter, uppercase)` and `'Ÿ': Unicode U+0178 (category Lu: Letter, uppercase)` respectively in `_LatinStr`, because it is just an optimization for storing the full Unicode character set, not the ANSI/ISO 8859-1 character set that ise used for the first 256 code points of Unicode.
Those three internal types should never be used directly, as indicated by the leading `_` in the name.

For all of the built-in `Str` types, there is a corresponding built-in character set encoding, i.e. `BinaryCSE`, `LatinCSE`, `UTF8CSE`, etc.
There are also a number of similar built-in character sets defined (`*CharSet`), and character types (`*Chr`).
The `cse` function returns the character set encoding for a string type or a string.
`charset` returns the character set, and `encoding` returns the encoding.
For example, `cse(UTF8Str)` returns `UTF8CSE`, `charset(UTF8Str)` returns `CharSet{UTF32}`,
`encoding(UTF8Str)` return `Encoding{UTF8}()`

## Functions

There is a new API that I am working on for indexing and searching, (however there is a problem on v0.7 due to the deprecation for `find` being overbroad, and causing a type of type piracy, deprecating methods of types not in Base):

* `find(First, ...)` replaces `findfirst`
* `find(Last, ...)`  replaces `findlast`
* `find(Next, ...)`  replaces `findnext`
* `find(Prev, ...)`  replaces `findprev`

* `index(str, pos)`  replaces `thisind(str, pos)`
* `index(Next, ...`  replaces `nextind(...)`
* `index(Prev, ...`  replaces `prevind(...)`

Also there are more readable function names that always separate words with `_`, and avoid hard to understand abbreviations:

* `is*`              -> `is_*` (for `ascii`, `digit`, `space`, `numeric`, `valid`,
		                  `defined`, `empty`, `assigned`)
* `iscntrl`          -> `is_control`
* `isgraph`          -> `is_graphic`
* `isprint`          -> `is_printable`
* `ispunct`          -> `is_punctuation`
* `isalpha`          -> `is_letter`
* `isalnum`          -> `is_alphanumeric`
* `isgraphemebreak`  -> `is_grapheme_break`
* `isgraphemebreak!` -> `is_grapheme_break!`
* `occursin`         -> `occurs_in`
* `textwidth`        -> `text_width`
* `lowercasefirst`   -> `lowercase_first`
* `uppercasefirst`   -> `uppercase_first`
* `startswith`       -> `starts_with`
* `endswith`         -> `ends_with`

* In addition, I've added `is_alphabetic`

## Kudos

Nobody is an island, and to achieve great things, one must stand on the shoulders of giants.

I would like to thank some of those giants in particular:

* The four co-creators of Julia: [Jeff Bezanson](https://github.com/JeffBezanson),[Viral B. Shah](https://github.com/ViralBShah), [Alan Edelman](https://github.com/alanedelman), and [Stefan Karpinski](https://github.com/StefanKarpinski), without their uncompromising greediness, none of this would be possible.

* [Tom Breloff](https://github.com/tbreloff), for showing how an ecosystem could be created in Julia, i.e. "Build it, and they will come", for providing some nice code in this [PR](https://github.com/JuliaIO/Formatting.jl/pull/10) (which I shamelessly pirated in order to create [Format](https://github.com/JuliaString/Format.jl), and for good advice at JuliaCon.
* [Ismael Venegas Castelló](https://twitter.com/SalchiPapa1337) for encouraging me to [tweet](https://twitter.com/GandalfSoftware) about Julia starting at the 2015 JuliaCon, for good advice, and being a great guy in general.
* [Chris Rackaukas](https://github.com/ChrisRackauckas) simply a star in Julia now, great guy, great advice, and great blogs about stuff that's usually way over my head. Julia is incredibly lucky to have him.
* [Jacob Quinn](https://github.com/quinnj), for collaborating & discussions early on in [Strings](https://github.com/quinnj/Strings.jl) on ideas for better string support in Julia, as well as a lot of hard work on things dear to me, such as databases and importing/exporting data [SQLite](https://github.com/JuliaDatabases/SQLite.jl), [ODBC](https://github.com/JuliaDatabases/ODBC.jl), [CSV](https://github.com/JuliaData/CSV.jl), [WeakRefStrings](https://github.com/JuliaData/WeakRefStrings.jl), [DataStreams](https://github.com/JuliaData/DataStreams.jl), [Feather](https://github.com/JuliaData/Feather.jl), [JSON2](https://github.com/quinnj/JSON2.jl)
* [Milan Bouchet-Valat](https://github.com/nalimilan), for discussions on string handling and encoding in [StringEncodings](https://github.com/nalimilan/StringEncodings.jl)
* [Tim Holy](https://github.com/timholy) for the famous "Holy" Trait Trick, which I use extensively in the Str* packages, for the work along with [Matt Bauman](https://github.com/mbauman) on making Julia arrays general, extensible while still performing well, and hence very useful in my work.
* [Steven G. Johnson]() for illuminating me on how one could create a whole package in very few lines of code when I first started learning Julia, see [DecFP](https://github.com/stevengj/DecFP.jl)
* [Tony Kelman](https://github.com/tkelman), for very thorough reviews of my PRs, I learned a great deal from his (and other Julians') comments), including that I didn't have to code in C anymore to get the performance I desired.

* [Lyndon White](https://github.com/oxinabox), I've already "appropriated" :grinning: his very nice [InternedStrings](https://github.com/JuliaString/InternedStrings.jl) into this package, I'm really lucky to have gotten him to join the organization!
* [Bogumił Kamiński](https://github.com/bkamins) who has been doing a great job testing and reviewing `Strs` (as well as doing the same for the string/character support in Julia Base), as well as input into the design. (Also very glad to have co-opted him to become a member of the org)
* Last but not least, Julia mathematical artist (and blogger!) extraordinaire, [Cormullion](https://github.com/cormullion), creator of our wonderful logo!

Also thanks to anybody who's answered my (sometimes stupid :grinning:) questions on [Gitter](https://gitter.im/JuliaLang/julia) and [Discourse](https://discourse.julialang.org/)

Kudos to all of the other contributors to Julia and the ever expanding Julia ecosystem!
