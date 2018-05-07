# Strs

[![Build Status](https://travis-ci.org/JuliaString/Strs.jl.svg?branch=master)](https://travis-ci.org/JuliaString/Strs.jl)

[![Coverage Status](https://coveralls.io/repos/github/JuliaString/Strs.jl/badge.svg?branch=master)](https://coveralls.io/github/JuliaString/Strs.jl?branch=master)

[![codecov.io](http://codecov.io/github/JuliaString/Strs.jl/coverage.svg?branch=master)](http://codecov.io/github/JuliaString/Strs.jl?branch=master)

[![](https://twitter.com/twitter/statuses/995385258332901377)]

The `Strs` package is now working on both the release version (v0.6.2) and the latest master (v0.7.0-DEV).

It represents an attempt to give Julia better string handling than possible with Base `String` and `Char`.

I am now trying to make sure that all of the functionality in String and Char is implemented for
Str and Chr, and to start optimizing the functions (although they are already substantially faster)

Strs.jl is now a container for a number of different packages from [JuliaString.org](https://juliastring.org)

* [StrAPI](https://github.com/JuliaString/StrAPI.jl): Common API for string/character functionality
* [CharSetEncodings](https://github.com/JuliaString/CharSetEncodings.jl): Basic types/support for Character Sets, Encodings, and Character Set Encodings
* [Chars](https://github.com/JuliaString/Chars.jl): `Chr{CharSet,CodeUnitType}` type and support
* [StrBase](https://github.com/JuliaString/StrBase.jl): `Str{CSE, Hash, SubSet, Cache}` type
* [PCRE2](https://github.com/JuliaString/PCRE2.jl): `PCRE2` library support
* [StrRegex](https://github.com/JuliaString/StrRegex.jl): `Regex` support for all string types
* [StrLiterals](https://github.com/JuliaString/StrLiterals.jl): Extensible string literal support
* [Format](https://github.com/JuliaString/Format.jl): Python/C style formatting (based on [Formatting](https://github.com/JuliaIO/Formatting.jl)
* [StrFormat](https://github.com/JuliaString/StrFormat.jl): formatting extensions for literals
* [StrTables](https://github.com/JuliaString/StrTables.jl): low-level support for entity tables
* [HTML_Entities](https://github.com/JuliaString/HTML_Entities.jl)
* [LaTeX_Entities](https://github.com/JuliaString/LaTeX_Entities.jl)
* [Emoji_Entities](https://github.com/JuliaString/Emoji_Entities.jl)
* [Unicode_Entities](https://github.com/JuliaString/Unicode_Entities.jl)
* [StrEntities](https://github.com/JuliaString/StrEntities.jl): Entity extensions for literals
* [InternedStrings](https://github.com/JuliaString/InternedStrings.jl): save space by interning strings (by @oxinabox!)

The new package [APITools](https://github.com/JuliaString/APITools.jl) is used to set up a consistent and easy to use API for most of the cooperating packages, without having to worry too much about imports, exports, using, and what functions are part of a public API, and which ones are part of the internal development API for other packages to extend.

I would very much appreciate any constructive criticism, help implementing some of the ideas, ideas on how to make it perform better, bikeshedding on names and API, etc.
Also, I'd love contributions of benchmark code and/or samples for different use cases of strings,
or pointers to such (such as a way to get lots of tweets, to test mixed text and emojis, for example).

## Architecture and Operations

The general philosophy of the architecture is as follows: have a single easy to use type that can replace `String` that conforms to the recommendations of the Unicode Organization (which internally uses 4 types and is implemented currently as a Julia Union, and has O(1) indexing to characters, not just code units), as well as types to represent binary strings, raw unvalidated strings (made up of 1, 2, or 4 byte codepoints), as well as types for validated ASCII, Latin1, UCS2 (16-bit, BMP [Basic Multilingual Plane]), UTF-8, UTF-16, and UTF-32 encoded strings.

Operations on multi code unit encodings such as UTF-8 & UTF-16 will be moved to a `UnicodeStr` package.
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
