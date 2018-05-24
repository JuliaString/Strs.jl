The Strs.jl package is licensed under the MIT "Expat" License:

Copyright (c) 2017-2018: Gandalf Software, Inc. (Scott Paul Jones), and other contributors (see below)

Some parts are derived from code that is part of the open source [Julia](https://github.com/JuliaLang/julia) project, see [the Julia license](https://github.com/JuliaLang/julia/blob/master/LICENSE.md) (since not all contributors to Julia are not displayed by [the Julia contributor's list](https://github.com/JuliaLang/julia/contributors), instead use git (git log | grep "Author:" | sort -u) to get a full list), in particular `base/char.jl`, `base/strings/*`, `test/strings/*`

This also incorporates a large amount of code from various PRs I made, which were merged into Julia for v0.4.x, although some of that code was later moved to [LegacyStrings](https://github.com/JuliaArchive/LegacyStrings.jl) in v0.5.x:

| **PR** | **Description** |
|:--------:|:-------------------------------------------|
| [#10948](https://github.com/JuliaLang/julia/pull/10948) | Fixed bug in unicode.jl/encode16 |
| [#11004](https://github.com/JuliaLang/julia/pull/11004) | (split into other PRs) |
| [#11107](https://github.com/JuliaLang/julia/pull/11107) | Improve performance of length on UTF8String and UTF16String |
| [#11175](https://github.com/JuliaLang/julia/pull/11175) | Fix #11171 is_valid_char |
| [#11178](https://github.com/JuliaLang/julia/pull/11178) | Simplify and improve performance of rand(Char) |
| [#11192](https://github.com/JuliaLang/julia/pull/11192) | Fix #11142 Added new ascii/utf8 methods |
| [#11195](https://github.com/JuliaLang/julia/pull/11195) | Fix #11140 Added `is_valid_utf32` |
| [#11203](https://github.com/JuliaLang/julia/pull/11203) | Fix #11141/#10973 and improve performance of is_valid_utf8/is_valid_ascii |
| [#11213](https://github.com/JuliaLang/julia/pull/11213) | Add update on `is_valid_char` |
| [#11241](https://github.com/JuliaLang/julia/pull/11241) | Add isvalid(Type, value) methods, to replace `is_valid_*` |
| [#11390](https://github.com/JuliaLang/julia/pull/11390) | Fix #11379 deprecate utf32(Integer...) |
| [#11482](https://github.com/JuliaLang/julia/pull/11482) | Add 2-argument isvalid(::Type{T},str::T) |
| [#11533](https://github.com/JuliaLang/julia/pull/11533) | Fix #9365 rsearch/rsearchindex with UTF-8 |
| [#11551](https://github.com/JuliaLang/julia/pull/11551) | Fix #10959 bugs with UTF-16 conversions |
| [#11573](https://github.com/JuliaLang/julia/pull/11573) | Improve Unicode related error messages  |
| [#11575](https://github.com/JuliaLang/julia/pull/11575) | Add UTF encoding validity functions |
| [#11607](https://github.com/JuliaLang/julia/pull/11607) | Fix #10959 bugs with UTF-32 conversions |
| [#11624](https://github.com/JuliaLang/julia/pull/11624) | Fix #10959, fix #11463 bugs with UTF-8 conversions |
| [#11719](https://github.com/JuliaLang/julia/pull/11719) | Fix #11659, problems with tab characters not counted correctly |
| [#11735](https://github.com/JuliaLang/julia/pull/11735) | Fix #11460, Fix #11464 uppercase/lowercase/map on a UTF16String should return a UTF16String |
| [#11925](https://github.com/JuliaLang/julia/pull/11925) | Reorganize `base/string.jl`, `base/utf*`, `test/strings.jl`, `test/unicode.jl` |
| [#12268](https://github.com/JuliaLang/julia/pull/12268) | Add more tests for UTF-16 and UTF-32 to improve unit test coverage |
| [#12305](https://github.com/JuliaLang/julia/pull/12305) | Add coverage tests for print_joined, print_unescaped, unescape_chars |
| [#12360](https://github.com/JuliaLang/julia/pull/12360) | Fix a bug handling CESU-8 strings in convert(UTF8String, Vector{UInt8} |
| [#12635](https://github.com/JuliaLang/julia/pull/12635) | Attempt to get 100% coverage for unicode functions |
| [#12639](https://github.com/JuliaLang/julia/pull/12639) | Fix bugs with using pointer on a SubString, add tests of pointer |
| [#12641](https://github.com/JuliaLang/julia/pull/12641) | Add more coverage for UTF8String |
| [#12646](https://github.com/JuliaLang/julia/pull/12646) | Fix several bugs in reverse(::UTF8String), add full coverage tests |
| [#12767](https://github.com/JuliaLang/julia/pull/12767) | Add coverage in more places for `unicode/*.jl` |
| [#12877](https://github.com/JuliaLang/julia/pull/12877) | Add more testing for strings/types.jl and strings/search.jl |
| [#12882](https://github.com/JuliaLang/julia/pull/12882) | Add extra coverage testing for char.jl |
| [#12898](https://github.com/JuliaLang/julia/pull/12898) | Add coverage tests for strings/basic.jl |
| [#12905](https://github.com/JuliaLang/julia/pull/12905) | Deprecate getindex/checkbounds methods for non Integer Real indices for Chars and Strings |

The Julia implementation of MurmurHash3 was based on [MurmurHash3](https://github.com/JuliaLang/julia/blob/master/src/support/MurmurHash3.c) by Autin Appleby, which was placed in the public domain.

Some of the ideas for the design and how to implement came from collaborating on the [StringEncodings](https://github.com/nalimilan/StringEncodings.jl) package, by [Milan Bouchet-Valat](https://github.com/nalimilan) and the [Strings](https://github.com/quinnj/Strings.jl) package, by [Jacob Quinn](https://github.com/quinnj)

The [JuliaString logo(s)](Logo.gif) are copyright (c) 2018 [@cormullion](https://github.com/cormullion) (Julia artist extraordinaire: math + code + Julia -> visual poetry)

Strs.jl is under the MIT license (see below):

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

