The Strs.jl package is licensed under the MIT "Expat" License:

Copyright (c) 2017-2018: Gandalf Software, Inc. (Scott Paul Jones), and other contributors (see below)

Some parts are derived/inspired from code that is:

1) part of the open source [Julia](https://github.com/JuliaLang/julia) project, see https://github.com/JuliaLang/julia/blob/master/LICENSE.md (since not all contributors to Julia are not displayed by https://github.com/JuliaLang/julia/contributors, instead use git (git log | grep "Author:" | sort -u) to get a full list), in particular base/char.jl, base/strings/*, test/strings/*
2) various PRs I wrote which were incorporated into Julia, some of the code of which was later moved to [LegacyStrings](https://github.com/JuliaArchive/LegacyStrings.jl)
   [#10948](https://github.com/JuliaLang/julia/pull/10948), [#11004](https://github.com/JuliaLang/julia/pull/11004) (split into other PRs), [#11081](https://github.com/JuliaLang/julia/pull/11081), [#11107](https://github.com/JuliaLang/julia/pull/11107), [#11175](https://github.com/JuliaLang/julia/pull/11175), [#11178](https://github.com/JuliaLang/julia/pull/11178), [#11192](https://github.com/JuliaLang/julia/pull/11192), [#11195](https://github.com/JuliaLang/julia/pull/11195), [#11203](https://github.com/JuliaLang/julia/pull/11203), [#11213](https://github.com/JuliaLang/julia/pull/11213), [#11241](https://github.com/JuliaLang/julia/pull/11241), [#11390](https://github.com/JuliaLang/julia/pull/11390), [#11482](https://github.com/JuliaLang/julia/pull/11482), [#11533](https://github.com/JuliaLang/julia/pull/11533), [#11551](https://github.com/JuliaLang/julia/pull/11551), [#11573](https://github.com/JuliaLang/julia/pull/11573), [#11575](https://github.com/JuliaLang/julia/pull/11575), [#11607](https://github.com/JuliaLang/julia/pull/11607), [#11624](https://github.com/JuliaLang/julia/pull/11624), [#11719](https://github.com/JuliaLang/julia/pull/11719), [#11735](https://github.com/JuliaLang/julia/pull/11735), [#11925](https://github.com/JuliaLang/julia/pull/11925), [#12268](https://github.com/JuliaLang/julia/pull/12268), [#12305](https://github.com/JuliaLang/julia/pull/12305), [#12360](https://github.com/JuliaLang/julia/pull/12360), [#12635](https://github.com/JuliaLang/julia/pull/12635), [#12639](https://github.com/JuliaLang/julia/pull/12639), [#12641](https://github.com/JuliaLang/julia/pull/12641), [#12646](https://github.com/JuliaLang/julia/pull/12646), [#12767](https://github.com/JuliaLang/julia/pull/12767), [#12877](https://github.com/JuliaLang/julia/pull/12877), [#12882](https://github.com/JuliaLang/julia/pull/12882), [#12898](https://github.com/JuliaLang/julia/pull/12898), [#12905](https://github.com/JuliaLang/julia/pull/12905)
3) the [MurmurHash3](https://github.com/JuliaLang/julia/blob/master/src/support/MurmurHash3.c) by Autin Appleby.
4) the [StringEncodings](https://github.com/nalimilan/StringEncodings.jl) package, by [Milan Bouchet-Valat](https://github.com/nalimilan)
5) the [Strings](https://github.com/quinnj/Strings.jl) package, by [Jacob Quinn](https://github.com/quinnj)


With the exception of MurmurHash, which was placed in the public domain),
those are under the MIT license (see below):

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

