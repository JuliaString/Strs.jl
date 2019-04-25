const test_legacy = false

using BenchmarkTools

using Strs

using REPL

@api test StrAPI, StrBase, StrRegex

const ver = "v0.$(VERSION.minor)"
const git = "https://github.com/JuliaString/"

const pkgdir    = dirname(pathof(Strs))
const RC        = REPL.REPLCompletions
_uninit(T, len) = T(undef, len)

@static test_legacy && (using LegacyStrings)

const STRS_SETUP = true

