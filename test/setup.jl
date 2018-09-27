const test_legacy = false

using BenchmarkTools

using Strs

@static V6_COMPAT || (using REPL)

@api test StrAPI, StrBase, StrRegex

const ver = "v0.$(VERSION.minor)"
const git = "https://github.com/JuliaString/"

const pkgdir    = @static V6_COMPAT ? Pkg.dir("Strs") : dirname(pathof(Strs))
const RC        = @static V6_COMPAT ? Base.REPLCompletions : REPL.REPLCompletions
_uninit(T, len) = @static V6_COMPAT ? T(len) : T(undef, len)

@static test_legacy && (using LegacyStrings)

const STRS_SETUP = true

