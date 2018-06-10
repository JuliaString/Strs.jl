const test_legacy = false

using ModuleInterfaceTools

@static V6_COMPAT || (using REPL)

using BenchmarkTools
using Strs

@api test StrAPI, StrBase, StrRegex

const ver = "v0.$(VERSION.minor)"
const git = "https://github.com/JuliaString/"
const pkgdir = Pkg.dir("Strs")

const RC        = @static V6_COMPAT ? Base.REPLCompletions : REPL.REPLCompletions
_uninit(T, len) = @static V6_COMPAT ? T(len) : T(undef, len)

@static test_legacy && (using LegacyStrings)

const STRS_SETUP = true

