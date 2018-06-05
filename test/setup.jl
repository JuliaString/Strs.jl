const test_legacy = false

using BenchmarkTools
using ModuleInterfaceTools
using Strs

@static V6_COMPAT || (using Pkg, REPL)

@api test StrAPI, StrBase, StrRegex

const ver = "v0.$(VERSION.minor)"
const git = "https://github.com/JuliaString/"
const pkgdir = Pkg.dir()

const RC        = @static V6_COMPAT ? Base.REPLCompletions : REPL.REPLCompletions
_uninit(T, len) = @static V6_COMPAT ? T(len) : T(undef, len)

@static test_legacy && (using LegacyStrings)

const STRS_SETUP = true

