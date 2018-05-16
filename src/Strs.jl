__precompile__(true)
"""
Strs package

Copyright 2017-018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
"""
module Strs

using APITools
@api init

using PCRE2

import StrTables: lookupname, matchchar, matches, longestmatches, completions
export lookupname, matchchar, matches, longestmatches, completions

using InternedStrings: intern
export @i_str, intern

macro i_str(str) ; Expr(:call, :intern, s_interp_parse(false, UniStr, str)) ; end
macro I_str(str) ; Expr(:call, :intern, s_interp_parse(true,  UniStr, str)) ; end

using Format
export FormatSpec, FormatExpr, printfmt, printfmtln, format, generate_formatter
export pyfmt, cfmt, fmt
export fmt_default, fmt_default!, reset!, default_spec, default_spec!

@api extend StrAPI, CharSetEncodings, Chars, StrBase, StrRegex,
            StrLiterals, StrFormat, StrEntities

# Need to fix APITools to do this!
for mod in (StrAPI, CharSetEncodings, Chars, StrBase, StrRegex, StrLiterals,
            StrFormat, StrEntities),
    grp in (:define_module, :define_public, :public)
    eval(Expr( :export, getfield(Core.eval(mod, :__api__), grp)...))
end

@api freeze

end # module Strs
