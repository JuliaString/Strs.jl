__precompile__(true)
"""
Strs package

Copyright 2017-018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
"""
module Strs

using ModuleInterfaceTools
export @api, V6_COMPAT

using PCRE2

import StrTables: lookupname, matchchar, matches, longestmatches, completions
export lookupname, matchchar, matches, longestmatches, completions

@api extend! StrRegex, StrLiterals
@api use! Format, StrFormat, StrEntities

using InternedStrings: intern
export @i_str, intern

macro i_str(str) ; Expr(:call, :intern, s_interp_parse(false, UniStr, str)) ; end
macro I_str(str) ; Expr(:call, :intern, s_interp_parse(true,  UniStr, str)) ; end

const m_eval = ModuleInterfaceTools.m_eval

function __init__()
    StrLiterals.string_type[] = UniStr
end

# Need to fix ModuleInterfaceTools to do this!
for mod in (StrRegex, StrLiterals), grp in (:modules, :public, :public!)
    m_eval(Expr( :export, getfield(m_eval(mod, :__api__), grp)...))
end

@api freeze

end # module Strs
