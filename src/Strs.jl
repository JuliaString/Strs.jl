__precompile__(true)
"""
Strs package

Copyright 2017-018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
"""
module Strs

using ModuleInterfaceTools
using ModuleInterfaceTools: m_eval
export @api, V6_COMPAT

using PCRE2

import StrTables: lookupname, matchchar, matches, longestmatches, completions
export lookupname, matchchar, matches, longestmatches, completions

@api extend! StrRegex, StrLiterals, Format
@api use! StrFormat, StrEntities

using InternedStrings: intern
export @i_str, @I_str, intern

macro i_str(str) ; Expr(:call, :intern, interpolated_parse(str, StrBase._str)) ; end
macro I_str(str) ; Expr(:call, :intern, interpolated_parse(str, StrBase._str, true)) ; end

const curmod = @static V6_COMPAT ? current_module() : @__MODULE__

# Need to fix ModuleInterfaceTools to do this!
for mod in (StrRegex, StrLiterals), grp in (:modules, :public, :public!)
    m_eval(curmod, Expr( :export, getfield(m_eval(mod, :__api__), grp)...))
end

@api freeze

end # module Strs
