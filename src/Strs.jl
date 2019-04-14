__precompile__(true)
"""
Strs package

Copyright 2017-2019 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
"""
module Strs

using ModuleInterfaceTools
export @api

using PCRE2

import StrTables: lookupname, matchchar, matches, longestmatches, completions
export lookupname, matchchar, matches, longestmatches, completions

@api extend! StrRegex, StrLiterals, Format
@api use! StrFormat, StrEntities

using InternedStrings: intern
export @i_str, @I_str, intern

macro i_str(str) ; Expr(:call, :intern, interpolated_parse(str, StrBase._str)) ; end
macro I_str(str) ; Expr(:call, :intern, interpolated_parse(str, StrBase._str, true)) ; end

@api reexport StrRegex, StrLiterals

@api freeze

end # module Strs
