"""
Strs package

This pulls together a number of different packages from JuliaString.org

It has two main goals, first, to be a drop-in replacement for the built-in `String` and `Char`
types, adding types that are both faster and easier to use, that are also using for interfacing
with other languages, and are safer to use and second,
to provide a better option than the built-in string literal syntax and the `@printf`/`@sprintf`
macros for formatted output.
See StrBase, StrRegex, StrLiterals, StrEntities, StrFormat and Format for more documentation.

Copyright 2017-2020 Gandalf Software, Inc., Scott P. Jones
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
