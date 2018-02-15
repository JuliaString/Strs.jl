__precompile__(true)
"""
Strs package

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based partly on code in LegacyStrings that used to be part of Julia
"""
module Strs

#=
Ideas:
Have substring support built in

Have types using both a UniStr type along with UTF-8 and/or UTF-16 versions, and take advantage of
knowing the real length of the string from the UniStr version, and always reading the string,
except for times when a UTF-8 (or UTF-16) version is needed, using the O(1) version.

Have extra parameter(s?), for types to hold things like: UnitRange{UInt16,UInt32,UInt64},
for substrings, UInt64 for caching hash value, Str{T}, where T could be Raw*, UTF8, or UTF16,
to cache either unchanged input data and/or validated UTF8/UTF16, and possibly even one of those with a substring type!

Add types _UCS2 and _UTF32, for the internal versions of those encodings, which are guaranteed to
have at least one character of that type, which allows for O(1) == comparisons between strings of
different types. [done!]

Having a mixed string type, for large strings where you may have occasional BMP or non-BMP characters, but the major portion is ASCII/Latin1.
   -> Keep a table of offsets and types, 2 bits per 64 character chunk for encoded type of chunk,
      offset table, 16-bits, 32-bits, or 64-bits?
      16-bits could handle 16+5-2 = 2^19 characters, max cost 16K for 512K character string,
      32-bits could handle up to 32GB character strings

Have a function that given a Vector{UInt8}, BinaryStr, UTF8Str, String, etc.
returns a vector of substring'ed UniStr, which only has to make copies if some of the lines
are _Latin, _UCS2, _UTF32.
(This could be done as well for input of Vector{UInt16}, RawWordStr)

To save space, and for better performance, lines with different types are pooled together,
and lines with ASCII can point to the original, except if the input is Vector{UInt8},
in which case they are also added to the pooled buffer for the lines.
In addition, depending on the percentage of characters still pointing into the original string
(again, only if it's not a Vector, which must be copied), it may decide to go ahead and copy
all of the ASCII substrings into the pool.
Note: for good substring performance, some of the operations that are optimized to work 8 bytes
(or more) at a time, will need to deal with masking the initial chunk, not just the final chunk.
=#

# Convenience functions
export to_ascii, utf8, utf16, utf32

export unsafe_str, codeunit, codeunits, codepoints, @str_str, @condimport

symstr(s...) = Symbol(string(s...))
quotesym(s...) = Expr(:quote, symstr(s...))

macro str_str(string)
    :( unsafe_str($(esc(string))) )
end

"""Import the symbol from Base if defined, otherwise export it"""
macro condimport(sym)
    :( if isdefined(Base, $(quotesym(sym))) ; import Base: $sym ; else ; export $sym ; end )
end        

"""Import the symbol from module if defined, otherwise export it"""
macro condimport(mod, sym)
    :( if isdefined($mod, $(quotesym(sym))) ; import $mod: $sym ; else ; export $sym ; end )
end

using Base: @_inline_meta, @propagate_inbounds, @_propagate_inbounds_meta

import Base: containsnul, convert, getindex, length, map, pointer, collect,
             reverse, rsearch, search, sizeof, string, unsafe_convert, unsafe_load, write,
             codeunit, start, next, done, nextind, prevind, reverseind,
             typemin, typemax, isvalid, rem, size, ndims, first, last, eltype, isempty, in,
             hash, isless, ==, -, +, *, ^, cmp, promote_rule, one, repeat, filter,
             print, show, isimmutable
    
@condimport ind2chr
@condimport chr2ind
@condimport thisind
@condimport codeunits
@condimport ncodeunits
@condimport bytestring
@condimport lastindex

isdefined(Base, :copyto!)        || (const copyto! = copy!)
isdefined(Base, :unsafe_copyto!) || (const unsafe_copyto! = unsafe_copy!)
isdefined(Base, :AbstractChar)   || (abstract type AbstractChar end ; export AbstractChar)
isdefined(Base, :Nothing)        || (const Nothing = Void)
isdefined(Base, :Cvoid)          || (const Cvoid = Void)

@static isdefined(Base, :codeunits) || include("codeunits.jl")

uninit(T, len) = @static isdefined(Base, :uninitialized) ? T(uninitialized, len) : T(len)
create_vector(T, len) = uninit(Vector{T}, len)

include("types.jl")
include("chars.jl")
include("access.jl")
include("traits.jl")
include("unicode.jl")
include("casefold.jl")
include("iters.jl")
include("core.jl")
include("support.jl")
include("compare.jl")
include("ascii.jl")
include("latin.jl")
include("utf8.jl")
include("utf16.jl")
include("utf32.jl")
include("encode.jl")
include("stats.jl")
include("legacy.jl")
include("utf8case.jl")
include("utf16case.jl")
#include("util.jl")
#include("substring.jl")
#include("io.jl")

end # module Strs
