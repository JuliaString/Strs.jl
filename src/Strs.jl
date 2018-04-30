__precompile__(true)
"""
Strs package

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones,
and other contributors to the Julia language
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


New ideas:
Have a single concrete "UniStr" type, which uses bits in the trailing "nul" byte of the String
representation, to store the following information:

NotValidated, Invalid, NoASCII, Latin, BMP, UTF32, Hash present, Short

2 bits: 00 -> Valid, 01 -> Invalid, 10 -> NotValidated, 11 means?
1 bit:  0  -> Some ASCII, 1 -> no ASCII (bit flipped from others so that 0 -> ASCIIStr)
1 bit:  0  -> No Latin1,  1 -> some Latin1
1 bit:  0  -> ByteWise,   1 -> WordWise
1 bit:  0/1 Hash present
1 bit:  0/1 Short
1 bit:  ?

Extra byte for wordwise:
1 bit:  0  -> None > 0x7ff, 1 -> some > 0x7ff (for UTF8 conversions?)
1 bit:  0  -> No BMP,   1 -> some BMP (0x800-0xd7ff,0xe000-0xffff)
1 bit:  0  -> Only BMP, 1 -> some non-BMP (0x10000-0x10ffff)
Have at least 5 bits for other information.

So: ASCIIStr would be: Valid, All ASCII, ... i.e. 0 + short/hash bits
    _LatinStr would be: Valid, maybe no ascii, Latin1, no bmp, no non-bmp
    _UCS2Str  would be: Valid, maybe no ascii, maybe Latin1, some bmp, no non-bmp
    _UTF32Str would be: Valid, maybe no ascii, maybe Latin1, maybe BMP, some non-bmp
=#

const V6_COMPAT = VERSION < v"0.7.0-DEV"

@static if V6_COMPAT
    has_module(mod) = isdefined(module_parent(current_module()), mod) || isdefined(Main, mod)
else
    has_module(mod) = isdefined(parentmodule(@__MODULE__), mod) || isdefined(Main, mod)
end

const HAS_COMPAT = has_module(:Compat)

# Convenience functions
export to_ascii, utf8, utf16, utf32

export str, unsafe_str, codepoints

export category_code, category_string, category_abbrev, is_mutable, index

# From types.jl
export Str, Chr, UniStr, CSE, CharSet, Encoding, @cs_str, @enc_str, @cse
export cse, charset, encoding, basetype

# Note: the generated *Str, *Chr, *CSE, *CharSet and encoding names
# (Native*, Swapped*, UTF8Encoding) for the built-in types are exported directly from types.jl

export BIG_ENDIAN, LITTLE_ENDIAN

# From search.jl
export found, find_result

# From unicode.jl
export is_assigned, is_grapheme_break, is_grapheme_break!, graphemes,
       category_code, category_abbrev, category_string

symstr(s...) = Symbol(string(s...))
quotesym(s...) = Expr(:quote, symstr(s...))

using Base: @_inline_meta, @propagate_inbounds, @_propagate_inbounds_meta, RefValue

import Base: containsnul, convert, getindex, length, map, pointer, collect, in, hash,
             reverse, sizeof, string, cconvert, unsafe_convert, unsafe_load, read, write,
             start, next, done, nextind, prevind,
             typemin, typemax, rem, size, ndims, first, last, eltype,
             isless, isequal, ==, -, +, *, ^, cmp, promote_rule, one, repeat, filter,
             print, show, isimmutable, chop, chomp, replace, ascii, uppercase, lowercase,
             lstrip, rstrip, strip, lpad, rpad, split, rsplit, join, IOBuffer, IteratorSize

# Conditionally import names that are only in v0.6 or in master
for sym in (:codeunit, :codeunits, :ncodeunits,
            :thisind, :firstindex, :lastindex, :codepoint, :Fix2, :unsafe_crc32c)
    if isdefined(Base, sym)
        @eval import Base: $sym
    else
        @eval export $sym
    end
end

# Possibly import functions, give new names with underscores

for (oldname, newname) in ((:textwidth,      :text_width),
                           (:lowercasefirst, :lowercase_first),
                           (:uppercasefirst, :uppercase_first),
                           (:occursin,       :occurs_in),
                           (:startswith,     :starts_with),
                           (:endswith,       :ends_with))
    if isdefined(Base, oldname)
        @eval import Base: $oldname
        @eval const $newname = $oldname
    else
        @eval function $newname end
    end
    @eval export $newname
end

# Possibly import `is` functions, give more readable names starting with `is_`

for (oldn, newn) in ((:xdigit, :hex_digit),
                     (:cntrl,  :control),
                     (:punct,  :punctuation),
                     (:print,  :printable))
    oldname, newname = symstr("is",  oldn), symstr("is_", newn)
    if isdefined(Base, oldname)
        @eval import Base: $oldname
        @eval const $newname = $oldname
    end
    @eval export $newname
end

# Possibly import `is` functions, rename to start with `is_`

for nam in (:ascii, :digit, :space, :alpha, :numeric, :valid, :defined, :empty)
    oldname, newname = symstr("is",  nam), symstr("is_", nam)
    if isdefined(Base, oldname)
        @eval import Base: $oldname
        @eval const $newname = $oldname
    end
    @eval export $newname
end

# Handle renames where function was deprecated

export utf8crc, is_alphanumeric, is_graphic, is_lowercase, is_uppercase

# Location of isgraphemebreak moved from Base.UTF8proc to Base.Unicode,
# import and add new names with underscores

@eval import Base.$(V6_COMPAT ? :UTF8proc : :Unicode): isgraphemebreak, isgraphemebreak!,
             graphemes, category_code, category_abbrev, category_string

@static if V6_COMPAT

    # Handle some name changes between v0.6 and master
    const copyto! = copy!
    const unsafe_copyto! = unsafe_copy!
    const Nothing = Void
    const Cvoid = Void
    abstract type AbstractChar end
    export AbstractChar
    import Base: find, ind2chr, chr2ind

    # Handle changes in array allocation
    create_vector(T, len) = Vector{T}(len)

    # Add new short name for deprecated hex function
    outhex(v, p=1) = hex(v,p)

    function get_iobuffer(siz)
        out = IOBuffer(Base.StringVector(siz), true, true)
        out.size = 0
        out
    end

else # !V6_COMPAT

    using Random

    import Base.GC: @preserve

    function find end

    # Handle changes in array allocation
    create_vector(T, len)  = Vector{T}(undef, len)

    # Add new short name for deprecated hex function
    outhex(v, p=1) = string(v, base=16, pad=p)

    get_iobuffer(siz) = IOBuffer(sizehint=siz)

    const utf8crc = Base._crc32c

    import Base: islowercase, isuppercase
    const is_lowercase = islowercase
    const is_uppercase = isuppercase

end # !V6_COMPAT

const is_grapheme_break  = isgraphemebreak
const is_grapheme_break! = isgraphemebreak!

export fnd
const fnd = find

# Operations for find/search operations

export FindOp, Direction, Fwd, Rev, First, Last, Next, Prev, Each, All

abstract type FindOp end
abstract type Direction <: FindOp end
struct Fwd <: Direction end
struct Rev <: Direction end

struct First <: FindOp end
struct Last <: FindOp end
struct Next <: FindOp end
struct Prev <: FindOp end
struct Each <: FindOp end
struct All <: FindOp end

include("types.jl")
@static V6_COMPAT && include("compat.jl")
include("chars.jl")
include("access.jl")
include("traits.jl")
include("utf8proc.jl")
include("unicode.jl")
include("casefold.jl")
include("core.jl")
include("support.jl")
include("compare.jl")
include("ascii.jl")
include("latin.jl")
include("utf8.jl")
include("utf16.jl")
include("utf32.jl")
include("search.jl")
include("utf8search.jl")
include("utf16search.jl")
include("encode.jl")
include("stats.jl")
include("legacy.jl")
include("utf8case.jl")
include("utf16case.jl")
include("util.jl")
include("io.jl")
include("literals.jl")
include("murmurhash3.jl")
include("hash.jl")

end # module Strs
