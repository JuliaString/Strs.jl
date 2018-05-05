#=
Basic types for characters and strings

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md

Encodings inspired from collaborations on the following packages:
https://github.com/quinnj/Strings.jl with @quinnj (Jacob Quinn)
https://github.com/nalimilan/StringEncodings.jl with @nalimilan (Milan Bouchet-Valat)
=#
const STR_KEEP_NUL    = true  # keep nul byte placed by String

# Note: this is still in transition to expressing character set, encoding
# and optional cached info for hashes, UTF-8/UTF-16 encodings, subsets, etc.
# via more type parameters

struct Str{T,SubStr,Cache,Hash} <: AbstractString
    data::String
    substr::SubStr
    cache::Cache
    hash::Hash

    (::Type{Str})(::Type{T}, v::String, s::S, c::C, h::H) where {T<:CSE,S,C,H} =
        new{T,S,C,H}(v,s,c,h)
end

(::Type{Str})(::Type{C}, v::String) where {C<:CSE} = Str(C, v, nothing, nothing, nothing)
(::Type{Str})(::Type{C}, v::Str) where {C<:CSE} = Str(C, v.data, nothing, nothing, nothing)

struct Chr{CS<:CharSet,T<:CodeUnitTypes} <: AbstractChar
    v::T
    (::Type{Chr})(::Type{CS}, v::T) where {CS<:CharSet,T<:CodeUnitTypes} = new{CS,T}(v)
end

(::Type{<:Chr{CS,T}})(v::Number) where {CS<:CharSet,T<:CodeUnitTypes} = Chr(CS, T(v))

# Handle change from endof -> lastindex
@static if !isdefined(Base, :lastindex)
    lastindex(str::AbstractString) = Base.endof(str)
    lastindex(arr::AbstractArray) = Base.endof(arr)
    Base.endof(str::Str) = lastindex(str)
end
@static if !isdefined(Base, :firstindex)
    firstindex(str::AbstractString) = 1
    # AbstractVector might be an OffsetArray
    firstindex(str::Vector) = 1
end

# This needs to be redone, with character sets and the code unit as part of the type

using CharSetEncodings: _cpname1, _cpname2, _cpname4
for (names, typ) in ((_cpname1, UInt8), (_cpname2, UInt16), (_cpname4, UInt32)), nam in names
    chrnam = symstr(nam, "Chr")
    @eval const $chrnam = Chr{$(symstr(nam, "CharSet")), $typ}
    @eval export $chrnam
    @eval show(io::IO, ::Type{$chrnam}) = print(io, $(quotesym(chrnam)))
end

const _LatinChr = Chr{LatinSubSet, UInt8}
show(io::IO, ::Type{_LatinChr}) = print(io, :_LatinChr)

codepoint(ch::Chr) = ch.v
basetype(::Type{<:Chr{CS,B}}) where {CS,B} = B
charset(::Type{<:Chr{CS,B}}) where {CS,B} = CS
typemin(::Type{T}) where {CS,B,T<:Chr{CS,B}} = Chr(CS, typemin(B))
typemax(::Type{T}) where {CS,B,T<:Chr{CS,B}} = Chr(CS, typemax(B))

const LatinChars   = Union{LatinChr, _LatinChr}
const ByteChars    = Union{ASCIIChr, LatinChr, _LatinChr, Text1Chr}
const WideChars    = Union{UCS2Chr, UTF32Chr}

# Make wrappers for String type, this can help to be able to make (hashed) SubStr's of Strings
export RawUTF8Str
const RawUTF8Str = Str{RawUTF8CSE, Nothing, Nothing, Nothing}
show(io::IO, ::Type{RawUTF8Str}) = print(io, :RawUTF8Str)

using CharSetEncodings: _charsets, _subsetnam, _mbwname, _binname

codepoint_cse(::Type{Char}) = RawUTF8CSE
for nam in vcat(_charsets, :_Latin)
    @eval codepoint_cse(::Type{$(symstr(nam,"Chr"))}) = $(symstr(nam,"CSE"))
end

# Definition of built-in Str types

const empty_string = ""

for nam in vcat(_charsets, _subsetnam, _mbwname, _binname)
    str = String(nam)
    sym = symstr(nam, "Str")
    @eval const $sym = Str{$cse, Nothing, Nothing, Nothing}
    @eval (::Type{$sym})(v::Vector{UInt8}) = convert($sym, v)
    @eval show(io::IO, ::Type{$sym}) = print(io, $(quotesym(sym)))
    low = lowercase(str)
    if str[1] == '_'
        @eval empty_str(::Type{$cse}) = $(symstr("empty", low))
    else
        emp = symstr("empty_", low)
        @eval const $emp = Str($cse, empty_string)
        @eval empty_str(::Type{$cse}) = $emp
        @eval export $sym
    end
    @eval convert(::Type{$sym}, str::$sym) = str
end
empty_str(::Type{<:Str{C}}) where {C<:CSE} = empty_str(C)
empty_str(::Type{String}) = empty_string

const empty_rawutf8 = Str(RawUTF8CSE, empty_string)
empty_str(::Type{RawUTF8CSE}) = empty_rawutf8

typemin(::Type{T}) where {T<:Str} = empty_str(T)
typemin(::T) where {T<:Str} = empty_str(T)

"""Union type for fast dispatching"""
const UniStr = Union{ASCIIStr, _LatinStr, _UCS2Str, _UTF32Str}
show(io::IO, ::Type{UniStr}) = print(io, :UniStr)

# Display BinaryCSE as if String
show(io::IO, str::T) where {T<:Str{BinaryCSE}} = show(io, str.data)
show(io::IO, str::SubString{T}) where {T<:Str{BinaryCSE}} =
    @inbounds show(io, SubString(str.string.data, str.offset+1, str.offset+lastindex(str)))

_allocate(len) = Base._string_n((len+STR_KEEP_NUL-1)%Csize_t)

function _allocate(::Type{T}, len) where {T <: CodeUnitTypes}
    buf = _allocate((len+STR_KEEP_NUL-1) * sizeof(T))
    buf, reinterpret(Ptr{T}, pointer(buf))
end

# Various useful groups of character set types

using CharSetEncodings: Binary_CSEs, Latin_CSEs, UTF8_CSEs, UCS2_CSEs, SubSet_CSEs
using CharSetEncodings: Byte_CSEs, Word_CSEs, Quad_CSEs

const MS_UTF8     = MaybeSub{<:Str{UTF8CSE}}
const MS_UTF16    = MaybeSub{<:Str{UTF16CSE}}
const MS_UTF32    = MaybeSub{<:Str{UTF32CSE}}
const MS_SubUTF32 = MaybeSub{<:Str{_UTF32CSE}}
const MS_Latin    = MaybeSub{<:Str{<:Latin_CSEs}}
const MS_ByteStr  = MaybeSub{<:Str{<:Binary_CSEs}}
const MS_RawUTF8  = MaybeSub{<:Union{String,Str{RawUTF8CSE}}}

_wrap_substr(::Type{<:Any}, str) = str
_wrap_substr(::Type{SubString}, str) = SubString(str, 1)
_empty_sub(::Type{T}, ::Type{C}) where {T,C} = _wrap_substr(T, empty_str(C))

const AbsChar = @static isdefined(Base, :AbstractChar) ? AbstractChar : Union{Char, Chr}

## Get the character set / encoding used by a string type
cse(::Type{<:Str{C}}) where {C<:CSE} = C

# Promotion rules for characters

promote_rule(::Type{T}, ::Type{T}) where {T<:Str} = T
promote_rule(::Type{T}, ::Type{T}) where {T<:Chr} = T

promote_rule(::Type{String}, ::Type{<:Str}) = String
promote_rule(::Type{Char}, ::Type{<:Chr})   = Char

promote_rule(::Type{<:Str{S}}, ::Type{<:Str{T}}) where {S,T} =
    (P = promote_rule(S,T)) === Union{} ? Union{} : Str{P}

sizeof(s::Str) = sizeof(s.data) + 1 - STR_KEEP_NUL

"""Codeunits of string as a Vector"""
_data(s::Vector{UInt8}) = s
_data(s::String)        = s
_data(s::Str{<:Byte_CSEs}) =
    @static V6_COMPAT ? Vector{UInt8}(s.data) : unsafe_wrap(Vector{UInt8}, s.data)

"""Pointer to codeunits of string"""
pointer(s::Str{<:Byte_CSEs}) = pointer(s.data)
pointer(s::Str{<:Word_CSEs}) = reinterpret(Ptr{UInt16}, pointer(s.data))
pointer(s::Str{<:Quad_CSEs}) = reinterpret(Ptr{UInt32}, pointer(s.data))

const CHUNKSZ = sizeof(UInt64) # used for fast processing of strings
const CHUNKMSK = (CHUNKSZ-1)%UInt64

_pnt64(s::Union{String,Vector{UInt8}}) = reinterpret(Ptr{UInt64}, pointer(s))
_pnt64(s::Str) = reinterpret(Ptr{UInt64}, pointer(s.data))

"""Length of string in codeunits"""
ncodeunits(s::Str)              = sizeof(s)
ncodeunits(s::Str{<:Word_CSEs}) = sizeof(s) >>> 1
ncodeunits(s::Str{<:Quad_CSEs}) = sizeof(s) >>> 2

# For convenience
@inline _calcpnt(str, siz) = (pnt = _pnt64(str) - CHUNKSZ;  (pnt, pnt + siz))

@inline _mask_bytes(n) = (1%UInt << ((n & CHUNKMSK) << 3)) - 0x1

Base.need_full_hex(c::Chr) = is_hex_digit(c)
Base.escape_nul(c::Chr) = ('0' <= c <= '7') ? "\\x00" : "\\0"

# Support for SubString of Str

Base.SubString(str::Str{C}) where {C<:SubSet_CSEs} =
    SubString(Str(basecse(C), str))
Base.SubString(str::Str{C}, off::Int) where {C<:SubSet_CSEs} =
    SubString(Str(basecse(C), str), off)
Base.SubString(str::Str{C}, off::Int, fin::Int) where {C<:SubSet_CSEs} =
    SubString(Str(basecse(C), str), off, fin)

# pointer conversions of ASCII/UTF8/UTF16/UTF32 strings:
pointer(str::Str, pos::Integer) = bytoff(pointer(str), pos - 1)

# pointer conversions of SubString of ASCII/UTF8/UTF16/UTF32:
pointer(x::SubString{<:Str}) = bytoff(pointer(x.string), x.offset)
pointer(x::SubString{<:Str}, pos::Integer) = bytoff(pointer(x.string), x.offset + pos - 1)

# CodePoints iterator

struct CodePoints{T}
    xs::T
end

"""
    codepoints(str)

An iterator that generates the code points of a string

# Examples
```jldoctest
julia> a = Str("abc\U1f596")

julia> collect(a)

julia> collect(codepoints(a))
```
"""
codepoints(xs) = CodePoints(xs)
eltype(::Type{<:CodePoints{S}}) where {S} = eltype(S)
length(it::CodePoints) = length(it.xs)
start(it::CodePoints) = start(it.xs)
done(it::CodePoints, state) = done(it.xs, state)
@propagate_inbounds next(it::CodePoints{T}, state) where {T<:Str} =
    _next(CodePointStyle(T), eltype(T), it.xs, state)
