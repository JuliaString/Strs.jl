__precompile__(true)
"""
Strs package

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based partly on code in LegacyStrings that used to be part of Julia
"""
module Strs

export Str, BinaryStr, ASCIIStr, LatinStr, UTF8Str, UCS2Str, UTF16Str, UTF32Str, UniStr
export ascii, latin1, utf8, ucs2, utf16, utf32
export codeunits, codepoints, codeunit_type, codeunit_size

import Base: containsnul, convert, endof, getindex, length, map, pointer,
             reverse, rsearch, search, show, sizeof, string, unsafe_convert, write,
             codeunit, start, next, done, nextind, prevind, reverseind, ind2chr, chr2ind,
             typemin, typemax, isvalid, rem, size, ndims, first, last, eltype, isempty, in,
             hash, isless, ==, -, +

const _list =
    "normalize, graphemes, isassigned, textwidth, islower, isupper, isalpha, isdigit, " *
    "isxdigit, isnumeric, isalnum, iscntrl, ispunct, isspace, isprint, isgraph, " *
    "lowercase, uppercase, titlecase, lcfirst, ucfirst"

const _mod = @static isdefined(Base, :Unicode) ? "Base.Unicode" : "Base"
eval(Meta.parse("import $_mod: $_list"))
eval(Meta.parse("export $_list"))

if isdefined(Base, :ncodeunits)
    import Base: ncodeunits
else
    export ncodeunits
end
if !isdefined(Base, :AbstractChar)
    abstract type AbstractChar end
    export AbstractChar
end

include("chars.jl")

struct Str{T} <: AbstractString
    data::Vector{UInt8}
end

_sv(len) = Base.StringVector(len)

function Str(dat::T) where {T <: Union{Vector{UInt8}, AbstractString}}
    len = length(dat)
    # handle zero length string quickly
    len == 0 && return empty_ascii
    len, flags, num4byte, num3byte, num2byte = unsafe_checkstring(dat, 1, len)
    if flags == 0
        buf = _sv(len)
        if T == Vector{UInt8}
            @inbounds copy!(buf, dat)
        else
            @inbounds for i = 1:len; set_codepoint(buf, dat[i], i); end
        end
        ASCIIStr(buf)
    elseif num4byte != 0
        UTF32Str(_encode(UInt32, dat, len))
    elseif num3byte != 0
        UCS2Str(_encode(UInt16, dat, len))
    else
        buf = _sv(len)
        out = pos = 0
        @inbounds while out < len
            ch8 = dat[pos += 1]
            buf[out += 1] = ch8 <= 0x7f ? ch8 : (((ch8 & 3) << 6) | (dat[pos += 1] & 0x3f))
        end
        num2byte == 0 ? ASCIIStr(buf) : LatinStr(buf)
    end
end

"""Encode a String as a UniStr, picking the best representation"""
Str(str::String) = Str(Vector{UInt8}(str))

const validtypes = (:Binary, :ASCII, :Latin, :UTF8, :UCS2, :UTF16, :UTF32)

function Str(T::Symbol, data::Vector{UInt8})
    T in validtypes || error("Not valid type")
    Str{T}(data)
end

sizeof(s::Str) = sizeof(s.data)

const BinaryStr = Str{:Binary}
const ASCIIStr  = Str{:ASCII}
const LatinStr  = Str{:Latin}
const UTF8Str   = Str{:UTF8}
const UCS2Str   = Str{:UCS2}
const UTF16Str  = Str{:UTF16}
const UTF32Str  = Str{:UTF32}

const ByteStr = Union{BinaryStr, ASCIIStr, LatinStr, UTF8Str}
const WordStr = Union{UCS2Str, UTF16Str}
const WideStr = Union{UCS2Str, UTF16Str, UTF32Str}

abstract type Uni8Str end
abstract type Uni16Str end
abstract type Uni32Str end

const DirectIndexStr = Union{BinaryStr, ASCIIStr, LatinStr, UCS2Str, UTF32Str}

const UniStr = Union{ASCIIStr, LatinStr, UCS2Str, UTF32Str}
const Unicode8 = Union{ASCIIStr, LatinStr}

const UTF8Strings = Union{UTF8Str, String}

const ByteStrings    = Union{BinaryStr, ASCIIStr, LatinStr}
const UnicodeStrings = Union{String, UTF8Str, UTF16Str, UTF32Str}

const ValidStrings = Union{BinaryStr, ASCIIStr, LatinStr, UTF8Str, UCS2Str, UTF16Str, UTF32Str}

isvalid(str::T) where {T<:Str} = isvalid(T, str)
isvalid(::Type{T}, str::T) where {T<:Str} = isvalid(T, _pnt(str), _len(str))
isvalid(::Type{T}, str::T) where {T<:ValidStrings} = true

"""Codeunits of string as a Vector"""
_data(s::String)   = Vector{UInt8}(s)
_data(s::ByteStr)  = s.data
_data(s::WordStr)  = reinterpret(Vector{UInt16}, s.data)
_data(s::UTF32Str) = reinterpret(Vector{UInt32}, s.data)

"""Pointer to codeunits of string"""
_pnt(s::String)   = pointer(s)
_pnt(s::ByteStr)  = pointer(s.data)
_pnt(s::WordStr)  = reinterpret(Ptr{UInt16}, pointer(s.data))
_pnt(s::UTF32Str) = reinterpret(Ptr{UInt32}, pointer(s.data))

"""Length of string in codeunits"""
_len(s::String)   = sizeof(s)
_len(s::ByteStr)  = sizeof(s.data)
_len(s::WordStr)  = sizeof(s.data) >>> 1
_len(s::UTF32Str) = sizeof(s.data) >>> 2

ncodeunits(str::T) where {T<:Str} = _len(str)

const CodeUnitTypes = Union{UInt8, UInt16, UInt32}

"""Type of codeunits"""
codeunit_type(::Type{String})    = UInt8
codeunit_type(::Type{<:ByteStr}) = UInt8
codeunit_type(::Type{<:WordStr}) = UInt16
codeunit_type(::Type{UTF32Str})  = UInt32

"""Size of codeunits"""
codeunit_size(::Type{T}) where {T<:Union{String,Str}} = sizeof(codeunit_type(T))

#= This should be changed to use the new CodePoint type
codepoint_type(::Type{BinaryStr})        = UInt8
codepoint_type(::Type{ASCIIStr})         = _ASCII
codepoint_type(::Type{LatinStr})         = _Latin
codepoint_type(::Type{UCS2Str})          = _UCS2
codepoint_type(::Type{<:UnicodeStrings}) = _UTF32
=#
codepoint_type(::Type{<:ByteStrings})    = UInt8
codepoint_type(::Type{UCS2Str})          = UInt16
codepoint_type(::Type{<:UnicodeStrings}) = UInt32

codepoint_size(::Type{T}) where {T<:Union{String,Str}} = sizeof(codepoint_type(T))

get_codeunit(dat, pos) = codeunit(dat, pos)
get_codeunit(pnt::Ptr{<:CodeUnitTypes}, pos) = unsafe_load(pnt, pos)
get_codeunit(dat::AbstractVector{<:CodeUnitTypes}, pos) = dat[pos]
get_codeunit(str::Str, pos) = get_codeunit(_pnt(str), pos)

codeunit(str::Str, pos::Integer) = get_codeunit(str, pos)

set_codeunit!(pnt::Ptr{<:CodeUnitTypes}, ch, pos) = unsafe_store!(pnt, ch, pos)
set_codeunit!(dat::AbstractVector{<:CodeUnitTypes}, ch, pos) = (dat[pos] = ch)

get_unaligned32(pnt::Ptr{UInt8}) =
    unsafe_load(pnt+3)%UInt32<<24 | unsafe_load(pnt+2)%UInt32<<16 |
    unsafe_load(pnt+1)%UInt32<<8 | unsafe_load(pnt)

get_unaligned16(pnt::Ptr{UInt8}) = unsafe_load(pnt+1)%UInt16<<8 | unsafe_load(pnt)

get_swapped32(pnt::Ptr{UInt8}) =
    unsafe_load(pnt)%UInt32<<24 | unsafe_load(pnt+1)%UInt32<<16 |
    unsafe_load(pnt+2)%UInt32<<8 | unsafe_load(pnt+3)

get_swapped16(pnt::Ptr{UInt8}) = unsafe_load(pnt)%UInt16<<8 | unsafe_load(pnt+1)

_lenpnt(s::Union{String, ByteStr, WordStr, UTF32Str}) = _len(s), _pnt(s)
_lendata(s::Union{String, ByteStr}) = _len(s), _data(s)

function _allocate(::Type{T}, len) where {T <: CodeUnitTypes}
    buf = _sv(len * sizeof(T))
    buf, reinterpret(Ptr{T}, pointer(buf))
end

function _cvtsize(::Type{T}, dat, len) where {T <: CodeUnitTypes}
    buf, pnt = _allocate(T, len)
    @inbounds for i = 1:len ; set_codeunit!(pnt, get_codeunit(dat, i), i) ; end
    buf
end

const empty_string = ""
const empty_strvec = _sv(0)
const empty_binary = BinaryStr(empty_strvec)
const empty_ascii  = ASCIIStr(empty_strvec)
const empty_latin  = LatinStr(empty_strvec)
const empty_utf8   = UTF8Str(empty_strvec)
const empty_ucs2   = UCS2Str(empty_strvec)
const empty_utf16  = UTF16Str(empty_strvec)
const empty_utf32  = UTF32Str(empty_strvec)

empty_str(::Type{String})    = empty_string
empty_str(::Type{BinaryStr}) = empty_binary
empty_str(::Type{ASCIIStr})  = empty_ascii
empty_str(::Type{LatinStr})  = empty_latin
empty_str(::Type{UTF8Str})   = empty_utf8
empty_str(::Type{UCS2Str})   = empty_ucs2
empty_str(::Type{UTF16Str})  = empty_utf16
empty_str(::Type{UTF32Str})  = empty_utf32

endof(s::DirectIndexStr) = _len(s)
@inline getindex(s::DirectIndexStr, i::Int) = Char(get_codeunit(_pnt(s), ind2chr(s, i)))

# Need to optimize these to avoid doing array reinterpret
getindex(s::T, r::Vector) where {T<:DirectIndexStr} = T(getindex(_data(s), r))
getindex(s::T, r::UnitRange{Int}) where {T<:DirectIndexStr} = T(getindex(_data(s), r))
getindex(s::T, indx::AbstractVector{Int}) where {T<:DirectIndexStr} = T(_data(s)[indx])

next(s::DirectIndexStr, i::Int) = (s[i], i+1)

length(s::DirectIndexStr) = endof(s)

isvalid(s::DirectIndexStr, i::Integer) = (start(s) <= i <= endof(s))

prevind(s::DirectIndexStr, i::Integer) = Int(i) - 1
nextind(s::DirectIndexStr, i::Integer) = Int(i) + 1

function prevind(s::DirectIndexStr, i::Integer, nchar::Integer)
    nchar > 0 || throw(ArgumentError("nchar must be greater than 0"))
    Int(i) - nchar
end

function nextind(s::DirectIndexStr, i::Integer, nchar::Integer)
    nchar > 0 || throw(ArgumentError("nchar must be greater than 0"))
    Int(i) + nchar
end

ind2chr(s::DirectIndexStr, i::Integer) = begin checkbounds(s, i); i end
chr2ind(s::DirectIndexStr, i::Integer) = begin checkbounds(s, i); i end

length(s::SubString{<:DirectIndexStr}) = endof(s)

isvalid(s::SubString{<:DirectIndexStr}, i::Integer) = (start(s) <= i <= endof(s))

ind2chr(s::SubString{<:DirectIndexStr}, i::Integer) = begin checkbounds(s,i); i end
chr2ind(s::SubString{<:DirectIndexStr}, i::Integer) = begin checkbounds(s,i); i end

reverseind(s::Union{DirectIndexStr, SubString{<:DirectIndexStr}}, i::Integer) =
    length(s) + 1 - i

include("iters.jl")
include("support.jl")
include("ascii.jl")
include("latin.jl")
include("utf8.jl")
include("utf16.jl")
include("utf32.jl")

end # module Strs
