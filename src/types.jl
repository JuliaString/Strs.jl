#=
Basic types for characters and strings

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#
export Str, UniStr, BinaryStr, ASCIIStr, LatinStr, UTF8Str, UCS2Str, UTF16Str, UTF32Str
export RawByteStr, RawWordStr, RawCharStr
export CodePoint, ASCIIChr, LatinChr, UCS2Chr, UTF32Chr, RawByte, RawWord, RawChar

struct Str{T} <: AbstractString
    data::Vector{UInt8}
end

const CodeUnitTypes = Union{UInt8, UInt16, UInt32}

abstract type CodePoint <: AbstractChar end

primitive type ASCIIChr  <: CodePoint  8 end
primitive type LatinChr  <: CodePoint  8 end
primitive type UCS2Chr   <: CodePoint 16 end
primitive type UTF32Chr  <: CodePoint 32 end

primitive type _LatinChr <: CodePoint  8 end

primitive type RawByte   <: CodePoint  8 end
primitive type RawWord   <: CodePoint 16 end
primitive type RawChar   <: CodePoint 32 end

const CodePointTypes = Union{CodeUnitTypes, CodePoint}

const LatinChars   = Union{LatinChr, _LatinChr}
const ByteChars    = Union{ASCIIChr, LatinChr, _LatinChr, RawByte}
const WideChars    = Union{UCS2Chr, UTF32Chr}
const UnicodeChars = Union{ASCIIChr, LatinChars, UCS2Chr, UTF32Chr}

const RawByteStr = Str{:RawByte}
const RawWordStr = Str{:RawWord}
const RawCharStr = Str{:RawChar}
const BinaryStr = Str{:Binary}
const ASCIIStr  = Str{:ASCII}
const LatinStr  = Str{:Latin}
const UTF8Str   = Str{:UTF8}
const UCS2Str   = Str{:UCS2}
const UTF16Str  = Str{:UTF16}
const UTF32Str  = Str{:UTF32}

const _LatinStr = Str{:_Latin}
const _UCS2Str  = Str{:_UCS2}
const _UTF32Str = Str{:_UTF32}

_allocate(len) = Base.StringVector(len)

function _allocate(::Type{T}, len) where {T <: CodeUnitTypes}
    buf = _allocate(len * sizeof(T))
    buf, reinterpret(Ptr{T}, pointer(buf))
end

const empty_string = ""
const empty_strvec = _allocate(0)
const empty_binary = BinaryStr(empty_strvec)
const empty_ascii  = ASCIIStr(empty_strvec)
const empty_latin  = LatinStr(empty_strvec)
const empty_utf8   = UTF8Str(empty_strvec)
const empty_ucs2   = UCS2Str(empty_strvec)
const empty_utf16  = UTF16Str(empty_strvec)
const empty_utf32  = UTF32Str(empty_strvec)

const empty__latin = _LatinStr(empty_strvec)
const empty__ucs2  = _UCS2Str(empty_strvec)
const empty__utf32 = _UTF32Str(empty_strvec)

empty_str(::Type{String})    = empty_string
empty_str(::Type{BinaryStr}) = empty_binary
empty_str(::Type{ASCIIStr})  = empty_ascii
empty_str(::Type{UTF8Str})   = empty_utf8
empty_str(::Type{UCS2Str})   = empty_ucs2
empty_str(::Type{UTF16Str})  = empty_utf16
empty_str(::Type{UTF32Str})  = empty_utf32

empty_str(::Type{_LatinStr}) = empty__latin
empty_str(::Type{_UCS2Str})  = empty__ucs2
empty_str(::Type{_UTF32Str}) = empty__utf32

const ByteStr = Union{RawByteStr, BinaryStr, ASCIIStr, LatinStr, _LatinStr, UTF8Str}
const WordStr = Union{RawWordStr, UCS2Str, _UCS2Str, UTF16Str} # 16-bit code units
const QuadStr = Union{RawCharStr, UTF32Str, _UTF32Str} # 32-bit code units
const WideStr = Union{UCS2Str, UTF16Str, UTF32Str, _UCS2Str, _UTF32Str}

"""Union type for fast dispatching"""
const UniStr = Union{ASCIIStr, _LatinStr, _UCS2Str, _UTF32Str}

# These should be done via traits
const RawStrings     = Union{RawByteStr, RawWordStr, RawCharStr}
const LatinStrings   = Union{LatinStr, _LatinStr}
const UCS2Strings    = Union{UCS2Str,  _UCS2Str}
const UTF32Strings   = Union{UTF32Str, _UTF32Str}

const UnicodeByteStrings = Union{ASCIIStr, LatinStrings}
const ByteStrings    = Union{RawByteStr, BinaryStr, UnicodeByteStrings}
const UnicodeStrings = Union{String, UTF8Str, UTF16Str, UTF32Strings}

const validtypes = (:Binary, :ASCII, :Latin, :UTF8, :UCS2, :UTF16, :UTF32,
                    :_Latin, :_UCS2, :_UT32, :RawByte, :RawChar, :RawWord)

promote_rule(::Type{T}, ::Type{T}) where {T<:CodePoint} = T
promote_rule(::Type{RawWord}, ::Type{RawByte}) = RawWord
promote_rule(::Type{RawChar}, ::Type{RawByte}) = RawChar
promote_rule(::Type{RawChar}, ::Type{RawWord}) = RawChar

promote_rule(::Type{T}, ::Type{ASCIIChr}) where {T} = T
promote_rule(::Type{LatinChr}, ::Type{_LatinChr}) = LatinChr
promote_rule(::Type{UTF32Chr}, ::Type{UCS2Chr}) = UTF32Chr
promote_rule(::Type{T}, ::Type{S}) where {T<:WideChars,S<:ByteChars} = T


promote_rule(::Type{T}, ::Type{T}) where {T<:Str} = T
promote_rule(::Type{RawWordStr}, ::Type{RawByteStr}) = RawWordStr
promote_rule(::Type{RawCharStr}, ::Type{RawByteStr}) = RawCharStr
promote_rule(::Type{RawCharStr}, ::Type{RawWordStr}) = RawCharStr

promote_rule(::Type{T}, ::Type{ASCIIStr}) where {T<:Union{LatinStrings,UnicodeStrings,WideStr}} = T
promote_rule(::Type{T}, ::Type{LatinStrings}) where {T<:Union{UnicodeStrings,WideStr}} = T
promote_rule(::Type{T}, ::Type{UCS2Strings}) where {T<:Union{UTF32Strings}} = T

promote_rule(::Type{LatinStr}, ::Type{_LatinStr}) = LatinStr
promote_rule(::Type{UCS2Str}, ::Type{_UCS2Str})   = UCS2Str
promote_rule(::Type{UTF32Str}, ::Type{_UTF32Str}) = UTF32Str

function Str(T::Symbol, data::Vector{UInt8})
    T in validtypes || error("Not valid type")
    Str{T}(data)
end

sizeof(s::Str) = sizeof(s.data)

"""Codeunits of string as a Vector"""
_data(s::Vector{UInt8}) = s
_data(s::String)  = Vector{UInt8}(s)
_data(s::ByteStr) = s.data
_data(s::WordStr) = reinterpret(Vector{UInt16}, s.data)
_data(s::QuadStr) = reinterpret(Vector{UInt32}, s.data)

"""Pointer to codeunits of string"""
_pnt(s::Vector{UInt8}) = pointer(s)
_pnt(s::String)  = pointer(s)
_pnt(s::ByteStr) = pointer(s.data)
_pnt(s::WordStr) = reinterpret(Ptr{UInt16}, pointer(s.data))
_pnt(s::QuadStr) = reinterpret(Ptr{UInt32}, pointer(s.data))

"""Length of string in codeunits"""
_len(s::Vector{UInt8}) = sizeof(s)
_len(s::String)  = sizeof(s)
_len(s::ByteStr) = sizeof(s.data)
_len(s::WordStr) = sizeof(s.data) >>> 1
_len(s::QuadStr) = sizeof(s.data) >>> 2

_lenpnt(s::Union{String, ByteStr, WordStr, UTF32Str, Vector{UInt8}}) = _len(s), _pnt(s)
_lendata(s::Union{String, ByteStr, Vector{UInt8}}) = _len(s), _data(s)

