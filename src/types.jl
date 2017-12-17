#=
Basic types for characters and strings

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#
export Str, UniStr, BinaryStr, ASCIIStr, LatinUStr, LatinStr, UTF8Str
export UCS2Str, UTF16Str, UTF32Str
export RawByteStr, RawWordStr, RawCharStr
export CodePoint, ASCIIChr, LatinChr, UCS2Chr, UTF32Chr, RawByte, RawWord, RawChar

struct Str{T} <: AbstractString
    data::Vector{UInt8}
end

const CodeUnitTypes = Union{UInt8, UInt16, UInt32}

abstract type CodePoint <: AbstractChar end

primitive type ASCIIChr  <: CodePoint  8 end
primitive type LatinChr  <: CodePoint  8 end
primitive type LatinUChr <: CodePoint  8 end
primitive type UCS2Chr   <: CodePoint 16 end
primitive type UTF32Chr  <: CodePoint 32 end

primitive type RawByte  <: CodePoint  8 end
primitive type RawWord  <: CodePoint 16 end
primitive type RawChar  <: CodePoint 32 end

const CodePointTypes = Union{CodeUnitTypes, CodePoint}

const LatinChars   = Union{LatinChr, LatinUChr}
const ByteChars    = Union{ASCIIChr, LatinChr, LatinUChr, RawByte}
const WideChars    = Union{UCS2Chr, UTF32Chr}
const UnicodeChars = Union{ASCIIChr, LatinChars, UCS2Chr, UTF32Chr}

const RawByteStr = Str{:RawByte}
const RawWordStr = Str{:RawWord}
const RawCharStr = Str{:RawChar}
const BinaryStr = Str{:Binary}
const ASCIIStr  = Str{:ASCII}
const LatinStr  = Str{:Latin}
const LatinUStr = Str{:LatinU}
const UTF8Str   = Str{:UTF8}
const UCS2Str   = Str{:UCS2}
const UTF16Str  = Str{:UTF16}
const UTF32Str  = Str{:UTF32}

const ByteStr = Union{RawByteStr, BinaryStr, ASCIIStr, LatinStr, LatinUStr, UTF8Str}
const WordStr = Union{RawWordStr, UCS2Str, UTF16Str} # 16-bit code units
const QuadStr = Union{RawCharStr, UTF32Str} # 32-bit code units
const WideStr = Union{UCS2Str, UTF16Str, UTF32Str}

"""Union type for fast dispatching"""
const UniStr = Union{ASCIIStr, LatinUStr, UCS2Str, UTF32Str}

# These should be done via traits
const UnicodeByteStrings = Union{ASCIIStr, LatinStr, LatinUStr}
const LatinStrings   = Union{LatinStr,LatinUStr}
const ByteStrings    = Union{RawByteStr, BinaryStr, UnicodeByteStrings}
const UnicodeStrings = Union{String, UTF8Str, UTF16Str, UTF32Str}

const validtypes = (:Binary, :ASCII, :Latin, :LatinU, :UTF8, :UCS2, :UTF16, :UTF32,
                    :RawByte, :RawChar, :RawWord)

_allocate(len) = Base.StringVector(len)

function _allocate(::Type{T}, len) where {T <: CodeUnitTypes}
    buf = _allocate(len * sizeof(T))
    buf, reinterpret(Ptr{T}, pointer(buf))
end

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

