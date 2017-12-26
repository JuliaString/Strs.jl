#=
Basic types for characters and strings

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md

Portion of encodings from collaboration on StringEncodings.jl with @nalimilan
=#
export Str, UniStr, BinaryStr, ASCIIStr, LatinStr, UTF8Str, UCS2Str, UTF16Str, UTF32Str
export RawByteStr, RawWordStr, RawCharStr
export CodePoint, ASCIIChr, LatinChr, UCS2Chr, UTF32Chr, RawByte, RawWord, RawChar
export CharSet, Encoding, @cs_str, @enc_str, charset, encoding
export BIG_ENDIAN, LITTLE_ENDIAN

const BIG_ENDIAN    = (ENDIAN_BOM == 0x01020304)
const LITTLE_ENDIAN = !BIG_ENDIAN

struct CharSet{CS}   end
struct Encoding{Enc} end
struct CSE{CS<:CharSet,Enc<:Encoding} end

CharSet(s)  = CharSet{Symbol(s)}()
Encoding(s) = Encoding{Symbol(s)}()
CSE(cs, e)  = CSE{CharSet(cs), Encoding(e)}()

macro cs_str(s)
    :(CharSet{$(Expr(:quote, Symbol(s)))}())
end
macro enc_str(s)
    :(Encoding{$(Expr(:quote, Symbol(s)))}())
end
macro cse(cs, e)
    :(CSE{$(cs_str(cs)), $(enc_str(e))}())
end

const BinaryCharSet  = CharSet{:Binary}  # really, no character set at all, not text
const ASCIICharSet   = CharSet{:ASCII}   # (7-bit subset of Unicode)
const LatinCharSet   = CharSet{:Latin}   # ISO-8859-1 (8-bit subset of Unicode)
const UCS2CharSet    = CharSet{:UCS2}    # BMP (16-bit subset of Unicode)
const UnicodeCharSet = CharSet{:Unicode} # corresponding to codepoints (0-0xd7ff, 0xe000-0x10fff)
const UniPlusCharSet = CharSet{:UniPlus} # valid Unicode, plus unknown characters (for String)
const Text1CharSet   = CharSet{:Text1}   # Unknown character set, for RawByteChr/RawByteStr
const Text2CharSet   = CharSet{:Text2}   # Unknown character set, for RawWordChr/RawWordStr
const Text4CharSet   = CharSet{:Text4}   # Unknown character set, for RawCharChr/RawCharStr

const _CSE{U} = Union{CharSet{U}, Encoding{U}, CSE{U}} where {U}

convert(::Type{T}, ::S) where {T<:AbstractString,S<:_CSE{U}} where {U} = T(U)
print(io::IO, ::S) where {T<:AbstractString,S<:_CSE{U}} where {U} = print(io, U)

show(io::IO, ::CharSet{S}) where {S}  = print(io, string(S), " character set")
show(io::IO, ::Encoding{S}) where {S} = print(io, string(S), " encoding")
show(io::IO, ::CSE{S}) where {S}      = print(io, string(S), " charset encoding")

# Note: this is still in transition to expressing character set, encoding
# and optional cached info for hashes, UTF-8/UTF-16 encodings, subsets, etc.
# via more type parameters

struct Str{T} <: AbstractString
    data::Vector{UInt8}
    (::Type{Str})(S::Symbol, v::Vector{UInt8}) = new{S}(v)
end
(::Type{Str{T}})(v::Vector{UInt8}) where {T} = Str(T, v)

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

const BuiltInTypes = (:RawByte, :RawWord, :RawChar,
                      :Binary, :ASCII, :Latin, :UTF8, :UCS2, :UTF16, :UTF32,
                      :_Latin, :_UCS2, :_UTF32)

for nam in BuiltInTypes
    sym = Symbol("$(nam)Str")
    @eval const $sym = Str{$(Expr(:quote, Symbol(nam)))}
end
#=
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
=#

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

const NoEncoding    = Encoding("")
const UTF8Encoding  = Encoding(:UTF8)

const Native2Byte   = Encoding(:2)
const Native4Byte   = Encoding(:4)
const NativeUTF16   = Encoding(:UTF16)

const Swapped2Byte  = Encoding(:S2)
const Swapped4Byte  = Encoding(:S4)
const SwappedUTF16  = Encoding(:SUTF16)

@static if BIG_ENDIAN
    const LE2       = Native2Byte
    const LE4       = Native4Byte
    const BE2       = Swapped2Byte
    const BE4       = Swapped4Byte
    const UTF16LE   = NativeUTF16
    const UTF16BE   = SwappedUTF16
else
    const LE2       = Swapped2Byte
    const LE4       = Swapped4Byte
    const BE2       = Native2Byte
    const BE4       = Native4Byte
    const UTF16LE   = SwappedUTF16
    const UTF16BE   = NativeUTF16
end

## Get the character set / encoding used by a string type

charset(::Type{BinaryStr})    = BinaryCharSet
charset(::Type{ASCIIStr})     = ASCIICharSet
charset(::Type{LatinStrings}) = LatinCharSet
charset(::Type{UCS2Strings})  = UCS2CharSet
charset(::Type{T}) where {T<:Union{UTF8Str, UTF16Str, UTF32Strings}} = UnicodeCharSet
charset(::Type{RawByteStr})   = Text1CharSet
charset(::Type{RawWordStr})   = Text2CharSet
charset(::Type{RawCharStr})   = Text4CharSet
charset(::Type{String})       = UniPlusCharSet

## Get the (default) encoding used by a string type

encoding(::Type{<:AbstractString}) = UTF8Encoding # Julia likes to think of this as the default
encoding(::Type{<:Str})       = NoEncoding
encoding(::Type{UCS2Strings}) = Native2Byte
encoding(::Type{UTF8Str})     = UTF8Encoding
encoding(::Type{UTF16Str})    = NativeUTF16Encoding
encoding(::Type{UTF32Str})    = Native4Byte
encoding(::Type{RawWordStr})  = Native2Byte
encoding(::Type{RawCharStr})  = Native4Byte

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

function Str(v::Vector{UInt8})
    siz = sizeof(v)
    buf = _allocate(siz)
    @inbounds copyto!(buf, v, siz)
    RawByteStr(buf)
end
function Str(v::Vector{UInt16})
    len = length(v)
    buf, pnt = _allocate(UInt16, v)
    @inbounds unsafe_copyto!(pnt, v, len)
    RawWordStr(buf)
end
function Str(v::Vector{UInt32})
    len = length(v)
    buf, pnt = _allocate(UInt32, len)
    @inbounds unsafe_copyto!(buf, v, len)
    RawCharStr(buf)
end
