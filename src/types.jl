#=
Basic types for characters and strings

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md

Portion of encodings from collaboration on StringEncodings.jl with @nalimilan
=#
export Str, UniStr, BinaryStr, ASCIIStr, LatinStr, UTF8Str, UCS2Str, UTF16Str, UTF32Str
export RawByteStr, RawWordStr, RawCharStr
export CodePoint, ASCIIChr, LatinChr, UCS2Chr, UTF32Chr, RawByte, RawWord, RawChar
export CharSet, Encoding, @cs_str, @enc_str, @cse, charset, encoding
export BIG_ENDIAN, LITTLE_ENDIAN

const BIG_ENDIAN    = (ENDIAN_BOM == 0x01020304)
const LITTLE_ENDIAN = !BIG_ENDIAN

const STR_DATA_VECTOR = true

struct CharSet{CS}   end
struct Encoding{Enc} end
struct CSE{CS, ENC}  end

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
    :(CSE{CharSet{$(Expr(:quote, Symbol(cs)))}(),
          Encoding{$(Expr(:quote, Symbol(e)))}()})
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

# These are to indicate string types that must have at least one character of the type,
# for the internal types to make up the UniStr union type

const LatinSubSet    = CharSet{:LatinSubSet}   # Has at least 1 character > 0x7f, all <= 0xff
const UCS2SubSet     = CharSet{:UCS2SubSet}    # Has at least 1 character > 0xff, all <= 0xffff
const UnicodeSubSet  = CharSet{:UnicodeSubSet} # Has at least 1 non-BMP character in string

const NoEncoding    = Encoding(:None)
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

const _CSE{U} = Union{CharSet{U}, Encoding{U}} where {U}

#=
convert(::Type{T}, ::S) where {T<:AbstractString, S<:_CSE{U}} where {U} = T(U)
(convert(::Type{T}, ::CSE{CS,E})
    where {T<:AbstractString, CS<:CharSet{S}, E<:Encoding{U}}
    where {S,U}) =
        T(string("CSE{",S, ",", U,"}"))
=#

print(io::IO, ::S) where {S<:_CSE{U}} where {U} =
    print(io, U)
print(io::IO, ::CSE{CS,E}) where {CS<:CharSet{S},E<:Encoding{U}} where {S,U} =
    print(io, "CSE{", string(S), ",", string(U), "}()")

show(io::IO, ::Type{CharSet{S}}) where {S}   = print(io, "CharSet{", string(S), "}")
show(io::IO, ::Type{Encoding{S}}) where {S}  = print(io, "Encoding{", string(S), "}")
show(io::IO, ::Type{CSE{CS,E}}) where {CS<:CharSet{S},E<:Encoding{T}} where {S,T} =
    print(io, "CSE{", string(S), ", ", string(T), "}")

show(io::IO, ::Type{NoEncoding})   = print(io, "None")
show(io::IO, ::Type{UTF8Encoding}) = print(io, "UTF-8")
show(io::IO, ::Type{NativeUTF16})  = print(io, "UTF-16")
show(io::IO, ::Type{Native2Byte})  = print(io, "16-bit")
show(io::IO, ::Type{Native4Byte})  = print(io, "32-bit")
show(io::IO, ::Type{SwappedUTF16}) = print(io, "UTF-16", BIG_ENDIAN ? "LE" : "BE")
show(io::IO, ::Type{Swapped2Byte}) = print(io, "16-bit ", BIG_ENDIAN ? "LE" : "BE")
show(io::IO, ::Type{Swapped4Byte}) = print(io, "32-bit ", BIG_ENDIAN ? "LE" : "BE")

# Note: this is still in transition to expressing character set, encoding
# and optional cached info for hashes, UTF-8/UTF-16 encodings, subsets, etc.
# via more type parameters

const STR_DATA_TYPE = STR_DATA_VECTOR ? Vector{UInt8} : String

struct Str{T,SubStr,Cache,Hash} <: AbstractString
    data::STR_DATA_TYPE
    substr::SubStr
    cache::Cache
    hash::Hash

    ((::Type{Str})(::CSE_T, v::STR_DATA_TYPE)
        where {CSE_T<:CSE} =
      new{CSE_T,Nothing,Nothing,Nothing}(v,nothing,nothing,nothing))
    ((::Type{Str})(::Type{CSE_T}, v::STR_DATA_TYPE)
        where {CSE_T<:CSE} =
      new{CSE_T,Nothing,Nothing,Nothing}(v,nothing,nothing,nothing))
end
(::Type{STR})(v::STR_DATA_TYPE) where {STR<:Str{T,S,C,H}} where {T<:CSE,S,C,H} = Str(T, v)

#=
(convert(::Type{T}, ::Type{STR})
 where {T<:AbstractString, STR<:Str{CSEnc,S,C,H}}
 where {CSEnc<:CSE{CS,E}, S, C, H}
 where {CS<:CharSet{ST}, E<:Encoding{ENC}}
 where {ST, ENC}) =
     T(string("Str{CSE{CharSet{",ST,",},Encoding{",ENC,"}},",S,",",C,",",H,"}"))
=#

#show(io::IO, ::Type{Str{T,Nothing,Nothing,Nothing}}) where {T<:CSE} = string("Str{",T,"}")
#show(io::IO, ::Type{Str{T,S,C,H}}) where {T<:CSE,S,C,H} = string("Str{",T,",",S,",",C,",",H,"}")

# This needs to be redone, with character sets and the code unit as part of the type

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

const RawByteCSE = CSE{Text1CharSet,   NoEncoding}
const RawWordCSE = CSE{Text2CharSet,   Native2Byte}
const RawCharCSE = CSE{Text4CharSet,   Native4Byte}
const BinaryCSE  = CSE{BinaryCharSet,  NoEncoding}
const ASCIICSE   = CSE{ASCIICharSet,   NoEncoding}
const LatinCSE   = CSE{LatinCharSet,   NoEncoding}
const UCS2CSE    = CSE{UCS2CharSet,    Native2Byte}
const UTF8CSE    = CSE{UnicodeCharSet, UTF8Encoding}
const UTF16CSE   = CSE{UnicodeCharSet, NativeUTF16}
const UTF32CSE   = CSE{UnicodeCharSet, Native4Byte}

const _LatinCSE  = CSE{LatinSubSet,    NoEncoding}
const _UCS2CSE   = CSE{UCS2SubSet,     Native2Byte}
const _UTF32CSE  = CSE{UnicodeSubSet,  Native4Byte}

for nam in BuiltInTypes
    sym = Symbol("$(nam)Str")
    cse = Symbol("$(nam)CSE")
    @eval const $sym = Str{$cse, Nothing, Nothing, Nothing}
end

"""Union type for fast dispatching"""
const UniStr = Union{ASCIIStr, _LatinStr, _UCS2Str, _UTF32Str}

#show(io::IO, str::Type{<:Str}) = print(io, convert(String, str))
#=
for nam in BuiltInTypes
    sym = Symbol("$(nam)Str")
    @eval show(io::IO, ::Type{$sym}) = print(io, $sym)
end
=#

show(io::IO, ::Type{RawByteStr}) = print(io, :RawByteStr)
show(io::IO, ::Type{RawWordStr}) = print(io, :RawWordStr)
show(io::IO, ::Type{RawCharStr}) = print(io, :RawCharStr)
show(io::IO, ::Type{BinaryStr})  = print(io, :BinaryStr)
show(io::IO, ::Type{ASCIIStr})   = print(io, :ASCIIStr)
show(io::IO, ::Type{LatinStr})   = print(io, :LatinStr)
show(io::IO, ::Type{UTF8Str})    = print(io, :UTF8Str)
show(io::IO, ::Type{UCS2Str})    = print(io, :UCS2Str)
show(io::IO, ::Type{UTF16Str})   = print(io, :UTF16Str)
show(io::IO, ::Type{UTF32Str})   = print(io, :UTF32Str)

show(io::IO, ::Type{_LatinStr})  = print(io, :_LatinStr)
show(io::IO, ::Type{_UCS2Str})   = print(io, :_UCS2Str)
show(io::IO, ::Type{_UTF32Str})  = print(io, :_UTF32Str)

show(io::IO, ::Type{UniStr})     = print(io, :UniStr)

if STR_DATA_VECTOR
    _allocate(len) = Vector{UInt8}(uninitialized, len)
else
    _allocate(len) = Base._string_n((len-1)%Csize_t)
end

function _allocate(::Type{T}, len) where {T <: CodeUnitTypes}
    buf = _allocate(len * sizeof(T))
    buf, reinterpret(Ptr{T}, pointer(buf))
end

const empty_string = ""
if STR_DATA_VECTOR
    const empty_strvec = _allocate(0)
else
    const empty_strvec = empty_string
end
const empty_binary = Str(BinaryCSE, empty_strvec)
const empty_ascii  = Str(ASCIICSE,  empty_strvec)
const empty_latin  = Str(LatinCSE,  empty_strvec)
const empty_utf8   = Str(UTF8CSE,   empty_strvec)
const empty_ucs2   = Str(UCS2CSE,   empty_strvec)
const empty_utf16  = Str(UTF16CSE,  empty_strvec)
const empty_utf32  = Str(UTF32CSE,  empty_strvec)

const empty__latin = Str(_LatinCSE, empty_strvec)
const empty__ucs2  = Str(_UCS2CSE,  empty_strvec)
const empty__utf32 = Str(_UTF32CSE, empty_strvec)

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

# These should be done via traits
const RawStrings     = Union{RawByteStr, RawWordStr, RawCharStr}
const LatinStrings   = Union{LatinStr, _LatinStr}
const UCS2Strings    = Union{UCS2Str,  _UCS2Str}
const UTF32Strings   = Union{UTF32Str, _UTF32Str}

const UnicodeByteStrings = Union{ASCIIStr, LatinStrings}
const ByteStrings    = Union{RawByteStr, BinaryStr, UnicodeByteStrings}
const UnicodeStrings = Union{String, UTF8Str, UTF16Str, UTF32Strings}

## Get the character set / encoding used by a string type

charset(::Type{<:AbstractString})  = UniPlusCharSet
charset(::Type{T}) where {T<:Str{C}} where {C<:CSE{CS,E}} where {CS,E} = CS

encoding(::Type{<:AbstractString}) = UTF8Encoding # Julia likes to think of this as the default
encoding(::Type{T}) where {T<:Str{C}} where {C<:CSE{CS,E}} where {CS,E} = E

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
if STR_DATA_VECTOR
    _data(s::String)  = Vector{UInt8}(s)
    _data(s::ByteStr) = s.data
    _data(s::WordStr) = reinterpret(Vector{UInt16}, s.data)
    _data(s::QuadStr) = reinterpret(Vector{UInt32}, s.data)
else
    _data(s::String)  = s
    #_data(s::ByteStr) = s.data
    _data(s::ByteStr) = Vector{UInt8}(s.data)
    _data(s::WordStr) = reinterpret(Vector{UInt16}, Vector{UInt8}(s.data))
    _data(s::QuadStr) = reinterpret(Vector{UInt32}, Vector{UInt8}(s.data))
end

"""Pointer to codeunits of string"""
_pnt(s::Union{String,Vector{UInt8}}) = pointer(s)
_pnt(s::ByteStr) = pointer(s.data)
_pnt(s::WordStr) = reinterpret(Ptr{UInt16}, pointer(s.data))
_pnt(s::QuadStr) = reinterpret(Ptr{UInt32}, pointer(s.data))

const CHUNKSZ = sizeof(UInt64) # used for fast processing of strings

_pnt64(s::Union{String,Vector{UInt8}}) = reinterpret(Ptr{UInt64}, pointer(s))
_pnt64(s::Str) = reinterpret(Ptr{UInt64}, pointer(s.data))

"""Length of string in codeunits"""
_len(s::Vector{UInt8}) = sizeof(s)
_len(s::String)  = sizeof(s)
_len(s::ByteStr) = sizeof(s.data)
_len(s::WordStr) = sizeof(s.data) >>> 1
_len(s::QuadStr) = sizeof(s.data) >>> 2

# For convenience
@inline _sizpnt64(s) = sizeof(s), _pnt64(s)
@inline _lenpnt(s) = _len(s), _pnt(s)
@inline _lendata(s::Union{String, ByteStr, Vector{UInt8}}) = _len(s), _data(s)

@inline function _calcpnt(str, siz)
    pnt = _pnt64(str)
    pnt - CHUNKSZ, pnt + siz
end

@inline _mask_bytes(n) = (1%UInt << ((n & (CHUNKSZ - 1)) << 3)) - 0x1

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

