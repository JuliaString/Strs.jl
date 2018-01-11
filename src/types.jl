#=
Basic types for characters and strings

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md

Encodings inspired from collaborations on the following packages:
https://github.com/quinnj/Strings.jl with @quinnj (Jacob Quinn)
https://github.com/nalimilan/StringEncodings.jl with @nalimilan (Milan Bouchet-Valat)
=#
export Str, UniStr, CodePoint, CharSet, Encoding, @cs_str, @enc_str, @cse, charset, encoding
export BIG_ENDIAN, LITTLE_ENDIAN

const BIG_ENDIAN    = (ENDIAN_BOM == 0x01020304)
const LITTLE_ENDIAN = !BIG_ENDIAN

const STR_DATA_VECTOR = true

symstr(s...) = Symbol(string(s...))
quotesym(s...) = Expr(:quote, symstr(s...))

struct CharSet{CS}   end
struct Encoding{Enc} end
struct CSE{CS, ENC}  end

CharSet(s)  = CharSet{Symbol(s)}()
Encoding(s) = Encoding{Symbol(s)}()
CSE(cs, e)  = CSE{CharSet(cs), Encoding(e)}()

macro cs_str(s)
    :(CharSet{$(quotesym(s))}())
end
macro enc_str(s)
    :(Encoding{$(quotesym(s))}())
end
macro cse(cs, e)
    :(CSE{CharSet{$(quotesym(cs)), $(quotesym(e))}()})
end

const charsets =
    (:Binary,  # really, no character set at all, not text
     :ASCII,   # (7-bit subset of Unicode)
     :Latin,   # ISO-8859-1 (8-bit subset of Unicode)
     :UCS2,    # BMP (16-bit subset of Unicode)
     :UTF32,   # corresponding to codepoints (0-0xd7ff, 0xe000-0x10fff)
     :UniPlus, # valid Unicode, plus unknown characters (for String)
     :Text1,   # Unknown character set, 1 byte
     :Text2,   # Unknown character set, 2 byte
     :Text4)   # Unknown character set, 4 byte

const BinaryCharSet  = CharSet{:Binary}  # really, no character set at all, not text
for nam in charsets
    @eval const $(symstr(nam, "CharSet")) = CharSet{$(quotesym(nam))}
end

# These are to indicate string types that must have at least one character of the type,
# for the internal types to make up the UniStr union type

const LatinSubSet  = CharSet{:LatinSubSet} # Has at least 1 character > 0x7f, all <= 0xff
const UCS2SubSet   = CharSet{:UCS2SubSet}  # Has at least 1 character > 0xff, all <= 0xffff
const UTF32SubSet  = CharSet{:UTF32SubSet} # Has at least 1 non-BMP character in string

const Native1Byte  = Encoding(:Byte)
const NativeUTF8   = Encoding(:UTF8)
@eval show(io::IO, ::Type{NativeUTF8})  = print(io, "UTF-8")
@eval show(io::IO, ::Type{Native1Byte}) = print(io, "8-bit")

for (n, l, b, s) in (("2Byte", :LE2, :BE2, "16-bit"),
                     ("4Byte", :LE4, :BE4, "32-bit"),
                     ("UTF16", :UTF16LE, :UTF16BE, "UTF-16"))
    nat, swp = BIG_ENDIAN ? (b, l) : (l, b)
    natnam = symstr("Native",  n)
    swpnam = symstr("Swapped", n)
    @eval const $natnam = Encoding($(quotesym("N", n)))
    @eval const $swpnam = Encoding($(quotesym("S", n)))
    @eval const $nat = $natnam
    @eval const $swp = $swpnam
    @eval show(io::IO, ::Type{$natnam}) = print(io, $s)
    @eval show(io::IO, ::Type{$swpnam}) = print(io, $(string(s, " ", BIG_ENDIAN ? "LE" : "BE")))
end

const _CSE{U} = Union{CharSet{U}, Encoding{U}} where {U}

print(io::IO, ::S) where {S<:_CSE{U}} where {U} =
    print(io, U)
print(io::IO, ::CSE{CS,E}) where {CS<:CharSet{S},E<:Encoding{U}} where {S,U} =
    print(io, "CSE{", string(S), ",", string(U), "}()")

show(io::IO, ::Type{CharSet{S}}) where {S}   = print(io, "CharSet{", string(S), "}")
show(io::IO, ::Type{Encoding{S}}) where {S}  = print(io, "Encoding{", string(S), "}")
show(io::IO, ::Type{CSE{CS,E}}) where {CS<:CharSet{S},E<:Encoding{T}} where {S,T} =
    print(io, "CSE{", string(S), ", ", string(T), "}")

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
#(::Type{STR})(v::STR_DATA_TYPE) where {STR<:Str{T,S,C,H}} where {T<:CSE,S,C,H} = Str(T, v)

# This needs to be redone, with character sets and the code unit as part of the type

const CodeUnitTypes = Union{UInt8, UInt16, UInt32}

abstract type CodePoint <: AbstractChar end

const _cpname1 = [:Text1, :ASCII, :Latin]
const _cpname2 = [:Text2, :UCS2]
const _cpname4 = [:Text4, :UTF32]
const _subsetnam = [:_Latin, :_UCS2, :_UTF32]
const _mbwname   = [:UTF8, :UTF16] # Multi-byte/word

for (names, siz) in ((_cpname1, 8), (_cpname2, 16), (_cpname4, 32)), nam in names
    @eval primitive type $(symstr(nam, "Chr")) <: CodePoint $siz end
end
primitive type _LatinChr <: CodePoint 8 end

const CodePointTypes = Union{CodeUnitTypes, CodePoint}

const LatinChars   = Union{LatinChr, _LatinChr}
const ByteChars    = Union{ASCIIChr, LatinChr, _LatinChr, Text1Chr}
const WideChars    = Union{UCS2Chr, UTF32Chr}
const UnicodeChars = Union{ASCIIChr, LatinChars, UCS2Chr, UTF32Chr}

const BuiltInTypes = vcat(_cpname1, _cpname2, _cpname4, _subsetnam, _mbwname)

const BinaryCSE = CSE{BinaryCharSet,  Native1Byte}

const _encnam1 = [:Text1, :Binary, :ASCII, :Latin]

for (cs, enc) in ((Native1Byte, _encnam1), (Native2Byte, _cpname2), (Native4Byte, _cpname4)),
    nam in enc
    @eval const $(symstr(nam, "CSE")) = CSE{$(symstr(nam, "CharSet")), $cs}
end
const UTF8CSE   = CSE{UTF32CharSet, NativeUTF8}
const UTF16CSE  = CSE{UTF32CharSet, NativeUTF16}

const _LatinCSE = CSE{LatinSubSet,  Native1Byte}
const _UCS2CSE  = CSE{UCS2SubSet,   Native2Byte}
const _UTF32CSE = CSE{UTF32SubSet,  Native4Byte}

for nam in BuiltInTypes
    sym = Symbol("$(nam)Str")
    cse = Symbol("$(nam)CSE")
    @eval const $sym = Str{$cse, Nothing, Nothing, Nothing}
    @eval show(io::IO, ::Type{$sym}) = print(io, $(quotesym(sym)))
end

"""Union type for fast dispatching"""
const UniStr = Union{ASCIIStr, _LatinStr, _UCS2Str, _UTF32Str}
show(io::IO, ::Type{UniStr}) = print(io, :UniStr)

if STR_DATA_VECTOR
    _allocate(len) = Vector{UInt8}(uninitialized, len)
else
    _allocate(len) = Base._string_n((len-1)%Csize_t)
end

function _allocate(::Type{T}, len) where {T <: CodeUnitTypes}
    buf = _allocate(len * sizeof(T))
    buf, reinterpret(Ptr{T}, pointer(buf))
end

const list = [(:ASCII, :ascii), (:Latin, :latin), (:UCS2,  :ucs2), (:UTF32, :utf32),
              (:UTF8,  :utf8), (:UTF16, :utf16), (:Binary, :binary)]
const sublist = [(:_Latin, :_latin), (:_UCS2, :_ucs2), (:_UTF32, :_utf32)]

const empty_string = ""
if STR_DATA_VECTOR
    const empty_strvec = _allocate(0)
else
    const empty_strvec = empty_string
end
empty_str(::Type{String}) = empty_string

for (nam, low) in vcat(list, sublist)
    sym = symstr(nam, "Str")
    @eval const $sym = Str{$(symstr(nam, "CSE")), Nothing,  Nothing, Nothing}
    @eval const $(symstr("empty_", low)) = Str($(symstr(nam, "CSE")), empty_strvec)
    @eval const empty_str(::Type{$sym}) = $(symstr("empty_", low))
    @eval (::Type{$sym})(v::Vector{UInt8}) = convert($sym, v)
end
for val in list ; @eval export $(symstr(val[1], "Str")) ; end

@inline _convert(::Type{T}, a::Vector{UInt8}) where {T<:Str} =
    Str(cse(T), copyto!(_allocate(sizeof(a)), a))

const ByteStr = Union{Text1Str, BinaryStr, ASCIIStr, LatinStr, _LatinStr, UTF8Str}
const WordStr = Union{Text2Str, UCS2Str, _UCS2Str, UTF16Str} # 16-bit code units
const QuadStr = Union{Text4Str, UTF32Str, _UTF32Str} # 32-bit code units
const WideStr = Union{UCS2Str, UTF16Str, UTF32Str, _UCS2Str, _UTF32Str}

# These should be done via traits
const RawStrings   = Union{Text1Str, Text2Str, Text4Str}
const LatinStrings = Union{LatinStr, _LatinStr}
const UCS2Strings  = Union{UCS2Str,  _UCS2Str}
const UTF32Strings = Union{UTF32Str, _UTF32Str}

const UnicodeByteStrings = Union{ASCIIStr, LatinStrings}
const ByteStrings        = Union{Text1Str, BinaryStr, UnicodeByteStrings}
const UnicodeStrings     = Union{String, UTF8Str, UTF16Str, UTF32Strings}

## Get the character set / encoding used by a string type
cse(::T) where {T<:Str{C}} where {C<:CSE} = C
cse(::Type{T}) where {T<:Str{C}} where {C<:CSE} = C

charset(::Type{<:AbstractString})  = UniPlusCharSet
charset(::Type{T}) where {T<:Str{C}} where {C<:CSE{CS,E}} where {CS,E} = CS

encoding(::Type{<:AbstractString}) = UTF8Encoding # Julia likes to think of this as the default
encoding(::Type{T}) where {T<:Str{C}} where {C<:CSE{CS,E}} where {CS,E} = E

promote_rule(::Type{T}, ::Type{T}) where {T<:CodePoint} = T
promote_rule(::Type{Text2Chr}, ::Type{Text1Chr}) = Text2Chr
promote_rule(::Type{Text4Chr}, ::Type{Text1Chr}) = Text4Chr
promote_rule(::Type{Text4Chr}, ::Type{Text2Chr}) = Text4Chr

promote_rule(::Type{T}, ::Type{ASCIIChr}) where {T} = T
promote_rule(::Type{LatinChr}, ::Type{_LatinChr}) = LatinChr
promote_rule(::Type{UTF32Chr}, ::Type{UCS2Chr}) = UTF32Chr
promote_rule(::Type{T}, ::Type{S}) where {T<:WideChars,S<:ByteChars} = T

promote_rule(::Type{T}, ::Type{T}) where {T<:Str} = T
promote_rule(::Type{Text2Str}, ::Type{Text1Str}) = Text2Str
promote_rule(::Type{Text4Str}, ::Type{Text1Str}) = Text4Str
promote_rule(::Type{Text4Str}, ::Type{Text2Str}) = Text4Str

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
    _data(s::String)  = unsafe_wrap(Vector{UInt8}, s)
    _data(s::ByteStr) = s.data
else
    _data(s::String)  = s
    _data(s::ByteStr) = unsafe_wrap(Vector{UInt8}, s.data)
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
_len(s) = sizeof(s)
_len(s::ByteStr) = sizeof(s.data)
_len(s::WordStr) = sizeof(s.data) >>> 1
_len(s::QuadStr) = sizeof(s.data) >>> 2

# For convenience
@inline _lenpnt(s) = _len(s), _pnt(s)
@inline _lendata(s::Union{String, ByteStr, Vector{UInt8}}) = _len(s), _data(s)

@inline _calcpnt(str, siz) = (pnt = _pnt64(str) - CHUNKSZ;  (pnt, pnt + siz))

@inline _mask_bytes(n) = (1%UInt << ((n & (CHUNKSZ - 1)) << 3)) - 0x1
