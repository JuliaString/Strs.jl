#=
Basic types for characters and strings

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md

Encodings inspired from collaborations on the following packages:
https://github.com/quinnj/Strings.jl with @quinnj (Jacob Quinn)
https://github.com/nalimilan/StringEncodings.jl with @nalimilan (Milan Bouchet-Valat)
=#
const BIG_ENDIAN    = (ENDIAN_BOM == 0x01020304)
const LITTLE_ENDIAN = !BIG_ENDIAN

const STR_KEEP_NUL    = true  # keep nul byte placed by String

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

# Define symbols used for characters, codesets, codepoints

const _cpname1 =
    [:Text1,   # Unknown character set, 1 byte
     :ASCII,   # (7-bit subset of Unicode)
     :Latin]   # ISO-8859-1 (8-bit subset of Unicode)
const _cpname2 =
    [:Text2,   # Unknown character set, 2 byte
     :UCS2]    # BMP (16-bit subset of Unicode)
const _cpname4 =
    [:Text4,   # Unknown character set, 4 byte
     :UTF32]   # corresponding to codepoints (0-0xd7ff, 0xe000-0x10fff)
const _subsetnam =
    [:_Latin,
     :_UCS2,
     :_UTF32]
const _mbwname =
    [:UTF8,
     :UTF16]
const _binname = vcat(_cpname1, :Binary)

# List of basic character sets
const charsets = vcat(_cpname1, _cpname2, _cpname4)

for nam in vcat(charsets,
                :Binary,   # really, no character set at all, not text
                :UniPlus)  # valid Unicode, plus unknown characters (for String)
    @eval const $(symstr(nam, "CharSet")) = CharSet{$(quotesym(nam))}
end

# These are to indicate string types that must have at least one character of the type,
# for the internal types to make up the UniStr union type

const LatinSubSet  = CharSet{:LatinSubSet} # Has at least 1 character > 0x7f, all <= 0xff
const UCS2SubSet   = CharSet{:UCS2SubSet}  # Has at least 1 character > 0xff, all <= 0xffff
const UTF32SubSet  = CharSet{:UTF32SubSet} # Has at least 1 non-BMP character in string

export Native1Byte, UTF8Encoding
const Native1Byte  = Encoding(:Byte)
const UTF8Encoding = Encoding(:UTF8)
@eval show(io::IO, ::Type{UTF8Encoding})  = print(io, "UTF-8")
@eval show(io::IO, ::Type{Native1Byte}) = print(io, "8-bit")


# Allow handling different endian encodings

for (n, l, b, s) in (("2Byte", :LE2, :BE2, "16-bit"),
                     ("4Byte", :LE4, :BE4, "32-bit"),
                     ("UTF16", :UTF16LE, :UTF16BE, "UTF-16"))
    nat, swp = BIG_ENDIAN ? (b, l) : (l, b)
    natnam = symstr("Native",  n)
    swpnam = symstr("Swapped", n)
    @eval export $nat, $swp
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
print(io::IO, ::CSE{CS,E}) where {S,U,CS<:CharSet{S},E<:Encoding{U}} =
    print(io, "CSE{", string(S), ",", string(U), "}()")

show(io::IO, ::Type{CharSet{S}}) where {S}   = print(io, "CharSet{", string(S), "}")
show(io::IO, ::Type{Encoding{S}}) where {S}  = print(io, "Encoding{", string(S), "}")
show(io::IO, ::Type{CSE{CS,E}}) where {S,T,CS<:CharSet{S},E<:Encoding{T}} =
    print(io, "CSE{", string(S), ", ", string(T), "}")

const CodeUnitTypes = Union{UInt8, UInt16, UInt32}

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

const MaybeSub{T} = Union{T, SubString{T}} where {T<:Str}

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

abstract type CodePoint <: AbstractChar end

for (names, siz) in ((_cpname1, 8), (_cpname2, 16), (_cpname4, 32)), nam in names
    chrnam = symstr(nam, "Chr")
    @eval primitive type $chrnam <: CodePoint $siz end
    @eval export $chrnam
end
primitive type _LatinChr <: CodePoint 8 end

# Define all of the function for mapping CodePoint types to a CharSet
for nam in charsets
    csnam = symstr(nam, "CharSet")
    @eval charset(::Type{$(symstr(nam, "Chr"))}) = $csnam
    @eval export $csnam
end

# Handle a few quirks
charset(::Type{<:AbstractChar}) = UTF32CharSet
charset(::Type{UInt8})     = BinaryCharSet  # UInt8 instead of "BinaryChr"
charset(::Type{Char})      = UniPlusCharSet # Char instead of "UniPlusChr"
charset(::Type{_LatinChr}) = LatinSubSet    # LatinSubSet instead of "_LatinCharSet"

export BinaryCharSet, UniPlusCharSet

const LatinChars   = Union{LatinChr, _LatinChr}
const ByteChars    = Union{ASCIIChr, LatinChr, _LatinChr, Text1Chr}
const WideChars    = Union{UCS2Chr, UTF32Chr}

# Definition of built-in CSEs (Character Set Encodings)

for (cs, enc) in ((Native1Byte, _binname), (Native2Byte, _cpname2), (Native4Byte, _cpname4)),
    nam in enc
    @eval const $(symstr(nam, "CSE")) = CSE{$(symstr(nam, "CharSet")), $cs}
end
const UTF8CSE   = CSE{UTF32CharSet, UTF8Encoding}
const UTF16CSE  = CSE{UTF32CharSet, NativeUTF16}

const _LatinCSE = CSE{LatinSubSet,  Native1Byte}
const _UCS2CSE  = CSE{UCS2SubSet,   Native2Byte}
const _UTF32CSE = CSE{UTF32SubSet,  Native4Byte}

export UniPlusCSE
const UniPlusCSE = CSE{UniPlusCharSet, UTF8Encoding}

for nam in vcat(charsets, :_Latin)
    @eval codepoint_cse(::Type{$(symstr(nam,"Chr"))}) = $(symstr(nam,"CSE"))
end
codepoint_cse(::Type{Char}) = UniPlusCSE

# Definition of built-in Str types

const empty_string = ""

for nam in vcat(_binname, _cpname2, _cpname4, _subsetnam, _mbwname)
    str = String(nam)
    sym = symstr(nam, "Str")
    cse = symstr(nam, "CSE")
    @eval const $sym = Str{$cse, Nothing, Nothing, Nothing}
    @eval (::Type{$sym})(v::Vector{UInt8}) = convert($sym, v)
    @eval show(io::IO, ::Type{$sym}) = print(io, $(quotesym(sym)))
    @eval show(io::IO, ::Type{$cse}) = print(io, $(quotesym(cse)))
    low = lowercase(str)
    if str[1] == '_'
        @eval empty_str(::Type{$cse}) = $(symstr("empty", low))
    else
        emp = symstr("empty_", low)
        @eval const $emp = Str($cse, empty_string)
        @eval empty_str(::Type{$cse}) = $emp
        @eval export $sym, $cse
    end
    @eval convert(::Type{$sym}, str::$sym) = str
end
empty_str(::Type{<:Str{C}}) where {C<:CSE} = empty_str(C)
empty_str(::Type{String}) = empty_string

typemin(::Type{T}) where {T<:Str} = empty_str(T)
typemin(::T) where {T<:Str} = empty_str(T)

"""Union type for fast dispatching"""
const UniStr = Union{ASCIIStr, _LatinStr, _UCS2Str, _UTF32Str}
show(io::IO, ::Type{UniStr}) = print(io, :UniStr)

_allocate(len) = Base._string_n((len+STR_KEEP_NUL-1)%Csize_t)

function _allocate(::Type{T}, len) where {T <: CodeUnitTypes}
    buf = _allocate((len+STR_KEEP_NUL-1) * sizeof(T))
    buf, reinterpret(Ptr{T}, pointer(buf))
end

# Various useful groups of character set types

# These should be done via traits
const Binary_CSEs   = Union{Text1CSE, BinaryCSE}
const Raw_CSEs      = Union{Text1CSE, Text2CSE, Text4CSE}
const Latin_CSEs    = Union{LatinCSE, _LatinCSE}
const UTF8_CSEs     = Union{UTF8CSE,  UniPlusCSE}
const UCS2_CSEs     = Union{UCS2CSE,  _UCS2CSE}
const UTF32_CSEs    = Union{UTF32CSE, _UTF32CSE}
const Unicode_CSEs  = Union{UTF8CSE, UTF16CSE, UTF32_CSEs}
const SubSet_CSEs   = Union{_LatinCSE, _UCS2CSE, _UTF32CSE}

const Byte_CSEs     = Union{ASCIICSE, Binary_CSEs, Latin_CSEs, UTF8CSE}
const Word_CSEs     = Union{Text2CSE, UCS2_CSEs, UTF16CSE} # 16-bit characters
const Quad_CSEs     = Union{Text4CSE, UTF32_CSEs}          # 32-bit code units
const Wide_CSEs     = Union{UTF16CSE, UCS2_CSEs, UTF32_CSEs}
const WordQuad_CSEs = Union{Text2CSE,Text4CSE,UCS2CSE,UTF16CSE,UTF32CSE}

const ASCIIStrings  = Str{ASCIICSE}
const BinaryStrings = Str{<:Binary_CSEs}
const RawStrings    = Str{<:Raw_CSEs}
const LatinStrings  = Str{<:Latin_CSEs}
const UCS2Strings   = Str{<:UCS2_CSEs}
const UTF32Strings  = Str{<:UTF32_CSEs}

const ByteStr = Str{<:Byte_CSEs}
const WordStr = Str{<:Word_CSEs}
const QuadStr = Str{<:Quad_CSEs}
const WideStr = Str{<:Wide_CSEs}

basecse(::Type{C}) where {C<:CSE} = C
basecse(::Type{_LatinCSE}) = LatinCSE
basecse(::Type{_UCS2CSE})  = UCS2CSE
basecse(::Type{_UTF32CSE}) = UTF32CSE

const AbsChar = @static isdefined(Base, :AbstractChar) ? AbstractChar : Union{Char, CodePoint}

## Get the character set / encoding used by a string type
cse(::Type{<:AbstractString}) = UTF8CSE # Default unless overridden
cse(::Type{String}) = UniPlusCSE         # allows invalid sequences
cse(::Type{<:SubString{T}}) where {T} = cse(T)
cse(::Type{<:SubString{<:Str{C}}}) where {C<:SubSet_CSEs} = basecse(C)
cse(::Type{<:Str{C}}) where {C<:CSE} = C
cse(str::AbstractString) = cse(typeof(str))

charset(::Type{<:AbstractString}) = UTF32CharSet # Default unless overridden
charset(::Type{String}) = UniPlusCharSet # allows invalid sequences
charset(::Type{<:SubString{T}}) where {T} = charset(T)
charset(::Type{<:SubString{<:Str{C}}}) where {C<:SubSet_CSEs} = charset(basecse(C))
charset(::Type{C}) where {CS,C<:CSE{CS}} = CS
charset(::Type{<:Str{C}}) where {C} = charset(C)
charset(str::AbstractString) = charset(typeof(str))

encoding(::Type{<:AbstractString}) = UTF8Encoding # Julia likes to think of this as the default
encoding(::Type{<:SubString{T}}) where {T} = encoding(T)
encoding(::Type{C}) where {CS,E,C<:CSE{CS,E}} = E
encoding(::Type{<:Str{C}}) where {C} = encoding(C)
encoding(str::AbstractString) = encoding(typeof(str))

promote_rule(::Type{T}, ::Type{T}) where {T<:CodePoint} = T
promote_rule(::Type{Text2Chr}, ::Type{Text1Chr}) = Text2Chr
promote_rule(::Type{Text4Chr}, ::Type{Text1Chr}) = Text4Chr
promote_rule(::Type{Text4Chr}, ::Type{Text2Chr}) = Text4Chr

promote_rule(::Type{T}, ::Type{ASCIIChr}) where {T} = T
promote_rule(::Type{LatinChr}, ::Type{_LatinChr}) = LatinChr
promote_rule(::Type{UTF32Chr}, ::Type{UCS2Chr}) = UTF32Chr
promote_rule(::Type{T}, ::Type{<:ByteChars}) where {T<:WideChars} = T

promote_rule(::Type{T}, ::Type{T}) where {T<:Str} = T
promote_rule(::Type{T}, ::Type{<:Str{Text1CSE}}) where {T<:Str{Text2CSE}} = T
promote_rule(::Type{T}, ::Type{<:Str{Text1CSE}}) where {T<:Str{Text4CSE}} = T
promote_rule(::Type{T}, ::Type{<:Str{Text2CSE}}) where {T<:Str{Text4CSE}} = T

promote_rule(::Type{String}, ::Type{<:Str{ASCIICSE}}) = String
promote_rule(::Type{T}, ::Type{<:Str{ASCIICSE}}) where {T<:Str{<:CSE}} = T
promote_rule(::Type{T}, ::Type{<:Str{Latin_CSEs}}) where {T<:Str{<:Union{UTF8CSE, Wide_CSEs}}} = T
promote_rule(::Type{T}, ::Type{<:Str{UCS2_CSEs}}) where {T<:Str{UTF32_CSEs}} = T

promote_rule(::Type{T}, ::Type{<:Str{_LatinCSE}}) where {T<:Str{LatinCSE}} = T
promote_rule(::Type{T}, ::Type{<:Str{_UCS2CSE}})  where {T<:Str{UCS2CSE}} = T
promote_rule(::Type{T}, ::Type{<:Str{_UTF32CSE}}) where {T<:Str{UTF32CSE}} = T

sizeof(s::Str) = sizeof(s.data) + 1 - STR_KEEP_NUL

"""Codeunits of string as a Vector"""
_data(s::Vector{UInt8}) = s
_data(s::String)  = s
_data(s::ByteStr) = @static V6_COMPAT ? Vector{UInt8}(s.data) : unsafe_wrap(Vector{UInt8}, s.data)

"""Pointer to codeunits of string"""
_pnt(s)          = pointer(s)
_pnt(s::ByteStr) = pointer(s.data)
_pnt(s::WordStr) = reinterpret(Ptr{UInt16}, pointer(s.data))
_pnt(s::QuadStr) = reinterpret(Ptr{UInt32}, pointer(s.data))

const CHUNKSZ = sizeof(UInt64) # used for fast processing of strings

_pnt64(s::Union{String,Vector{UInt8}}) = reinterpret(Ptr{UInt64}, pointer(s))
_pnt64(s::Str) = reinterpret(Ptr{UInt64}, pointer(s.data))

"""Length of string in codeunits"""
_len(s) = sizeof(s)
_len(s::WordStr) = sizeof(s) >>> 1
_len(s::QuadStr) = sizeof(s) >>> 2

# For convenience
@inline _lenpnt(s) = _len(s), _pnt(s)

@inline _calcpnt(str, siz) = (pnt = _pnt64(str) - CHUNKSZ;  (pnt, pnt + siz))

@inline _mask_bytes(n) = (1%UInt << ((n & (CHUNKSZ - 1)) << 3)) - 0x1

Base.need_full_hex(c::CodePoint) = is_hex_digit(c)
Base.escape_nul(c::CodePoint) = ('0' <= c <= '7') ? "\\x00" : "\\0"

# Support for SubString of Str

Base.SubString(str::Str{C}) where {C<:SubSet_CSEs} =
    SubString(Str(basecse(C), str))
Base.SubString(str::Str{C}, off::Int) where {C<:SubSet_CSEs} =
    SubString(Str(basecse(C), str), off)
Base.SubString(str::Str{C}, off::Int, fin::Int) where {C<:SubSet_CSEs} =
    SubString(Str(basecse(C), str), off, fin)

# Todo: clean these up, avoid so many definitions
_pnt(s::SubString{<:WordStr}) = reinterpret(Ptr{UInt16}, pointer(s))
_pnt(s::SubString{<:QuadStr}) = reinterpret(Ptr{UInt32}, pointer(s))

_len(s::SubString{<:WordStr}) = sizeof(s) >>> 1
_len(s::SubString{<:QuadStr}) = sizeof(s) >>> 2

@static V6_COMPAT && (sizeof(s::SubString{<:Str}) = s.endof == 0 ? 0 : nextind(s, s.endof) - 1)
