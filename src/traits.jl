# Copyright 2017-2018 Gandalf Software, Inc. (Scott Paul Jones)
# Licensed under MIT License, see LICENSE.md

# These use the "Holy Trait Trick", which was created by @timholy (Tim Holy),
# which was made possible by the type system in Julia from @jeff.bezanson (Jeff Bezanson).

# One is proud to stand on the shoulders of such people!

## Traits for string and character types ##

"""
    ValidatedStyle(A)
    ValidatedStyle(typeof(A))

`ValidatedStyle` specifies the whether a string or character type is always valid or not
When you define a new `AbstractString` or `AbstractChar` type, you can choose to implement it
as always validated, or validation state unknown.

    Strs.ValidatedStyle(::Type{<:MyString}) = AwaysValid()

The default is `UnknownValidity`
"""
abstract type ValidatedStyle end
struct AlwaysValid     <: ValidatedStyle end
struct UnknownValidity <: ValidatedStyle end

ValidatedStyle(::Type{<:AbstractChar}) = UnknownValidity()
@static isdefined(Base, :AbstractChar) || (ValidatedStyle(::Type{Char}) = UnknownValidity())
ValidatedStyle(::Type{<:CodePoint}) = AlwaysValid()

ValidatedStyle(::Type{<:AbstractString}) = UnknownValidity()
ValidatedStyle(::Type{<:Str}) = AlwaysValid()

ValidatedStyle(A::T) where {T<:Union{AbsChar,AbstractString}} = ValidatedStyle(T)

# single or multiple codeunits per codepoint

"""
    CodePointStyle(A)
    CodePointStyle(typeof(A))

`CodePointStyle` specifies the whether a string type uses one or multiple codeunits to
encode a single codepoint.
When you define a new `AbstractString` type, you can choose to implement it with either
single or multi-codeunit indexing.

    Strs.CodePointStyle(::Type{<:MyString}) = CodeUnitMulti()

The default is `CodeUnitMulti()`
"""
abstract type CodePointStyle end

struct CodeUnitSingle <: CodePointStyle end
struct CodeUnitMulti  <: CodePointStyle end

CodePointStyle(::Type{<:CSE}) = CodeUnitSingle()
CodePointStyle(::Type{<:Union{UTF8CSE,UTF16CSE}}) = CodeUnitMulti()

CodePointStyle(::Type{T}) where {T<:AbstractString} = CodePointStyle(cse(T))

CodePointStyle(::Type{<:SubString{T}}) where {T<:AbstractString} = CodePointStyle(T)
CodePointStyle(v::AbstractString) = CodePointStyle(typeof(v))

ismulti(str::AbstractString) = CodePointStyle(str) === CodeUnitMulti()

# Type of character set

"""
    CharSetStyle(A)
    CharSetStyle(typeof(A))

`CharSetStyle` specifies the information about the character set used by the string or
characters.
When you define a new `AbstractString` or `AbstractChar` type,
you can choose to implement it with

    Strs.CharSetStyle(::Type{<:MyString}) = CharSetISOCompat()

The default is `CharSetUnicode()`
"""
abstract type CharSetStyle end

"""Codepoints are not in Unicode compatible order"""
struct CharSetOther         <: CharSetStyle end
"""Characters 0-0x7f same as ASCII"""
struct CharSetASCIICompat   <: CharSetStyle end
"""Characters 0-0x9f follows ISO 8859"""
struct CharSetISOCompat     <: CharSetStyle end
"""Characters 0-0xd7ff, 0xe000-0xffff follow Unicode BMP"""
struct CharSetBMPCompat     <: CharSetStyle end
"""Full Unicode character set, no additions"""
struct CharSetUnicode       <: CharSetStyle end
"""Unicode character set, plus encodings of invalid characters"""
struct CharSetUnicodePlus   <: CharSetStyle end
"""8-bit Binary string, not text"""
struct CharSetBinary        <: CharSetStyle end
"""Raw bytes, words, or character string, unknown encoding/character set"""
struct CharSetUnknown       <: CharSetStyle end
                                 
CharSetStyle(A::AbstractString) = CharSetStyle(typeof(A))

CharSetStyle(::Type{<:AbstractString}) = CharSetUnicode()
CharSetStyle(::Type{String})           = CharSetUnicodePlus() # Encodes invalid characters also
CharSetStyle(::Type{<:RawStrings})     = CharSetUnknown()
CharSetStyle(::Type{Str{<:BinaryCSE}}) = CharSetBinary()
CharSetStyle(::Type{Str{<:ASCIICSE}})  = CharSetASCIICompat()
CharSetStyle(::Type{<:UCS2Strings})    = CharSetBMPCompat()
CharSetStyle(::Type{<:LatinStrings})   = CharSetISOCompat()

CharSetStyle(A::AbstractChar)   = CharSetStyle(typeof(A))

CharSetStyle(::Type{<:AbstractChar}) = CharSetUnicode()
CharSetStyle(::Type{Char})      = CharSetUnicodePlus() # Encodes invalid characters also
CharSetStyle(::Type{UInt8})     = CharSetBinary()
CharSetStyle(::Type{ASCIIChr})  = CharSetASCIICompat()
CharSetStyle(::Type{LatinChr})  = CharSetISOCompat()
CharSetStyle(::Type{_LatinChr}) = CharSetISOCompat()
CharSetStyle(::Type{UCS2Chr})   = CharSetBMPCompat()
CharSetStyle(::Type{UInt16})    = CharSetUnknown()
CharSetStyle(::Type{UInt32})    = CharSetUnknown()

# must check range if CS1 is smaller than CS2, even if CS2 is valid for it's range
_isvalid(::ValidatedStyle, ::Type{ASCIICharSet}, ::Type{T}, val) where {T<:CharSet} = isascii(val)

(_isvalid(::ValidatedStyle, ::Type{LatinCharSet}, ::Type{T}, val)
 where {T<:Union{Text2CharSet, Text4CharSet, UCS2CharSet, UTF32CharSet}}) =
     islatin(val)

(_isvalid(::ValidatedStyle, ::Type{UCS2CharSet}, ::Type{T}, val)
 where {T<:Union{Text2CharSet, Text4CharSet, UTF32CharSet}}) =
     isbmp(val)

_isvalid(::ValidatedStyle, ::Type{UTF32CharSet}, ::Type{<:CharSet}, val) =
    isunicode(val)

# no checking needed for cases where it is a superset of T
(_isvalid(::AlwaysValid, ::Type{LatinCharSet}, ::Type{T}, str)
  where {T<:Union{Text1CharSet,ASCIICharSet,LatinSubSet}}) = true

(_isvalid(::AlwaysValid, ::Type{UCS2CharSet}, ::Type{T}, str)
  where {T<:Union{Text1CharSet,ASCIICharSet,LatinCharSet,LatinSubSet,UCS2SubSet}}) = true

(_isvalid(::AlwaysValid, ::Type{UTF32CharSet}, ::Type{T}, str)
  where {T<:Union{Text1CharSet,ASCIICharSet,LatinCharSet,UCS2CharSet,LatinSubSet,UCS2SubSet,UTF32SubSet}}) =
    true

(_isvalid(::AlwaysValid, ::Type{S}, ::Type{<:CodeUnitTypes}, chr)
 where {S<:Union{Text1CharSet,Text2CharSet,Text4CharSet}}) =
     chr <= typemax(S)
 
# Different character sets
isvalid(::Type{Str{<:CSE{CS1}}}, str::T) where {T<:Str{<:CSE{CS2}},CS1} where {CS2} =
    _isvalid(ValidatedStyle(T), CS1, CS2, str)

# Same character set
isvalid(::Type{Str{<:CSE{CS}}}, str::T) where {T<:Str{<:CSE{CS}}} where {CS} =
     _isvalid(ValidatedStyle(T), str)

_isvalid(::AlwaysValid, v) = true

_isvalid(::UnknownValidity, str::T) where {T<:Str} = _isvalid(T, _pnt(str), _len(str))

# By default, check that it is valid Unicode codepoint
_isvalid(::UnknownValidity, v) = _isvalid(UnknownValidity(), UTF32CharSet, charset(v), v)

isvalid(::Type{T}, str::T) where {T<:Str}       = _isvalid(ValidatedStyle(T), str)
isvalid(::Type{T}, chr::T) where {T<:CodePoint} = _isvalid(ValidatedStyle(T), chr)

isvalid(str::T) where {T<:Str}       = _isvalid(ValidatedStyle(T), str)
isvalid(chr::T) where {T<:CodePoint} = _isvalid(ValidatedStyle(T), chr)

# Different character set
function isvalid(::Type{S}, chr::T) where {S<:CodePoint, T<:CodePoint}
    CS = charset(S)
    CT = charset(T)
    CS == CT ? _isvalid(ValidatedStyle(T), chr) : _isvalid(ValidatedStyle(T), CS, CT, chr)
end

# Not totally sure how to get rid of some of these, they really should be handled
# by the compiler, using the ValidatedStyle trait along with the character sets

_isvalid_chr(::Type{ASCIICharSet}, v)  = isascii(v)
_isvalid_chr(::Type{LatinCharSet}, v)  = islatin(v)
_isvalid_chr(::Type{UCS2CharSet},  v)  = isbmp(v)
_isvalid_chr(::Type{UTF32CharSet}, v)  = isunicode(v)
_isvalid_chr(::Type{LatinSubSet}, v)   = islatin(v)
_isvalid_chr(::Type{UCS2SubSet},  v)   = isbmp(v)
_isvalid_chr(::Type{UTF32SubSet}, v)   = isunicode(v)
_isvalid_chr(::Type{Text1CharSet}, v)  = v <= typemax(UInt8)
_isvalid_chr(::Type{Text2CharSet}, v)  = v <= typemax(UInt16)
_isvalid_chr(::Type{Text4CharSet}, v)  = v <= typemax(UInt32)
_isvalid_chr(::Type{BinaryCharSet}, v) = v <= typemax(UInt8)

# Not totally sure about this, base Char is rather funky in v0.7
_isvalid_chr(::Type{UniPlusCharSet}, v) = v <= typemax(UInt32)

isvalid(::Type{T}, v::Unsigned) where {T<:CodePoint} =
    _isvalid_chr(charset(T), v)
isvalid(::Type{T}, v::Signed) where {T<:CodePoint} =
    v >= 0 && _isvalid_chr(charset(T), v%Unsigned)

isvalid(::Type{Char}, ch::UnicodeChars) = true
isvalid(::Type{Char}, ch::Text1Chr) = true
isvalid(::Type{Char}, ch::Text2Chr) = isbmp(ch)
isvalid(::Type{Char}, ch::Text4Chr) = isunicode(ch)
isvalid(::Type{T},    ch::Char) where {T<:CodePoint} = isvalid(T, ch%UInt32)

# For now, there is only support for immutable `Str`s, when mutable `Str`s are added.

"""
    MutableStyle(A)
    MutableStyle(typeof(A))

`MutableStyle` specifies the whether a string type is mutable or not

    Strs.MutableStyle(::Type{<:MyString}) = MutableStr()

The default is `ImmutableStr`
"""
abstract type MutableStyle end
struct ImmutableStr <: MutableStyle end
struct MutableStr   <: MutableStyle end

MutableStyle(A::AbstractString) = MutableStyle(typeof(A))

MutableStyle(A::Str) = ImmutableStr()

_isimmutable(::ImmutableStr, str::Type{T}) where {T<:Str} = true
_isimmutable(::MutableStr, str::Type{T}) where {T<:Str} = false
isimmutable(::Type{T}) where {T<:Str} = _isimmutable(MutableStyle(T))

# Comparison traits

"""
    CompareStyle(Union{A, typeof(A)}, Union{B, typeof(B)})

`CompareStyle` specifies how to compare two strings with character set encodings A and B

    Strs.CompareStyle(::Type{<:MyString}, ::Type{String}) = ByteCompare()

The default is `CodePointCompare`
"""
abstract type CompareStyle end

# Todo: merge these with EqualsStyle

struct NoCompare        <: CompareStyle end # For equality checks, can't be equal
struct ByteCompare      <: CompareStyle end # Compare bytewise
struct ASCIICompare     <: CompareStyle end # Compare bytewise for ASCII subset, else codepoint
struct WordCompare      <: CompareStyle end # Compare first not equal word with <
struct UTF16Compare     <: CompareStyle end # Compare first not equal word adjusting > 0xd7ff
struct WidenCompare     <: CompareStyle end # Narrower can be simply widened for comparisons
struct CodePointCompare <: CompareStyle end # Compare CodePoints

CompareStyle(::Type{<:CSE}, ::Type{<:CSE}) = CodePointCompare()

CompareStyle(::Type{C}, ::Type{C}) where {C<:CSE} =
    ByteCompare()
CompareStyle(::Type{C}, ::Type{C}) where {C<:Union{Word_CSEs,Quad_CSEs}} =
    WordCompare()

CompareStyle(::Type{UTF16CSE},    ::Type{UTF16CSE})    = UTF16Compare()
CompareStyle(::Type{UTF16CSE},    ::Type{<:UCS2_CSEs}) = UTF16Compare()
CompareStyle(::Type{<:UCS2_CSEs}, ::Type{UTF16CSE})    = UTF16Compare()

CompareStyle(::Type{ASCIICSE}, ::Type{<:Union{Binary_CSEs,Latin_CSEs,UTF8CSE}}) =
    ByteCompare()
CompareStyle(::Type{ASCIICSE}, ::Type{<:Union{Word_CSEs,Quad_CSEs}}) =
    WidenCompare()

CompareStyle(::Type{<:Latin_CSEs}, ::Type{<:Latin_CSEs}) =
    ByteCompare()
CompareStyle(::Type{<:Latin_CSEs}, ::Type{UTF8CSE}) =
    ASCIICompare()
CompareStyle(::Type{<:Latin_CSEs}, ::Type{<:Union{Word_CSEs,Quad_CSEs}}) =
    WidenCompare()

CompareStyle(::Type{<:UCS2_CSEs}, ::Type{<:Union{ASCIICSE,Binary_CSEs,Latin_CSEs,Quad_CSEs}}) =
    WidenCompare()
CompareStyle(::Type{<:UCS2_CSEs},  ::Type{<:UCS2_CSEs}) =
    WordCompare()

CompareStyle(::Type{<:UTF32_CSEs},
             ::Type{<:Union{ASCIICSE,Binary_CSEs,Latin_CSEs,Text2CSE,UCS2_CSEs}}) =
    WidenCompare()
CompareStyle(::Type{<:UTF32_CSEs},  ::Type{<:UTF32_CSEs}) =
    WordCompare()

CompareStyle(::Type{S}, ::Type{T}) where {S<:AbstractString, T<:AbstractString} =
    CompareStyle(cse(S), cse(T))
CompareStyle(::Type{T}, ::Type{T}) where {T<:AbstractString} = ByteCompare()

CompareStyle(A::AbstractString, B::AbstractString) = CompareStyle(typeof(A), typeof(B))

Base.promote(::Type{NoCompare},    ::Type{<:CompareStyle})   = NoCompare
Base.promote(::Type{ByteCompare},  ::Type{CodePointCompare}) = ByteCompare
Base.promote(::Type{WordCompare},  ::Type{CodePointCompare}) = WordCompare
Base.promote(::Type{UTF16Compare}, ::Type{CodePointCompare}) = UTF16Compare
Base.promote(::Type{ASCIICompare}, ::Type{CodePointCompare}) = ASCIICompare
Base.promote(::Type{WidenCompare}, ::Type{CodePointCompare}) = WidenCompare

"""
    CanContain(Union{A, typeof(A)}, Union{B, typeof(B)})

`CanContainStyle` specifies whether the first string can contain a substring of the second type,
    and if so, what is the most efficient method of comparison

    Strs.CanContain(::Type{<:MyString}, ::Type{String}) = ByteCompare()

Returns an instance of type `CompareStyle`, default `CodePointCompare`
"""
CanContain(::Type{<:CSE}, ::Type{<:CSE}) = CodePointCompare()

CanContain(::Type{C}, ::Type{C}) where {C<:CSE} = ByteCompare()

CanContain(::Type{<:Binary_CSEs}, ::Type{<:Union{_UCS2CSE,_UTF32CSE}}) =
    NoCompare()
CanContain(::Type{<:Binary_CSEs}, ::Type{<:Byte_CSEs}) =
    ByteCompare()
CanContain(::Type{<:Binary_CSEs}, ::Type{<:WordQuad_CSEs}) =
    WidenCompare()

CanContain(::Type{ASCIICSE}, ::Type{<:SubSet_CSEs}) =
    NoCompare()
CanContain(::Type{ASCIICSE}, ::Type{<:Union{Binary_CSEs, LatinCSE, UTF8CSE}}) =
    ByteCompare()
CanContain(::Type{ASCIICSE}, ::Type{<:WordQuad_CSEs}) =
    WidenCompare()

CanContain(::Type{<:Latin_CSEs}, ::Type{<:Union{_UCS2CSE,_UTF32CSE}}) =
    NoCompare()
CanContain(::Type{<:Latin_CSEs}, ::Type{<:Union{Binary_CSEs,ASCIICSE,Latin_CSEs}}) =
    ByteCompare()
CanContain(::Type{<:Latin_CSEs}, ::Type{UTF8CSE}) =
    ASCIICompare()
CanContain(::Type{<:Latin_CSEs}, ::Type{<:WordQuad_CSEs}) =
    WidenCompare()

CanContain(::Type{UTF8CSE}, ::Type{<:Union{ASCIICSE,Binary_CSEs}}) =
    ByteCompare()
CanContain(::Type{UTF8CSE}, ::Type{<:Latin_CSEs}) =
    ASCIICompare()

CanContain(::Type{<:Union{Text2CSE,UCS2CSE}}, ::Type{_UTF32CSE}) =
    NoCompare()
CanContain(::Type{<:Union{Text2CSE,UCS2_CSEs}}, ::Type{<:Word_CSEs}) =
    ByteCompare()
CanContain(::Type{<:Union{Text2CSE,UCS2_CSEs}},
           ::Type{<:Union{ASCIICSE, Binary_CSEs, Latin_CSEs, Quad_CSEs}}) =
    WidenCompare()

CanContain(::Type{UTF16CSE}, ::Type{<:Union{ASCIICSE,Binary_CSEs,Latin_CSEs}}) =
    WidenCompare()
CanContain(::Type{UTF16CSE}, ::Type{<:Union{Text2CSE, UCS2_CSEs}}) =
    ByteCompare()

CanContain(::Type{<:Quad_CSEs}, ::Type{<:Quad_CSEs}) =
    ByteCompare()
CanContain(::Type{<:Quad_CSEs},
           ::Type{<:Union{Binary_CSEs,ASCIICSE,Latin_CSEs,Text2CSE,UCS2_CSEs}}) =
    WidenCompare()

CanContain(::Type{S}, ::Type{T}) where {S<:AbstractString, T<:AbstractString} =
    CanContain(cse(S), cse(T))
CanContain(::Type{T}, ::Type{T}) where {T<:AbstractString} = ByteCompare()

CanContain(A::AbstractString, B::AbstractString) = CanContain(typeof(A), typeof(B))

"""
    EqualsStyle(Union{A, typeof(A)}, Union{B, typeof(B)})

`EqualsStyle` specifies whether the first string can equal a substring of the second type,
    and if so, what is the most efficient method of comparison

This is determined by the Strs.CanContain trait
Returns an instance of type `CompareStyle`, default `CodePointCompare`
"""
EqualsStyle(::Type{S}, ::Type{T}) where {S<:CSE,T<:CSE} =
    Base.promote_typeof(CanContain(S, T), CanContain(T, S))()
EqualsStyle(::Type{T}, ::Type{T}) where {T<:CSE} = ByteCompare()

EqualsStyle(::Type{S}, ::Type{T}) where {S<:AbstractString, T<:AbstractString} =
    EqualsStyle(cse(S), cse(T))
EqualsStyle(::Type{T}, ::Type{T}) where {T<:AbstractString} = ByteCompare()

EqualsStyle(A::AbstractString, B::AbstractString) =  EqualsStyle(typeof(A), typeof(B))
