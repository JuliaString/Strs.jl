# Copyright 2017 Gandalf Software, Inc., Scott P. Jones
# Licensed under MIT License, see LICENSE.md

## Traits for string types ##

"""
    ValidatedStyle(A)
    ValidatedStyle(typeof(A))

`ValidatedStyle` specifies the whether a string type use one or multiple codeunits to
encode a single codepoint.
When you define a new `AbstractString` type, you can choose to implement it as always
validated, or validation state unknown.

    Strs.ValidatedStyle(::Type{<:MyString}) = AwaysValid()

The default is `UnknownValidity`
"""
abstract type ValidatedStyle end
struct AlwaysValid     <: ValidatedStyle end
struct UnknownValidity <: ValidatedStyle end

ValidatedStyle(A::AbstractString) = ValidatedStyle(typeof(A))
ValidatedStyle(::Type{T}) where {T<:AbstractString} = ValidatedStyle(T)

for T in (BinaryStr, ASCIIStr, LatinStr, UTF8Str, UCS2Str, UTF16Str, UTF32Str,
          _LatinStr, _UCS2Str, _UTF32Str)
    @eval(ValidatedStyle(::Type{$T}) = AlwaysValid())
end
                                 
ValidatedStyle(A::T) where {T<:AbstractChar} = ValidatedStyle(T)
ValidatedStyle(::Type{T}) where {T<:AbstractChar} = UnknownValidity()
ValidatedStyle(A::Char) = UnknownValidity() # Only until Char becomes <: AbstractChar
ValidatedStyle(::Type{Char}) = UnknownValidity() # Only until Char becomes <: AbstractChar

for T in (ASCIIChr, LatinChr, _LatinChr, UCS2Chr, UTF32Chr)
    @eval(ValidatedStyle(::Type{$T}) = AlwaysValid())
end

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

CodePointStyle(A::AbstractString) = CodePointStyle(typeof(A))
CodePointStyle(::Type{<:AbstractString}) = CodeUnitSingle()
CodePointStyle(::Type{T}) where {T<:Union{UTF8Str,UTF16Str,String}} = CodeUnitMulti()

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
                                 
CharSetStyle(A::AbstractString)  = CharSetStyle(typeof(A))
CharSetStyle(::Type{<:AbstractString}) = CharSetUnicode()
CharSetStyle(::Type{String})     = CharSetUnicodePlus() # Encodes invalid characters also
CharSetStyle(::Type{RawByteStr}) = CharSetUnknown()
CharSetStyle(::Type{RawWordStr}) = CharSetUnknown()
CharSetStyle(::Type{RawCharStr}) = CharSetUnknown() # Char or UInt32, since Char might be invalid
CharSetStyle(::Type{BinaryStr})  = CharSetBinary()
CharSetStyle(::Type{ASCIIStr})   = CharSetASCIICompat()
CharSetStyle(::Type{LatinStr})   = CharSetISOCompat()
CharSetStyle(::Type{UCS2Str})    = CharSetBMPCompat()
CharSetStyle(::Type{_LatinStr})  = CharSetISOCompat()
CharSetStyle(::Type{_UCS2Str})   = CharSetBMPCompat()

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

_isvalid(::UnknownValidity, ::Type{T}, str::T) where {T<:Str} = _isvalid(T, _pnt(str), _len(str))
_isvalid(::AlwaysValid, ::Type{T}, str) where {T} = true
isvalid(::Type{T}, str::T) where {T<:Str}       = _isvalid(ValidatedStyle(T), T, str)
isvalid(::Type{T}, chr::T) where {T<:CodePoint} = _isvalid(ValidatedStyle(T), T, chr)
isvalid(str::T) where {T<:Str} = isvalid(T, str)
isvalid(ch::T) where {T<:CodePoint} = isvalid(T, ch)

# must check range if CS1 is smaller than CS2
_isvalid(::T, ::Type{ASCIICharSet}, ::Type{<:CharSet}, str) where {T<:ValidatedStyle} =
    isascii(str)
_isvalid(::T, ::Type{LatinCharSet}, ::Type{<:CharSet}, str) where {T<:ValidatedStyle} =
    islatin(str)
_isvalid(::T, ::Type{UCS2CharSet}, ::Type{<:CharSet}, str) where {T<:ValidatedStyle} =
    isucs2(str)
_isvalid(::T, ::Type{UnicodeCharSet}, ::Type{<:CharSet}, str) where {T<:ValidatedStyle} =
    isunicode(str)

# no checking needed for cases where T is a superset of S
(_isvalid(::AlwaysValid, ::Type{T}, ::Type{S}, str)
 where {T<:Union{LatinCharSet,LatinSubSet},
        S<:Union{ASCIICharSet,LatinCharSet,LatinSubSet}}) =
     true

(_isvalid(::AlwaysValid, ::Type{T}, ::Type{S}, str)
 where {T<:Union{UCS2CharSet,UCS2SubSet},
        S<:Union{LatinCharSet,UCS2CharSet,LatinSubSet,UCS2SubSet}}) =
     true

(_isvalid(::AlwaysValid, ::Type{T}, ::S, str)
 where {T<:Union{UnicodeCharSet,UnicodeSubSet},
        S<:Union{LatinCharSet,UCS2CharSet,UnicodeCharSet,LatinSubSet,UCS2SubSet,UnicodeSubSet}}) =
     true

(isvalid(::Type{S}, str::T)
 where {S<:Str{CSE1},T<:Str{CSE2}}
 where {CSE1<:CSE{CS1},CSE2<:CSE{CS2}}
 where {CS1, CS2}) =
     _isvalid(ValidatedStyle(T), CS1, CS2, str)

isvalid(::Type{S}, chr::T) where {S<:CodePoint, T<:CodePoint} =
     _isvalid(ValidatedStyle(T), S, T, chr)

                                          
