#=
CodePoint support

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
In part based on code for Char in Julia
=#
ncodeunits(str::T) where {T<:Str} = _len(str)

"""Type of codeunits"""
codeunit_type(::Type{String})    = UInt8
codeunit_type(::Type{<:ByteStr}) = UInt8
codeunit_type(::Type{<:WordStr}) = UInt16
codeunit_type(::Type{<:QuadStr}) = UInt32

"""Size of codeunits"""
codeunit_size(::Type{T}) where {T<:Union{String,Str}} = sizeof(codeunit_type(T))

basetype(::Type{ASCIIChr})  = UInt8
basetype(::Type{LatinChr})  = UInt8
basetype(::Type{_LatinChr}) = UInt8
basetype(::Type{UCS2Chr})   = UInt16
basetype(::Type{UTF32Chr})  = UInt32

basetype(::Type{RawByte}) = UInt8
basetype(::Type{RawWord}) = UInt16
basetype(::Type{RawChar}) = UInt32

basetype(::Type{T}) where {T<:CodeUnitTypes} = T

tobase(v::T) where {T<:CodePoint} = reinterpret(basetype(T), v)
tobase(v::T) where {T<:CodeUnitTypes} = v

typemin(::Type{T}) where {T<:CodePoint} = reinterpret(T, typemin(basetype(T)))
typemax(::Type{T}) where {T<:CodePoint} = reinterpret(T, typemax(basetype(T)))

typemax(::Type{T}) where {T<:Union{LatinChars,UCS2Chr}} = reinterpret(T, typemax(basetype(T)))
typemax(::Type{ASCIIChr}) = reinterpret(ASCIIChr, 0x7f)
typemax(::Type{UTF32Chr}) = reinterpret(UTF32Chr, 0x10ffff)

codepoint_type(::Type{RawByteStr})       = RawByte
codepoint_type(::Type{RawWordStr})       = RawWord
codepoint_type(::Type{RawCharStr})       = RawChar
codepoint_type(::Type{BinaryStr})        = UInt8

codepoint_type(::Type{ASCIIStr})         = ASCIIChr
codepoint_type(::Type{LatinStr})         = LatinChr
codepoint_type(::Type{_LatinStr})        = _LatinChr
codepoint_type(::Type{UCS2Str})          = UCS2Chr
codepoint_type(::Type{_UCS2Str})         = UCS2Chr
codepoint_type(::Type{<:UnicodeStrings}) = UTF32Chr

codepoint_size(::Type{T}) where {T<:Union{String,Str}} = sizeof(codepoint_type(T))

get_codeunit(dat, pos) = codeunit(dat, pos)
get_codeunit(pnt::Ptr{<:CodeUnitTypes}, pos) = unsafe_load(pnt, pos)
get_codeunit(dat::AbstractVector{<:CodeUnitTypes}, pos) = dat[pos]
get_codeunit(str::Str, pos) = get_codeunit(_pnt(str), pos)

codeunit(str::Str, pos::Integer) = get_codeunit(str, pos)

get_codeunit(dat) = get_codeunit(dat, 1)
get_codeunit(pnt::Ptr{<:CodeUnitTypes}) = unsafe_load(pnt)

set_codeunit!(pnt::Ptr{<:CodeUnitTypes}, pos, ch) = unsafe_store!(pnt, ch, pos)
set_codeunit!(dat::AbstractVector{<:CodeUnitTypes}, pos, ch) = (dat[pos] = ch)

set_codeunit!(pnt::Ptr{<:CodeUnitTypes}, ch) = unsafe_store!(pnt, ch)
set_codeunit!(dat::AbstractVector{<:CodeUnitTypes}, ch) = (dat[1] = ch)

isvalid(::Type{ASCIIStr}, str::Vector{ASCIIChr}) = true
isvalid(::Type{LatinStrings}, str::Vector{T}) where {T<:Union{ASCIIChr,LatinChars}} = true
isvalid(::Type{UCS2Str}, str::Vector{T}) where {T<:Union{ASCIIChr,LatinChars,UCS2Chr}} = true
isvalid(::Type{S}, str::Vector{T}) where {S<:UnicodeStrings} where {T<:UnicodeChars} = true

isvalid(::Type{T}, v::Signed) where {T<:ByteChars} = typemin(T) <= v <= typemax(T)
isvalid(::Type{T}, v::Unsigned) where {T<:ByteChars} = v <= typemax(T)
isvalid(::Type{T}, v::Signed) where {T<:WideChars} = v >= 0 && isvalid(T, Unsigned(v))
isvalid(::Type{T}, v::Unsigned) where {T<:WideChars} =
    (v <= typemax(T)) & !is_surrogate_codeunit(v)

convert(::Type{T}, v::S) where {T<:Integer, S<:CodePoint} = convert(T, tobase(v))
convert(::Type{T}, v::Signed) where {T<:CodePoint} =
    (v >= 0 && isvalid(T, v%Unsigned)) ? convert(T, tobase(v)) : error("Invalid CodePoint:$T $v")
convert(::Type{T}, v::Unsigned) where {T<:CodePoint} =
    isvalid(T, v) ? reinterpret(T, basetype(T)(v)) : error("Invalid CodePoint:$T $v")

rem(x::S, ::Type{T}) where {S<:CodePoint, T<:Number}    = rem(reinterpret(basetype(S), x), T)
rem(x::S, ::Type{T}) where {S<:Number, T<:CodePoint}    = reinterpret(T, x%basetype(T))
rem(x::S, ::Type{T}) where {S<:CodePoint, T<:CodePoint} = reinterpret(T, x%basetype(T))

UInt64(cu::T) where {T<:CodePoint} = tobase(cu)%UInt64
Int64(cu::T) where {T<:CodePoint} =  tobase(cu)%Int64

RawByte(v)   = convert(RawByte, v)
RawWord(v)   = convert(RawWord, v)
RawChar(v)   = convert(RawChar, v)
ASCIIChr(v)  = convert(ASCIIChr, v)
LatinChr(v)  = convert(LatinChr, v)
_LatinChr(v) = convert(_LatinChr, v)
UCS2Chr(v)   = convert(UCS2Chr, v)
UTF32Chr(v)  = convert(UTF32Chr, v)

size(cp::CodePoint) = ()
size(cp::CodePoint, dim) = convert(Int, dim) < 1 ? throw(BoundsError()) : 1
ndims(cp::CodePoint) = 0
ndims(::Type{<:CodePoint}) = 0
length(cp::CodePoint) = 1
endof(cp::CodePoint) = 1
getindex(cp::CodePoint) = cp
getindex(cp::CodePoint, i::Integer) = i == 1 ? cp : throw(BoundsError(i))
getindex(cp::CodePoint, I::Integer...) = all(x -> x == 1, I) ? cp : throw(BoundsError())
first(cp::CodePoint) = cp
last(cp::CodePoint) = cp
eltype(::Type{CodePoint}) = CodePoint

start(cp::CodePoint) = false
next(cp::CodePoint, state) = (cp, true)
done(cp::CodePoint, state) = state
isempty(cp::CodePoint) = false
in(x::CodePoint, y::CodePoint) = x == y

==(x::CodePointTypes, y::CodePoint) = tobase(x) == tobase(y)
==(x::CodePoint, y::CodeUnitTypes) = tobase(x) == tobase(y)
==(x::CodePoint, y::Char) = tobase(x) == y%UInt32
==(x::Char, y::CodePoint) = y == x

isless(x::CodePointTypes, y::CodePoint) = tobase(x) < tobase(y)
isless(x::CodePoint, y::CodeUnitTypes)  = tobase(x) < tobase(y)
isless(x::CodePointTypes, y::Char) = tobase(x) < y%UInt32
isless(x::Char, y::CodePointTypes) = x%UInt32 < tobase(y)

hash(x::CodePoint, h::UInt) =
    hash_uint64(xor((UInt32(x) + 0x0d4d64234) << 32), UInt64(h))

-(x::CodePointTypes, y::CodePoint) = Int(x) - Int(y)
-(x::CodePoint, y::Integer) = CodePoint((Int32(x) - Int32(y))%UInt32)
+(x::CodePoint, y::Integer) = CodePoint((Int32(x) + Int32(y))%UInt32)
+(x::Integer, y::CodePoint) = y + x

Base.show(io, cp::CodePoint) = show(io, Char(tobase(cp)))
