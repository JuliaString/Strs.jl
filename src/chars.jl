#=
CodePoint support

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
In part based on code for Char in Julia
=#

"""Default value for Str types"""
codeunit(::Type{<:Str}) = UInt8
ncodeunits(str::T) where {T<:Str} = _len(str)

"""Type of codeunits"""
codeunit(::Type{<:WordStr}) = UInt16
codeunit(::Type{<:QuadStr}) = UInt32
codeunit(::Type{UniStr})    = UInt32 # Note, the real type could be UInt8, UInt16, or UInt32

codeunit(::S) where {S<:Str} = codeunit(S)

"""Default value for CodePoint types"""
basetype(::Type{<:CodePoint}) = UInt8
basetype(::Type{UCS2Chr})     = UInt16
basetype(::Type{Text2Chr})    = UInt16
basetype(::Type{UTF32Chr})    = UInt32
basetype(::Type{Text4Chr})    = UInt32

basetype(::Type{T}) where {T<:CodeUnitTypes} = T

tobase(v::T) where {T<:CodePoint} = reinterpret(basetype(T), v)
tobase(v::T) where {T<:CodeUnitTypes} = v

typemin(::Type{T}) where {T<:CodePoint} = reinterpret(T, typemin(basetype(T)))
typemax(::Type{T}) where {T<:CodePoint} = reinterpret(T, typemax(basetype(T)))

typemax(::Type{ASCIIChr}) = reinterpret(ASCIIChr, 0x7f)
typemax(::Type{UTF32Chr}) = reinterpret(UTF32Chr, 0x10ffff)

codepoint_type(::Type{<:Text1Str})       = Text1Chr
codepoint_type(::Type{<:Text2Str})       = Text2Chr
codepoint_type(::Type{<:Text4Str})       = Text4Chr
codepoint_type(::Type{<:BinaryStr})      = UInt8

codepoint_type(::Type{<:ASCIIStr})       = ASCIIChr
codepoint_type(::Type{<:LatinStr})       = LatinChr
codepoint_type(::Type{<:_LatinStr})      = _LatinChr
codepoint_type(::Type{<:UCS2Str})        = UCS2Chr
codepoint_type(::Type{<:_UCS2Str})       = UCS2Chr
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
set_codeunit!(dat::String, pos, ch) = unsafe_store!(pointer(dat), pos, ch)

set_codeunit!(pnt::Ptr{<:CodeUnitTypes}, ch) = unsafe_store!(pnt, ch)
set_codeunit!(dat::AbstractVector{<:CodeUnitTypes}, ch) = (dat[1] = ch)
set_codeunit!(dat::String, ch) = set_codeunit!(dat, 1, ch)

isvalid(::Type{T}) where {T<:UnicodeChars} = true

isvalid(::Type{ASCIIStr}, str::Vector{ASCIIChr}) = true
isvalid(::Type{LatinStrings}, str::Vector{T}) where {T<:Union{ASCIIChr,LatinChars}} = true
isvalid(::Type{UCS2Str}, str::Vector{T}) where {T<:Union{ASCIIChr,LatinChars,UCS2Chr}} = true
isvalid(::Type{S}, str::Vector{T}) where {S<:UnicodeStrings} where {T<:UnicodeChars} = true

isvalid(::Type{T}, v::Signed) where {T<:ByteChars} = typemin(T) <= v <= typemax(T)
isvalid(::Type{T}, v::Unsigned) where {T<:ByteChars} = v <= typemax(T)
isvalid(::Type{T}, v::Signed) where {T<:WideChars} = v >= 0 && isvalid(T, Unsigned(v))
isvalid(::Type{T}, v::Unsigned) where {T<:WideChars} =
    (v <= typemax(T)) & !is_surrogate_codeunit(v)

isvalid(::Type{Char}, ch::T) where {T<:UnicodeChars} = true
isvalid(::Type{Char}, ch::Text1Chr) = true
isvalid(::Type{Char}, ch::Text2Chr) = !is_surrogate_codeunit(tobase(ch))
isvalid(::Type{Char}, ch::Text4Chr) = isvalid(UTF32Chr, tobase(ch))

convert(::Type{T}, v::S) where {T<:Integer, S<:CodePoint} = convert(T, tobase(v))
convert(::Type{T}, v::Signed) where {T<:CodePoint} =
    (v >= 0 && isvalid(T, v%Unsigned)) ? convert(T, tobase(v)) : codepoint_error(T, v)
convert(::Type{T}, v::Unsigned) where {T<:CodePoint} =
    isvalid(T, v) ? reinterpret(T, basetype(T)(v)) : codepoint_error(T, v)
convert(::Type{Char}, v::T) where {T<:CodePoint} = convert(Char, x%basetype(T))

rem(x::S, ::Type{T}) where {S<:CodePoint, T<:Number}    = rem(reinterpret(basetype(S), x), T)
rem(x::S, ::Type{T}) where {S<:Number, T<:CodePoint}    = reinterpret(T, x%basetype(T))
rem(x::S, ::Type{T}) where {S<:CodePoint, T<:CodePoint} = reinterpret(T, x%basetype(T))
rem(x::S, ::Type{Char}) where {S<:CodePoint} = (x%basetype(T))%Char

(::Type{S})(v::T) where {S<:Union{UInt32, Int, UInt, Char}, T<:CodePoint} = Strs.tobase(v)%S

for nam in (:Text1, :Text2, :Text4, :ASCII, :Latin, :_Latin, :UCS2, :UTF32)
    sym = Symbol(string(nam, "Chr"))
    @eval $sym(v) = convert($sym, v)
end

size(cp::CodePoint) = ()
size(cp::CodePoint, dim) = convert(Int, dim) < 1 ? boundserr(cp, dim) : 1
ndims(cp::CodePoint) = 0
ndims(::Type{<:CodePoint}) = 0
length(cp::CodePoint) = 1
endof(cp::CodePoint) = 1
getindex(cp::CodePoint) = cp
getindex(cp::CodePoint, i::Integer) = i == 1 ? cp : boundserr(cp, i)
getindex(cp::CodePoint, I::Integer...) = all(x -> x == 1, I) ? cp : boundserr(cp, I)
first(cp::CodePoint) = cp
last(cp::CodePoint) = cp
eltype(::Type{CodePoint}) = CodePoint

_uni_rng(m) = 0x00000:ifelse(m < 0xd800, m, m-0x800)
codepoint_rng(::Type{T}) where {T<:CodePoint} = _uni_rng(typemax(T)%UInt32)
codepoint_rng(::Type{Char}) = _uni_rng(0x10ffff)
codepoint_rng(::Type{Text2Chr}) = 0%UInt16:typemax(UInt16)
codepoint_rng(::Type{Text4Chr}) = 0%UInt32:typemax(UInt32)

codepoint_adj(::Type{T}, ch) where {T} = ifelse(ch < 0xd800, ch, ch+0x800)%T
codepoint_adj(::Type{T}, ch) where {T<:Union{Text2Chr,Text4Chr}} = ch%T

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

show(io::IO, cp::CodePoint)  = print(io, Char(tobase(cp)))
print(io::IO, cp::CodePoint) = print(io, Char(tobase(cp)))
