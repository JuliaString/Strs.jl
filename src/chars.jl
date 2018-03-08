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

bytoff(::Type{UInt8},  off) = off
bytoff(::Type{UInt16}, off) = off << 1
bytoff(::Type{UInt32}, off) = off << 2
chroff(::Type{UInt8},  off) = off
chroff(::Type{UInt16}, off) = off >>> 1
chroff(::Type{UInt32}, off) = off >>> 2

chrdiff(pnt::Ptr{T}, beg::Ptr{T}) where {T<:CodeUnitTypes} = Int(chroff(T, pnt - beg))

bytoff(pnt::Ptr{T}, off) where {T<:CodeUnitTypes} = pnt + bytoff(T, off)

"""Default value for CodePoint types"""
basetype(::Type{<:CodePoint}) = UInt8
basetype(::Type{UCS2Chr})     = UInt16
basetype(::Type{Text2Chr})    = UInt16
basetype(::Type{UTF32Chr})    = UInt32
basetype(::Type{Text4Chr})    = UInt32

basetype(::Type{Char})        = UInt32

basetype(::Type{T}) where {T<:CodeUnitTypes} = T

tobase(v::T) where {T<:CodePoint} = reinterpret(basetype(T), v)
tobase(v::T) where {T<:CodeUnitTypes} = v

tobase(v::Char) = v%UInt32

codepoint(v::T) where {T<:CodePoint} = tobase(v)

typemin(::Type{T}) where {T<:CodePoint} = reinterpret(T, typemin(basetype(T)))
typemax(::Type{T}) where {T<:CodePoint} = reinterpret(T, typemax(basetype(T)))

typemax(::Type{ASCIIChr}) = reinterpret(ASCIIChr, 0x7f)
typemax(::Type{UTF32Chr}) = reinterpret(UTF32Chr, 0x10ffff)

codepoint_type(::Type{<:AbstractString})   = Char

codepoint_type(::Type{<:Str{<:Text1CSE}})  = Text1Chr
codepoint_type(::Type{<:Str{<:Text2CSE}})  = Text2Chr
codepoint_type(::Type{<:Str{<:Text4CSE}})  = Text4Chr
codepoint_type(::Type{<:Str{<:BinaryCSE}}) = UInt8

codepoint_type(::Type{<:Str{<:ASCIICSE}})  = ASCIIChr
codepoint_type(::Type{<:Str{<:LatinCSE}})  = LatinChr
codepoint_type(::Type{<:Str{<:_LatinCSE}}) = _LatinChr
codepoint_type(::Type{<:Str{<:UCS2CSE}})   = UCS2Chr
codepoint_type(::Type{<:Str{<:_UCS2CSE}})  = UCS2Chr
codepoint_type(::Type{<:Str{<:UnicodeEncodings}}) = UTF32Chr

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

convert(::Type{T}, v::S) where {T<:Integer, S<:CodePoint} = convert(T, tobase(v))::T
convert(::Type{T}, v::Signed) where {T<:CodePoint} =
    (v >= 0 && isvalid(T, v%Unsigned)) ? convert(T, tobase(v)) : codepoint_error(T, v)
convert(::Type{T}, v::Unsigned) where {T<:CodePoint} =
    isvalid(T, v) ? reinterpret(T, basetype(T)(v)) : codepoint_error(T, v)
convert(::Type{Char}, v::T) where {T<:CodePoint} = convert(Char, tobase(v))
convert(::Type{T}, v::Char) where {T<:CodePoint} = convert(T, tobase(v))::T

rem(x::S, ::Type{T}) where {S<:CodePoint, T<:Number}    = rem(reinterpret(basetype(S), x), T)
rem(x::S, ::Type{T}) where {S<:Number, T<:CodePoint}    = reinterpret(T, x%basetype(T))
rem(x::S, ::Type{T}) where {S<:CodePoint, T<:CodePoint} = reinterpret(T, x%basetype(T))
rem(x::S, ::Type{Char}) where {S<:CodePoint} = Char(x%basetype(S))
rem(x::Char, ::Type{T}) where {T<:CodePoint} = x%UInt32%T

(::Type{S})(v::T) where {S<:Union{UInt32, Int, UInt}, T<:CodePoint} = tobase(v)%S
(::Type{Char})(v::CodePoint) = Char(tobase(v))
(::Type{T})(v::Char) where {T<:CodePoint} = T(tobase(v))

for nam in (:Text1, :Text2, :Text4, :ASCII, :Latin, :_Latin, :UCS2, :UTF32)
    sym = Symbol(string(nam, "Chr"))
    @eval $sym(v::Number) = convert($sym, v)
end

size(cp::CodePoint, dim) = convert(Int, dim) < 1 ? boundserr(cp, dim) : 1
getindex(cp::CodePoint, i::Integer) = i == 1 ? cp : boundserr(cp, i)
getindex(cp::CodePoint, I::Integer...) = all(x -> x == 1, I) ? cp : boundserr(cp, I)

@static if !isdefined(Base, :AbstractChar)
    size(cp::CodePoint) = ()
    ndims(cp::CodePoint) = 0
    ndims(::Type{<:CodePoint}) = 0
    length(cp::CodePoint) = 1
    lastindex(cp::CodePoint) = 1
    getindex(cp::CodePoint) = cp
    first(cp::CodePoint) = cp
    last(cp::CodePoint) = cp
    eltype(::Type{CodePoint}) = CodePoint
    start(cp::CodePoint) = false
    next(cp::CodePoint, state) = (cp, true)
    done(cp::CodePoint, state) = state
    isempty(cp::CodePoint) = false
    in(x::CodePoint, y::CodePoint) = x == y
    -(x::CodePoint, y::CodePoint) = Int(x) - Int(y)
    -(x::CodePoint, y::Integer) = CodePoint((Int32(x) - Int32(y))%UInt32)
    +(x::CodePoint, y::Integer) = CodePoint((Int32(x) + Int32(y))%UInt32)
    +(x::Integer, y::CodePoint) = y + x
end

_uni_rng(m) = 0x00000:ifelse(m < 0xd800, m, m-0x800)
codepoint_rng(::Type{T}) where {T<:CodePoint} = _uni_rng(typemax(T)%UInt32)
codepoint_rng(::Type{Char}) = _uni_rng(0x10ffff)
codepoint_rng(::Type{Text2Chr}) = 0%UInt16:typemax(UInt16)
codepoint_rng(::Type{Text4Chr}) = 0%UInt32:typemax(UInt32)

codepoint_adj(::Type{T}, ch) where {T} = ifelse(ch < 0xd800, ch, ch+0x800)%T
codepoint_adj(::Type{T}, ch) where {T<:Union{Text2Chr,Text4Chr}} = ch%T

==(x::CodePoint, y::AbsChar) = tobase(x) == tobase(y)
==(x::AbsChar, y::CodePoint) = tobase(x) == tobase(y)

isless(x::CodePoint, y::AbsChar)   = tobase(x) < tobase(y)
isless(x::AbsChar,   y::CodePoint) = tobase(x) < tobase(y)

# Note: this is not the same as the Base definition, which may be a problem
hash(x::CodePoint, h::UInt) =
    hash_uint64(xor((UInt32(x) + 0x0d4d64234) << 32), UInt64(h))

show(io::IO, cp::CodePoint)  = print(io, Char(tobase(cp)))
print(io::IO, cp::CodePoint) = print(io, Char(tobase(cp)))
