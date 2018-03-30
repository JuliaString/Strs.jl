#=
Chr support

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
In part based on code for Char in Julia
=#

"""Default value for Str /Chr types"""
codeunit(::Type{<:CSE}) = UInt8
codeunit(::Type{<:Word_CSEs}) = UInt16
codeunit(::Type{<:Quad_CSEs}) = UInt32

"""Type of codeunits"""
codeunit(::Type{UniStr})    = UInt32 # Note, the real type could be UInt8, UInt16, or UInt32
codeunit(::Type{<:Str{C}}) where {C<:CSE} = codeunit(C)

codeunit(::S) where {S<:Str} = codeunit(S)

bytoff(::Type{UInt8},  off) = off
bytoff(::Type{UInt16}, off) = off << 1
bytoff(::Type{UInt32}, off) = off << 2
chroff(::Type{UInt8},  off) = off
chroff(::Type{UInt16}, off) = off >>> 1
chroff(::Type{UInt32}, off) = off >>> 2

chrdiff(pnt::Ptr{T}, beg::Ptr{T}) where {T<:CodeUnitTypes} = Int(chroff(T, pnt - beg))

bytoff(pnt::Ptr{T}, off) where {T<:CodeUnitTypes} = pnt + bytoff(T, off)

typemax(::Type{ASCIIChr}) = ASCIIChr(0x7f)
typemax(::Type{UTF32Chr}) = UTF32Chr(0x10ffff)

basetype(::Type{Char})        = UInt32
basetype(::Type{T}) where {T<:CodeUnitTypes} = T

eltype(::Type{<:Str{BinaryCSE}}) = UInt8

eltype(::Type{<:Str{Text1CSE}})       = Text1Chr
eltype(::Type{<:Str{Text2CSE}})       = Text2Chr
eltype(::Type{<:Str{Text4CSE}})       = Text4Chr
eltype(::Type{<:Str{ASCIICSE}})       = ASCIIChr
eltype(::Type{<:Str{LatinCSE}})       = LatinChr
eltype(::Type{<:Str{_LatinCSE}})      = _LatinChr
eltype(::Type{<:Str{<:UCS2_CSEs}})    = UCS2Chr
eltype(::Type{<:Str{<:Unicode_CSEs}}) = UTF32Chr

codepoint_size(::Type{T}) where {T<:Union{String,Str}} = sizeof(eltype(T))

get_codeunit(dat, pos) = codeunit(dat, pos)
get_codeunit(pnt::Ptr{<:CodeUnitTypes}, pos) = unsafe_load(pnt, pos)
get_codeunit(dat::AbstractVector{<:CodeUnitTypes}, pos) = dat[pos]
get_codeunit(str::Str, pos) = get_codeunit(pointer(str), pos)

codeunit(str::Str, pos::Integer) = get_codeunit(str, pos)

get_codeunit(dat) = get_codeunit(dat, 1)
get_codeunit(pnt::Ptr{<:CodeUnitTypes}) = unsafe_load(pnt)

set_codeunit!(pnt::Ptr{<:CodeUnitTypes}, pos, ch) = unsafe_store!(pnt, ch, pos)
set_codeunit!(dat::AbstractVector{<:CodeUnitTypes}, pos, ch) = (dat[pos] = ch)
set_codeunit!(dat::String, pos, ch) = unsafe_store!(pointer(dat), pos, ch)

set_codeunit!(pnt::Ptr{<:CodeUnitTypes}, ch) = unsafe_store!(pnt, ch)
set_codeunit!(dat::AbstractVector{<:CodeUnitTypes}, ch) = (dat[1] = ch)
set_codeunit!(dat::String, ch) = set_codeunit!(dat, 1, ch)

convert(::Type{T}, v::S) where {T<:Integer, S<:Chr} = convert(T, codepoint(v))::T
convert(::Type{T}, v::Signed) where {T<:Chr} =
    (v >= 0 && is_valid(T, v%Unsigned)) ? convert(T, v%Unsigned) : codepoint_error(T, v)
convert(::Type{T}, v::Unsigned) where {CS,B,T<:Chr{CS,B}} =
    is_valid(T, v) ? Chr(CS, v%B) : codepoint_error(T, v)
convert(::Type{Char}, v::T) where {T<:Chr} = convert(Char, codepoint(v))
convert(::Type{T}, v::Char) where {T<:Chr} = convert(T, codepoint(v))::T

rem(x::Number, ::Type{<:Chr{CS,B}}) where {CS,B} = Chr(CS, x%B)
rem(x::Char, ::Type{T}) where {T<:Chr}   = x%UInt32%T
rem(x::Chr, ::Type{T}) where {T<:Chr}    = (x.v)%T
rem(x::Chr, ::Type{T}) where {T<:Char}   = (x.v)%T
rem(x::Chr, ::Type{T}) where {T<:Number} = (x.v)%T

(::Type{S})(v::T) where {S<:Union{UInt32, Int, UInt}, T<:Chr} = codepoint(v)%S
(::Type{Char})(v::Chr) = Char(codepoint(v))
(::Type{T})(v::Char) where {T<:Chr} = T(codepoint(v))

eltype(::Type{T}) where {T<:Chr} = T
size(cp::Chr, dim) = convert(Int, dim) < 1 ? boundserr(cp, dim) : 1
getindex(cp::Chr, i::Integer) = i == 1 ? cp : boundserr(cp, i)
getindex(cp::Chr, I::Integer...) = all(x -> x == 1, I) ? cp : boundserr(cp, I)

_uni_rng(m) = 0x00000:ifelse(m < 0xd800, m, m-0x800)
codepoint_rng(::Type{T}) where {T<:Chr} = _uni_rng(typemax(T)%UInt32)
codepoint_rng(::Type{Char}) = _uni_rng(0x10ffff)
codepoint_rng(::Type{Text2Chr}) = 0%UInt16:typemax(UInt16)
codepoint_rng(::Type{Text4Chr}) = 0%UInt32:typemax(UInt32)

codepoint_adj(::Type{T}, ch) where {T} = ifelse(ch < 0xd800, ch, ch+0x800)%T
codepoint_adj(::Type{T}, ch) where {T<:Union{Text2Chr,Text4Chr}} = ch%T

# returns a random valid Unicode scalar value in the correct range for the type of character
Random.rand(r::Random.AbstractRNG, ::Random.SamplerType{T}) where {T<:Chr} =
    codepoint_adj(T, rand(r, codepoint_rng(T)))

==(x::Chr, y::AbsChar) = codepoint(x) == codepoint(y)
==(x::AbsChar, y::Chr) = codepoint(x) == codepoint(y)
==(x::Chr, y::Chr)     = codepoint(x) == codepoint(y)

isless(x::Chr, y::AbsChar) = codepoint(x) < codepoint(y)
isless(x::AbsChar, y::Chr) = codepoint(x) < codepoint(y)
isless(x::Chr, y::Chr)     = codepoint(x) < codepoint(y)

# This is so that the hash is compatible with isless, but it's very inefficient
Base.hash(x::Chr, h::UInt) = hash(Char(x), h)

