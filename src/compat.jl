# This file contains code that was a part of Julia
# License is MIT: see LICENSE.md

export Unicode
module Unicode
export normalize, graphemes, isassigned
const normalize  = Base.UTF8proc.normalize_string
const graphemes  = Base.UTF8proc.graphemes
const isassigned = Base.UTF8proc.is_assigned_char
end

## Start of code from operators.jl =================================================
##
## It is used to support the new string searching syntax on v0.6.2

"""
    Fix2(f, x)

A type representing a partially-applied version of function `f`, with the second
argument fixed to the value "x".
In other words, `Fix2(f, x)` behaves similarly to `y->f(y, x)`.
"""
struct Fix2{F,T} <: Function
    f::F
    x::T

    Fix2(f::F, x::T) where {F,T} = new{F,T}(f, x)
    Fix2(f::Type{F}, x::T) where {F,T} = new{Type{F},T}(f, x)
end

(f::Fix2)(y) = f.f(y, f.x)

"""
    isequal(x)

Create a function that compares its argument to `x` using [`isequal`](@ref), i.e.
a function equivalent to `y -> isequal(y, x)`.

The returned function is of type `Base.Fix2{typeof(isequal)}`, which can be
used to implement specialized methods.
"""
isequal(x) = Fix2(isequal, x)

const EqualTo = Fix2{typeof(isequal)}

"""
    ==(x)

Create a function that compares its argument to `x` using [`==`](@ref), i.e.
a function equivalent to `y -> y == x`.

The returned function is of type `Base.Fix2{typeof(==)}`, which can be
used to implement specialized methods.
"""
==(x) = Fix2(==, x)

"""
    in(x)

Create a function that checks whether its argument is [`in`](@ref) `x`, i.e.
a function equivalent to `y -> y in x`.

The returned function is of type `Base.Fix2{typeof(in)}`, which can be
used to implement specialized methods.
"""
in(x) = Fix2(in, x)

const OccursIn = Fix2{typeof(in)}

## end of code from operators.jl =================================================

## Start of codeunits support from basic.jl ======================================
##
##It is used for CodeUnit support in pre v0.7 versions of Julia

## code unit access ##

codeunit(str::AbstractString) = UInt8
codeunit(::Type{<:AbstractString}) = UInt8
ncodeunits(str::AbstractString) = sizeof(str)

"""
    CodeUnits(s::AbstractString)

Wrap a string (without copying) in an immutable vector-like object that accesses the code units
of the string's representation.
"""
struct CodeUnits{T,S<:AbstractString} <: DenseVector{T}
    s::S
    CodeUnits(s::S) where {S<:AbstractString} = new{codeunit(s),S}(s)
end

length(s::CodeUnits) = ncodeunits(s.s)
sizeof(s::CodeUnits{T}) where {T} = ncodeunits(s.s) * sizeof(T)
size(s::CodeUnits) = (length(s),)
strides(s::CodeUnits) = (1,)
@propagate_inbounds getindex(s::CodeUnits, i::Int) = codeunit(s.s, i)
IndexStyle(::Type{<:CodeUnits}) = IndexLinear()
start(s::CodeUnits) = 1
next(s::CodeUnits, i) = (@_propagate_inbounds_meta; (s[i], i+1))
done(s::CodeUnits, i) = (@_inline_meta; i == length(s)+1)

write(io::IO, s::CodeUnits) = write(io, s.s)

unsafe_convert(::Type{Ptr{T}},    s::CodeUnits{T}) where {T} = unsafe_convert(Ptr{T}, s.s)
unsafe_convert(::Type{Ptr{Int8}}, s::CodeUnits{UInt8}) = unsafe_convert(Ptr{Int8}, s.s)

"""
    codeunits(s::AbstractString)

Obtain a vector-like object containing the code units of a string.
Returns a `CodeUnits` wrapper by default, but `codeunits` may optionally be defined
for new string types if necessary.
"""
codeunits(s::AbstractString) = CodeUnits(s)

## end of codeunits support ============================================================

size(cp::Chr) = ()
ndims(cp::Chr) = 0
ndims(::Type{<:Chr}) = 0
length(cp::Chr) = 1
lastindex(cp::Chr) = 1
getindex(cp::Chr) = cp
first(cp::Chr) = cp
last(cp::Chr) = cp
start(cp::Chr) = false
next(cp::Chr, state) = (cp, true)
done(cp::Chr, state) = state
isempty(cp::Chr) = false
in(x::AbsChar, y::Chr) = x == y
in(x::Chr, y::AbsChar) = x == y
in(x::Chr, y::Chr) = x == y
-(x::AbsChar, y::Chr) = Int(x) - Int(y)
-(x::Chr, y::AbsChar) = Int(x) - Int(y)
-(x::Chr, y::Chr) = Int(x) - Int(y)
-(x::Chr, y::Integer) = Chr((Int32(x) - Int32(y))%UInt32)
+(x::Chr, y::Integer) = Chr((Int32(x) + Int32(y))%UInt32)
+(x::Integer, y::Chr) = y + x
show(io::IO, cp::Chr)  = show(io, Char(cp))
print(io::IO, cp::Chr) = print(io, Char(cp))

codepoint(v::Char) = v%UInt32

unsafe_crc32c(a, n, crc) = ccall(:jl_crc32c, UInt32, (UInt32, Ptr{UInt8}, Csize_t), crc, a, n)

function sizeof(str::SubString{T}) where {T<:Str}
    is_multi(str) || return str.endof
    str.endof == 0 && return 0
    _nextind(CodeUnitMulti(), str.string, str.offset + str.endof) - str.offset - 1
end

occurs_in(str::String, hay::String) = contains(hay, str)
occurs_in(chr::Char,   hay::String) = contains(hay, string(chr))
Base.contains(hay::AbstractString, str::Str)     = occurs_in(str, hay)
Base.contains(hay::Str, str::AbstractString)     = occurs_in(str, hay)
Base.contains(hay::Str, str::Str)                = occurs_in(str, hay)
Base.contains(hay::AbstractString, chr::AbsChar) = occurs_in(chr, hay)
Base.contains(hay::AbstractString, pat::Regex)   = occurs_in(pat, hay)

const utf8crc = Base.crc32c

import Base: isalnum, isgraph, islower, isupper
const is_alphanumeric = isalnum
const is_graphic      = isgraph
const is_lowercase    = islower
const is_uppercase    = isupper

# These are deprecated in v0.7
for sym in (:bin, :oct, :dec, :hex)
    @eval import Base:$sym
    @eval ($sym)(x::Chr, p::Int) = ($sym)(codepoint(x), p, false)
    @eval ($sym)(x::Chr)         = ($sym)(codepoint(x), 1, false)
end

function repeat(ch::Char, cnt::Integer)
    cnt > 1 && return String(_repeat(CodeUnitMulti(), UTF8CSE, ch%UInt32, cnt))
    cnt < 0 && repeaterr(cnt)
    cnt == 0 ? empty_string : string(Char(ch%UInt32))
end

@noinline index_error(s::AbstractString, i::Integer) =
    throw(UnicodeError(UTF_ERR_INVALID_INDEX, Int(i), codeunit(s, i)))

function thisind(str::String, pos::Integer)
    @boundscheck 0 < pos <= ncodeunits(str) || boundserr(str, pos)
    pnt = pointer(str) + pos - 1
    pos - (checkcont(pnt) ? (checkcont(pnt - 1) ? (checkcont(pnt - 2) ? 3 : 2) : 1) : 0)
end

typemin(::Type{String}) = ""

macro preserve(args...)
    syms = args[1:end-1]
    for x in syms
        isa(x, Symbol) || error("Preserved variable must be a symbol")
    end
    #=
    s, r = gensym(), gensym()
    esc(quote
        $s = $(Expr(:gc_preserve_begin, syms...))
        $r = $(args[end])
        $(Expr(:gc_preserve_end, s))
        $r
        $(args[end])
    end)
    =#
    esc(quote ; $(args[end]) ; end)
end

Base.SubString(str::AbstractString, rng::UnitRange) = SubString(str, first(rng), last(rng))

Base.checkbounds(::Type{Bool}, s::AbstractString, i::Integer) = 1 <= i <= ncodeunits(s)

const _Chars = Union{<:Chr,Tuple{Vararg{<:Chr}},AbstractVector{<:Chr},Set{<:Chr},Base.Chars}

starts_with(str::AbstractString, chars::_Chars) = !is_empty(str) && first(str) in chars

ends_with(str::AbstractString, chars::_Chars) = !is_empty(str) && last(str) in chars

function Base.lstrip(s::AbstractString, chars::_Chars)
    e = lastindex(s)
    for (i, c) in pairs(s)
        c in chars || return SubString(s, i, e)
    end
    SubString(s, e+1, e)
end

function Base.rstrip(s::AbstractString, chars::_Chars)
    r = RevString(s)
    i = start(r)
    while !done(r,i)
        c, j = next(r,i)
        c in chars || return s[1:end-i+1]
        i = j
    end
    s[1:0]
end

Base.strip(s::AbstractString, chars::_Chars) = lstrip(rstrip(s, chars), chars)
