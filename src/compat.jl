# Handle some name changes between v0.6 and master
const copyto! = copy!
const unsafe_copyto! = unsafe_copy!
const Nothing = Void
const Cvoid = Void
abstract type AbstractChar end
export AbstractChar
import Base: find

# Handle changes in array allocation
create_vector(T, len) = Vector{T}(len)

# Add new short name for deprecated hex function
outhex(v, p=1) = hex(v,p)

include("codeunits.jl")
include("fix2.jl")

module Unicode
export normalize, graphemes, isassigned
const normalize  = Base.UTF8proc.normalize_string
const graphemes  = Base.UTF8proc.graphemes
const isassigned = Base.UTF8proc.is_assigned_char
end

size(cp::CodePoint) = ()
ndims(cp::CodePoint) = 0
ndims(::Type{<:CodePoint}) = 0
length(cp::CodePoint) = 1
lastindex(cp::CodePoint) = 1
getindex(cp::CodePoint) = cp
first(cp::CodePoint) = cp
last(cp::CodePoint) = cp
start(cp::CodePoint) = false
next(cp::CodePoint, state) = (cp, true)
done(cp::CodePoint, state) = state
isempty(cp::CodePoint) = false
in(x::CodePoint, y::CodePoint) = x == y
-(x::CodePoint, y::CodePoint) = Int(x) - Int(y)
-(x::CodePoint, y::Integer) = CodePoint((Int32(x) - Int32(y))%UInt32)
+(x::CodePoint, y::Integer) = CodePoint((Int32(x) + Int32(y))%UInt32)
+(x::Integer, y::CodePoint) = y + x
show(io::IO, cp::CodePoint)  = show(io, Char(cp))
print(io::IO, cp::CodePoint) = print(io, Char(cp))

codepoint(v::Char) = v%UInt32

unsafe_crc32c(a, n, crc) = ccall(:jl_crc32c, UInt32, (UInt32, Ptr{UInt8}, Csize_t), crc, a, n)

sizeof(s::SubString{<:Str}) = s.endof == 0 ? 0 : nextind(s, s.endof) - 1

Base.contains(hay::AbstractString, str::Str)     = _occurs_in(str, hay)
Base.contains(hay::Str, str::AbstractString)     = _occurs_in(str, hay)
Base.contains(hay::Str, str::Str)                = _occurs_in(str, hay)
Base.contains(hay::AbstractString, chr::AbsChar) = _occurs_in(chr, hay)
Base.contains(hay::AbstractString, pat::Regex)   = _occurs_in(pat, hay)
const occurs_in = _occurs_in

const utf8crc = Base.crc32c

import Base.UTF8proc: isgraphemebreak, isgraphemebreak!, graphemes

import Base: isalnum, isgraph, islower, isupper
const is_alphanumeric = isalnum
const is_graphic      = isgraph
const is_lowercase    = islower
const is_uppercase    = isupper

# These are deprecated in v0.7
for sym in (:bin, :oct, :dec, :hex)
    @eval import Base:$sym
    @eval ($sym)(x::CodePoint, p::Int) = ($sym)(codepoint(x), p, false)
    @eval ($sym)(x::CodePoint)         = ($sym)(codepoint(x), 1, false)
end

function repeat(ch::Char, cnt::Integer)
    cnt > 1 && return String(_repeat(CodeUnitMulti(), UTF8CSE, ch%UInt32, cnt))
    cnt < 0 && repeaterr(cnt)
    cnt == 0 ? empty_string : string(Char(ch%UInt32))
end

@noinline index_error(s::AbstractString, i::Integer) =
    throw(UnicodeError(UTF_ERR_INVALID_INDEX, Int(i), codeunit(s, i)))

function thisind(str::String, pos::Integer)
    @boundscheck 0 < pos <= _len(str) || boundserr(str, pos)
    pnt = _pnt(str) + pos - 1
    pos - (checkcont(pnt) ? (checkcont(pnt - 1) ? (checkcont(pnt - 2) ? 3 : 2) : 1) : 0)
end

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
