#=
Core functions


Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones, and others (see Julia contributors)
Licensed under MIT License, see LICENSE.md

Inspired by / derived from code in Julia
=#

# Todo: Need to optimize these to avoid doing array reinterpret
#=
getindex(s::T, r::Vector) where {T} = T(getindex(_data(s), r))
getindex(s::T, r) where {T} = T(getindex(_data(s), r))
getindex(s::T, rge{Int}) where {T} = T(getindex(_data(s), r))
getindex(s::T, indx::AbstractVector{Int}) where {T} = T(_data(s)[indx])
=#

_lastindex(::CodeUnitSingle, str) = (@_inline_meta(); _len(str))

@propagate_inbounds _getindex(::CodeUnitSingle, T, str, i::Int) =
    (@_inline_meta(); T(get_codeunit(_pnt(str), _ind2chr(CodeUnitSingle(), str, i))))

@propagate_inbounds function _next(::CodeUnitSingle, T, str, pos)
    @_inline_meta()
    @boundscheck 0 < pos <= _len(str) || boundserr(str, pos)
    T(get_codeunit(str, pos)), pos + 1
end

_nextcpfun(::CodeUnitSingle, ::Type{S}, pnt::Ptr{T}) where {S,T<:CodeUnitTypes} =
    get_codeunit(pnt), pnt + sizeof(T)
_nextcp(::Type{T}, pnt) where {T} = _nextcpfun(CodePointStyle(T), T, pnt)

@propagate_inbounds _getindex(::CodeUnitMulti, T, str, i::Int) =
    _next(CodeUnitMulti(), T, str, i)[1]

_length(::CodeUnitSingle, str) = (@_inline_meta(); _len(str))

_thisind(::CodeUnitSingle, str, i) = Int(i)
_thisind(::CodeUnitSingle, str, len, pnt, i) = Int(i)

_prevind(::CodeUnitSingle, str, i) = Int(i) - 1
@propagate_inbounds function _prevind(::CodeUnitSingle, str, i, nchar)
    @boundscheck nchar > 0 || ncharerr(nchar)
    Int(i) - nchar
end

_nextind(::CodeUnitSingle, str, i) = Int(i) + 1
@propagate_inbounds function _nextind(::CodeUnitSingle, str, i, nchar)
    @boundscheck nchar > 0 || ncharerr(nchar)
    Int(i) + nchar
end

@propagate_inbounds function _ind2chr(::CodeUnitSingle, str, i)
    @_inline_meta()
    @boundscheck checkbounds(str, i)
    i
end
@propagate_inbounds function _chr2ind(::CodeUnitSingle, str, i)
    @_inline_meta()
    @boundscheck checkbounds(str, i)
    i
end

_index(cs::CodePointStyle, str, i)               = _thisind(cs, str, i)
_index(cs::CodePointStyle, ::Fwd, str, i)        = _nextind(cs, str, i)
_index(cs::CodePointStyle, ::Fwd, str, i, nchar) = _nextind(cs, str, i, nchar)
_index(cs::CodePointStyle, ::Rev, str, i)        = _prevind(cs, str, i)
_index(cs::CodePointStyle, ::Rev, str, i, nchar) = _prevind(cs, str, i, nchar)

#  Call to specialized version via trait
@propagate_inbounds lastindex(str::T) where {T<:Str} =
    (@_inline_meta(); _lastindex(CodePointStyle(T), str))
@propagate_inbounds getindex(str::T, i::Int) where {T<:Str} =
    (@_inline_meta(); R = eltype(T) ; _getindex(CodePointStyle(T), R, str, i)::R)
@propagate_inbounds next(str::T, i::Int) where {T<:Str} =
    (@_inline_meta(); R = eltype(T) ; _next(CodePointStyle(T), R, str, i)::Tuple{R,Int})
@propagate_inbounds length(str::T) where {T<:Str} =
    (@_inline_meta(); _length(CodePointStyle(T), str))
@propagate_inbounds thisind(str::T, i::Int) where {T<:Str} =
    (@_inline_meta(); _thisind(CodePointStyle(T), str, i))
@propagate_inbounds prevind(str::T, i::Int) where {T<:Str} =
    (@_inline_meta(); _prevind(CodePointStyle(T), str, i))
@propagate_inbounds nextind(str::T, i::Int) where {T<:Str} =
    (@_inline_meta(); _nextind(CodePointStyle(T), str, i))
@propagate_inbounds prevind(str::T, i::Int, nchar::Int) where {T<:Str} =
    (@_inline_meta(); _prevind(CodePointStyle(T), str, i, nchar))
@propagate_inbounds nextind(str::T, i::Int, nchar::Int) where {T<:Str} =
    (@_inline_meta(); _nextind(CodePointStyle(T), str, i, nchar))

@propagate_inbounds index(str::T, i::Int) where {T<:Str} =
    (@_inline_meta(); _index(CodePointStyle(T), str, i))
@propagate_inbounds index(::D, str::T, i::Int) where {T<:Str,D<:Direction} =
    (@_inline_meta(); _index(CodePointStyle(T), D(), str, i))
@propagate_inbounds index(::D, str::T, i::Int, nchar::Int) where {T<:Str,D<:Direction} =
    (@_inline_meta(); _index(CodePointStyle(T), D(), str, i, nchar))

# This is deprecated on v0.7, recommended change to length(str, 1, i)
@propagate_inbounds ind2chr(str::T, i::Int) where {T<:Str} =
    _ind2chr(CodePointStyle(T), str, i)
# This is deprecated on v0.7, recommended change to nextind(str, 0, i)
@propagate_inbounds chr2ind(str::T, i::Int) where {T<:Str} =
    _chr2ind(CodePointStyle(T), str, i)

@propagate_inbounds function is_valid(str::T, i::Integer) where {T<:Str}
    @_inline_meta()
    @boundscheck 1 <= i <= _len(str) || return false
    _isvalid_char_pos(CodePointStyle(T), str, i)
end

_isvalid_char_pos(::CodeUnitSingle, str, i) = true

#=
# Handle substrings of Str

@propagate_inbounds length(str::S) where {S<:SubString{T}} where {T<:Str} =
    _lastindex(CodePointStyle(T), str)

is_valid(str::T, i::Integer) where {T<:SubString{<:Str}} =
    (start(str) <= i <= _lastindex(CodePointStyle(T), str))

@propagate_inbounds ind2chr(str::S, i::Integer) where {S<:SubString{T}} where {T<:Str} =
    _ind2chr(CodePointStyle(T), str, i)
@propagate_inbounds chr2ind(str::S, i::Integer) where {S<:SubString{T}} where {T<:Str} =
    _chr2ind(CodePointStyle(T), str, i)
@propagate_inbounds reverseind(str::S, i::Integer) where {S<:SubString{T}} where {T<:Str} =
    _reverseind(CodePointStyle(T), str, i)
=#

@propagate_inbounds _reverseind(::CodeUnitSingle, str::T, i) where {T<:Str} =
    (@_inline_meta(); _len(str) + 1 - i)
@propagate_inbounds reverseind(str::T, i::Integer) where {T<:Str} =
    _reverseind(CodePointStyle(T), str, i)

@propagate_inbounds function _collectstr(::CodeUnitMulti, ::Type{S}, str::T) where {S,T<:Str}
    len = _length(CodeUnitMulti(), str)
    vec = create_vector(S, len)
    pos = 1
    @inbounds for i = 1:len
        vec[i], pos = _next(CodeUnitMulti(), S, str, pos)
    end
    vec
end

@propagate_inbounds function _collectstr(::CodeUnitSingle, ::Type{S}, str::T) where {S,T<:Str}
    len, pnt = _lenpnt(str)
    vec = create_vector(S, len)
    cpt = eltype(T)
    if S == cpt
        @inbounds unsafe_copyto!(reinterpret(Ptr{basetype(cpt)}, pointer(vec)), pnt, len)
    else
        @inbounds for i = 1:len
            vec[i] = T(get_codeunit(pnt, i))
        end
    end
    vec
end

@propagate_inbounds collect(str::T) where {T<:Str} =
    _collectstr(CodePointStyle(T), eltype(T), str)

# An optimization here would be to check just if they are the same type, but
# rather if they are the same size with a compatible encoding, i.e. like
# UTF32Chr and UInt32, but not Char and UInt32.

@propagate_inbounds Base._collect(::Type{S}, str::T, isz::Base.HasLength) where {S,T<:Str} =
    _collectstr(CodePointStyle(T), S, str)

@inline function check_valid(ch, pos)
    is_surrogate_codeunit(ch) && unierror(UTF_ERR_SURROGATE, pos, ch)
    ch <= 0x10ffff || unierror(UTF_ERR_INVALID, pos, ch)
    ch
end

# Convert single characters to strings
convert(::Type{T}, ch::Char) where {T<:Str} = convert(T, UInt32(ch))

function _convert(::Type{C}, ch::T) where {C<:CSE,T<:CodeUnitTypes}
    buf, pnt = _allocate(T, 1)
    set_codeunit!(pnt, ch)
    Str(C, buf)
end

# Todo: These should be made more generic, work for all CodeUnitSingle types

convert(::Type{<:Str{ASCIICSE}}, ch::Unsigned) =
    is_ascii(ch) ? _convert(ASCIICSE, ch%UInt8) : unierror(UTF_ERR_INVALID_ASCII, 0, ch)
convert(::Type{<:Str{LatinCSE}}, ch::Unsigned) =
    is_latin(ch) ? _convert(LatinCSE, ch%UInt8) : unierror(UTF_ERR_INVALID_LATIN1, 0, ch)
convert(::Type{<:Str{UCS2CSE}}, ch::Unsigned) =
    is_bmp(ch) ? _convert(UCS2CSE, ch%UInt16) : unierror(UTF_ERR_INVALID, 0, ch)
convert(::Type{<:Str{UTF32CSE}}, ch::Unsigned) =
    is_unicode(ch) ? _convert(UTF32CSE, ch%UInt32) : unierror(UTF_ERR_INVALID, 0, ch)

convert(::Type{T}, ch::Signed) where {T<:Str} = ch < 0 ? ncharerr(ch) : convert(T, ch%Unsigned)

Str(str::SubString{<:Str{C}}) where {C<:Byte_CSEs} =
    Str(C, unsafe_string(pointer(str.string, str.offset+1), str.ncodeunits))

#=
function cmp(a::SubString{String}, b::SubString{String})
    na = sizeof(a)
    nb = sizeof(b)
    c = ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt),
              pointer(a), pointer(b), min(na, nb))
    return c < 0 ? -1 : c > 0 ? +1 : cmp(na, nb)
end
=#
# don't make unnecessary copies when passing substrings to C functions
cconvert(::Type{Ptr{UInt8}}, str::SubString{<:ByteStr}) = str
cconvert(::Type{Ptr{Int8}},  str::SubString{<:ByteStr}) = str

unsafe_convert(::Type{Ptr{UInt8}}, s::SubString{<:ByteStr}) =
    convert(Ptr{UInt8}, pointer(s.string)) + s.offset
unsafe_convert(::Type{Ptr{Int8}}, s::SubString{<:ByteStr}) =
    convert(Ptr{Int8}, pointer(s.string)) + s.offset

function _reverse(::CodeUnitSingle, ::Type{C}, len, str::Str{C}) where {C<:CSE}
    len < 2 && return str
    @preserve str begin
        pnt = _pnt(str)
        T = codeunit(C)
        buf, beg = _allocate(T, len)
        out = bytoff(beg, len)
        while out > beg
            set_codeunit!(out -= sizeof(T), get_codeunit(pnt))
            pnt += sizeof(T)
        end
        Str(C, buf)
    end
end

function _reverse(::CodeUnitMulti, ::Type{C}, len, str) where {C<:CSE}
    @inbounds ((t = nextind(str, 0)) > len || nextind(str, t) > len) && return str
    @preserve str _reverse(CodeUnitMulti(), C, len, _pnt(str))
end

reverse(str::T) where {C<:CSE,T<:Union{Str{C},SubString{Str{C}}}} =
    _reverse(CodePointStyle(str), C, _len(str), str)
