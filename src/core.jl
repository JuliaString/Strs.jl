#=
Core functions


Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and others (see Julia contributors)
Licensed under MIT License, see LICENSE.md

Inspired by / derived from code in Julia
=#

# Need to optimize these to avoid doing array reinterpret
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
_isvalid(::CodeUnitSingle, str, i) = (@_inline_meta(); 1 <= i <= _len(str))

_thisind(::CodeUnitSingle, str, i) = Int(i)

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

#  Call to specialized version via trait
@propagate_inbounds lastindex(str::T) where {T<:Str} =
    (@_inline_meta(); _lastindex(CodePointStyle(T), str))
@propagate_inbounds getindex(str::T, i::Int) where {T<:Str} =
    (@_inline_meta(); _getindex(CodePointStyle(T), codepoint_type(T), str, i))
@propagate_inbounds next(str::T, i::Int) where {T<:Str} =
    (@_inline_meta(); _next(CodePointStyle(T), codepoint_type(T), str, i))
@propagate_inbounds length(str::T) where {T<:Str} =
    (@_inline_meta(); _length(CodePointStyle(T), str))
@propagate_inbounds isvalid(str::T, i::Integer) where {T<:Str} =
    (@_inline_meta(); _isvalid(CodePointStyle(T), str, i))
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
@propagate_inbounds ind2chr(str::T, i::Int) where {T<:Str} =
    _ind2chr(CodePointStyle(T), str, i)
@propagate_inbounds chr2ind(str::T, i::Int) where {T<:Str} =
    _chr2ind(CodePointStyle(T), str, i)

#=
# Handle substrings of Str

@propagate_inbounds length(str::S) where {S<:SubString{T}} where {T<:Str} =
    _lastindex(CodePointStyle(T), str)

isvalid(str::T, i::Integer) where {T<:SubString{<:Str}} =
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
    cpt = codepoint_type(T)
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
    _collectstr(CodePointStyle(T), codepoint_type(T), str)

# An optimization here would be to check just if they are the same type, but
# rather if they are the same size with a compatible encoding, i.e. like
# UTF32Chr and UInt32, but not Char and UInt32.

@propagate_inbounds Base._collect(::Type{S}, str::T, isz::Base.HasLength) where {S,T<:Str} =
    _collectstr(CodePointStyle(T), S, str)

# Extra functions
convert(str::Str, ch::Char) = convert(str, UInt32(ch))
