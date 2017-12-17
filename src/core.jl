# Core functions

# Need to optimize these to avoid doing array reinterpret
#=
getindex(s::T, r::Vector) where {T} = T(getindex(_data(s), r))
getindex(s::T, r) where {T} = T(getindex(_data(s), r))
getindex(s::T, rge{Int}) where {T} = T(getindex(_data(s), r))
getindex(s::T, indx::AbstractVector{Int}) where {T} = T(_data(s)[indx])
=#

@propagate_inbounds next(it::CodeUnits{T}, pos::Int) where {T<:Union{String,Str}} =
    _next(CodePointStyle(T), codeunit_type(T), it.xs, pos)

_endof(::CodeUnitSingle, str) = (@_inline_meta(); _len(str))

@propagate_inbounds _getindex(::CodeUnitSingle, T, str, i::Int) =
    (@_inline_meta(); T(get_codeunit(_pnt(str), _ind2chr(CodeUnitSingle(), str, i))))

@propagate_inbounds function _next(::CodeUnitSingle, T, str, pos)
    @_inline_meta()
    len, pnt = _lenpnt(str)
    @boundscheck pos <= len || throw(BoundsError(str, pos))
    try
        T(get_codeunit(pnt, pos)), pos + 1
    catch ex
        println("Type: ", T, ":", pos, ":", _data(str))
        rethrow(ex)
    end
end

@propagate_inbounds getindex(::CodeUnitMulti, T, str, i::Int) =
    _next(CodeUnitMulti(), T, str, i)[1]

_length(::CodeUnitSingle, str) = (@_inline_meta(); _len(str))
_isvalid(::CodeUnitSingle, str, i) = (@_inline_meta(); 1 <= i <= _len(str))

_prevind(::CodeUnitSingle, str, i) = Int(i) - 1
@propagate_inbounds function _prevind(::CodeUnitSingle, str, i, nchar)
    @boundscheck nchar > 0 || throw(ArgumentError("nchar must be greater than 0"))
    Int(i) - nchar
end

_nextind(::CodeUnitSingle, str, i) = Int(i) + 1
@propagate_inbounds function _nextind(::CodeUnitSingle, str, i, nchar)
    @boundscheck nchar > 0 || throw(ArgumentError("nchar must be greater than 0"))
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
@propagate_inbounds endof(str::T) where {T<:Str} =
    (@_inline_meta(); _endof(CodePointStyle(T), str))
@propagate_inbounds getindex(str::T, i::Int) where {T<:Str} =
    (@_inline_meta(); _getindex(CodePointStyle(T), codeunit_type(T), str, i))
@propagate_inbounds next(str::T, i::Int) where {T<:Str} =
    (@_inline_meta(); _next(CodePointStyle(T), codepoint_type(T), str, i))
@propagate_inbounds length(str::T) where {T<:Str} =
    (@_inline_meta(); _length(CodePointStyle(T), str))
@propagate_inbounds isvalid(str::T, i::Integer) where {T<:Str} =
    (@_inline_meta(); _isvalid(CodePointStyle(T), str, i))
@propagate_inbounds prevind(str::Str, i::Integer) where {T<:Str} =
    (@_inline_meta(); _prevind(CodePointStyle(T), str, i))
@propagate_inbounds nextind(str::Str, i::Integer) where {T<:Str} =
    (@_inline_meta(); _nextind(CodePointStyle(T), str, i))
@propagate_inbounds prevind(str::Str, i::Integer, nchar::Integer) where {T<:Str} =
    (@_inline_meta(); _prevind(CodePointStyle(T), str, i, nchar))
@propagate_inbounds nextind(str::Str, i::Integer, nchar::Integer) where {T<:Str} =
    (@_inline_meta(); _nextind(CodePointStyle(T), str, i, nchar))
@propagate_inbounds ind2chr(str::Str, i::Integer) where {T<:Str} =
    _ind2chr(CodePointStyle(T), str, i)
@propagate_inbounds chr2ind(str::Str, i::Integer) where {T<:Str} =
    _chr2ind(CodePointStyle(T), str, i)

# Handle substrings of Str

@propagate_inbounds length(str::S) where {S<:SubString{T}} where {T<:Str} =
    _endof(CodePointStyle(T), str)

isvalid(str::SubString{<:Str}, i::Integer) = (start(str) <= i <= endof(str))

@propagate_inbounds ind2chr(str::S, i::Integer) where {S<:SubString{T}} where {T<:Str} =
    _ind2chr(CodePointStyle(T), str, i)
@propagate_inbounds chr2ind(str::S, i::Integer) where {S<:SubString{T}} where {T<:Str} =
    _chr2ind(CodePointStyle(T), str, i)

@propagate_inbounds _reverseind(::CodeUnitSingle, str::T, i) where {T<:Str} =
    (@_inline_meta(); _length(CodeUnitSingle(), str) + 1 - i)
@propagate_inbounds reverseind(str::T, i::Integer) where {T<:Str} =
    _reverseind(CodePointStyle(T), str, i)
@propagate_inbounds reverseind(str::S, i::Integer) where {S<:SubString{T}} where {T<:Str} =
    _reverseind(CodePointStyle(T), str, i)

@propagate_inbounds function _collectstr(::CodeUnitMulti, ::Type{S}, str::T) where {S,T<:Str}
    len = _length(CodeUnitMulti(), str)
    vec = Vector{S}(uninitialized, len)
    _transcode(vec, _pnt(str), len)
    vec
end

@propagate_inbounds function _collectstr(::CodeUnitSingle, ::Type{S}, str::T) where {S,T<:Str}
    len, pnt = _lenpnt(str)
    vec = Vector{S}(uninitialized, len)
    cpt = codepoint_type(T)
    if S == cpt
        @inbounds unsafe_copy!(reinterpret(Ptr{basetype(cpt)}, pointer(vec)), 1, pnt, 1, len)
    else
        @inbounds for i = 1:len
            vec[i] = T(get_codepoint(pnt, i))
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
search(str::Str, ch::Char, i::Integer) = search(str, UInt32(ch), i)
rsearch(str::Str, ch::Char, i::Integer) = rsearch(str, UInt32(ch), i)
convert(str::Str, ch::Char) = convert(str, UInt32(ch))
