#=
Core functions


Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones, and others (see Julia contributors)
Licensed under MIT License, see LICENSE.md

Inspired by / derived from code in Julia
=#

_lastindex(::SingleCU, str) = (@_inline_meta(); ncodeunits(str))

@propagate_inbounds function _getindex(::SingleCU, T, str, pos::Int)
    @_inline_meta()
    @boundscheck checkbounds(str, pos)
    T(get_codeunit(pointer(str), pos))
end

@propagate_inbounds function _next(::SingleCU, T, str, pos)
    @_inline_meta()
    @boundscheck 0 < pos <= ncodeunits(str) || boundserr(str, pos)
    T(get_codeunit(str, pos)), pos + 1
end

_nextcpfun(::SingleCU, ::Type{S}, pnt::Ptr{T}) where {S,T<:CodeUnitTypes} =
    get_codeunit(pnt), pnt + sizeof(T)
_nextcp(::Type{T}, pnt) where {T} = _nextcpfun(CodePointStyle(T), T, pnt)

@propagate_inbounds _getindex(::MultiCU, T, str, pos::Int) =
    first(_next(MultiCU(), T, str, pos))

@inline _length(::SingleCU, str) = ncodeunits(str)

# Use more generic length check
@inline _length_check(str::SubString{<:Str{C}}, cnt) where {C<:CSE} =
    _length(MultiCU(), C, pointer(str), cnt)

# Go directly to aligned length check
@inline _length_check(str::Str{C}, cnt) where {C<:CSE} =
    _length_al(MultiCU(), C, pointer(str), cnt)

@inline _length(::MultiCU, str::MaybeSub{T}) where {T<:Str} =
    (cnt = ncodeunits(str); cnt < 2 ? Int(cnt > 0) : @preserve str _length_check(str, cnt))

@inline _length(::SingleCU, ::Type{<:CSE}, ::Ptr{<:CodeUnitTypes}, cnt::Int) = cnt

@inline _length(::MultiCU, str::Str{RawUTF8CSE}) = length(str.data)
@inline _length(::MultiCU, str::Str{RawUTF8CSE}, i::Int, j::Int) = length(str.data, i, j)

@propagate_inbounds function _length(cs::CodePointStyle, str, i::Int, j::Int)
    @boundscheck begin
        # I think the bounds of these should be 1:siz
        lim = ncodeunits(str)+1
        0 <  i <= lim || boundserr(str, i)
        0 <= j <  lim || boundserr(str, j)
    end
    (cnt = j - i + 1) <= 0 ? 0 :
        @preserve str _length(cs, cse(str), bytoff(pointer(str), i - 1), cnt)
end

@inline _thisind(::SingleCU, str, len, pnt, pos) = Int(pos)

@propagate_inbounds function _thisind(cs::CS, str, pos) where {CS<:CodePointStyle}
    @_inline_meta()
    # I do think thisind should not return anything outside of the valid range
    # but for now, to make it compatible with the current String API, do this:
    pos == 0 && return 0
    len = ncodeunits(str)
    pos == len + 1 && return pos
    @boundscheck 0 < pos <= len || boundserr(str, pos)
    @preserve str _thisind(cs, str, len, pointer(str), pos)
end

@propagate_inbounds function _prevind(::SingleCU, str, i)
    @_inline_meta()
    @boundscheck 0 < i <= ncodeunits(str)+1 || boundserr(str, i)
    Int(i) - 1
end

@propagate_inbounds function _prevind(::SingleCU, str, i, nchar)
    @_inline_meta()
    nchar < 0 && ncharerr(nchar)
    @boundscheck 0 < i <= ncodeunits(str)+1 || boundserr(str, i)
    max(Int(i) - nchar, 0)
end

@propagate_inbounds function _nextind(::SingleCU, str, i)
    @_inline_meta()
    @boundscheck 0 <= i <= ncodeunits(str) || boundserr(str, i)
    Int(i) + 1
end

@propagate_inbounds function _nextind(::SingleCU, str, i, nchar)
    @_inline_meta()
    nchar < 0 && ncharerr(nchar)
    @boundscheck 0 <= i <= ncodeunits(str) || boundserr(str, i)
    min(Int(i) + nchar, ncodeunits(str)+1)
end

_index(cs::CodePointStyle, str, i)               = _thisind(cs, str, i)
_index(cs::CodePointStyle, ::Fwd, str, i)        = _nextind(cs, str, i)
_index(cs::CodePointStyle, ::Fwd, str, i, nchar) = _nextind(cs, str, i, nchar)
_index(cs::CodePointStyle, ::Rev, str, i)        = _prevind(cs, str, i)
_index(cs::CodePointStyle, ::Rev, str, i, nchar) = _prevind(cs, str, i, nchar)

#  Call to specialized version via trait
@propagate_inbounds lastindex(str::MaybeSub{T}) where {T<:Str} =
    (@_inline_meta(); _lastindex(CodePointStyle(T), str))
@propagate_inbounds getindex(str::MaybeSub{T}, i::Int) where {T<:Str} =
    (@_inline_meta(); R = eltype(T) ; _getindex(CodePointStyle(T), R, str, i)::R)
@propagate_inbounds next(str::T, i::Int) where {T<:Str} =
    (@_inline_meta(); R = eltype(T) ; _next(CodePointStyle(T), R, str, i)::Tuple{R,Int})
@propagate_inbounds next(str::SubString{T}, i::Int) where {T<:Str} =
    (@_inline_meta(); R = eltype(T) ; _next(CodePointStyle(T), R, str, i)::Tuple{R,Int})
@propagate_inbounds length(str::MaybeSub{T}) where {T<:Str} =
    (@_inline_meta(); _length(CodePointStyle(T), str))
@propagate_inbounds length(str::MaybeSub{T}, i::Int, j::Int) where {T<:Str} =
    (@_inline_meta(); _length(CodePointStyle(T), str, i, j))
@propagate_inbounds thisind(str::MaybeSub{T}, i::Int) where {T<:Str} =
    (@_inline_meta(); _thisind(CodePointStyle(T), str, i))
@propagate_inbounds prevind(str::MaybeSub{T}, i::Int) where {T<:Str} =
    (@_inline_meta(); _prevind(CodePointStyle(T), str, i))
@propagate_inbounds nextind(str::MaybeSub{T}, i::Int) where {T<:Str} =
    (@_inline_meta(); _nextind(CodePointStyle(T), str, i))
@propagate_inbounds prevind(str::MaybeSub{T}, i::Int, nchar::Int) where {T<:Str} =
    (@_inline_meta(); _prevind(CodePointStyle(T), str, i, nchar))
@propagate_inbounds nextind(str::MaybeSub{T}, i::Int, nchar::Int) where {T<:Str} =
    (@_inline_meta(); _nextind(CodePointStyle(T), str, i, nchar))

@propagate_inbounds index(str::MaybeSub{T}, i::Integer) where {T<:Str} =
    (@_inline_meta(); _index(CodePointStyle(T), str, Int(i)))
@propagate_inbounds index(::D, str::MaybeSub{T}, i::Integer) where {T<:Str,D<:Direction} =
    (@_inline_meta(); _index(CodePointStyle(T), D(), str, Int(i)))
@propagate_inbounds index(::D, str::MaybeSub{T}, i::Integer,
                          nchar::Integer) where {T<:Str,D<:Direction} =
    (@_inline_meta(); _index(CodePointStyle(T), D(), str, Int(i), Int(nchar)))

@propagate_inbounds reverseind(str::MaybeSub{T}, i::Integer) where {T<:Str} =
    (@_inline_meta(); _index(CodePointStyle(T), str, Int(ncodeunits(str) - i + 1)))

@static if V6_COMPAT
    @propagate_inbounds ind2chr(str::MaybeSub{T}, i::Int) where {T<:Str} = length(str, 1, i)
    @propagate_inbounds chr2ind(str::MaybeSub{T}, i::Int) where {T<:Str} = nextind(str, 0, i)
end

@propagate_inbounds function is_valid(str::MaybeSub{T}, i::Integer) where {T<:Str}
    @_inline_meta()
    @boundscheck 1 <= i <= ncodeunits(str) || return false
    _isvalid_char_pos(CodePointStyle(T), cse(T), str, i)
end

_isvalid_char_pos(::SingleCU, C, str, i) = true

@propagate_inbounds function _collectstr(::MultiCU, ::Type{S},
                                         str::MaybeSub{T}) where {S,T<:Str}
    len = _length(MultiCU(), str)
    vec = create_vector(S, len)
    pos = 1
    @inbounds for i = 1:len
        vec[i], pos = _next(MultiCU(), S, str, pos)
    end
    vec
end

@propagate_inbounds function _collectstr(::SingleCU, ::Type{S},
                                         str::MaybeSub{T}) where {S,T<:Str}
    len = ncodeunits(str)
    vec = create_vector(S, len)
    cpt = eltype(T)
    @preserve str begin
        pnt = pointer(str)
        if S == cpt
            unsafe_copyto!(reinterpret(Ptr{basetype(cpt)}, pointer(vec)), pnt, len)
        else
            @inbounds for i = 1:len
                vec[i] = T(get_codeunit(pnt, i))
            end
        end
    end
    vec
end

function map(fun, str::MaybeSub{T}) where {C<:CSE, T<:Str{C}}
    out = get_iobuffer(sizeof(str))
    CP = eltype(T)
    for ch in str
        retc = fun(ch)
        isa(retc, AbstractChar) || throw(ArgumentError(
            "map($fun, str::AbstractString) requires $fun to return AbstractChar; " *
            "try map($fun, collect(str)) or a comprehension instead"))
        is_valid(CP, retc) || codepoint_error(CP, retc)
        _write(C, out, retc)
    end
    Str{C}(String(take!(out)))
end

@propagate_inbounds collect(str::MaybeSub{T}) where {T<:Str} =
    @preserve str _collectstr(CodePointStyle(T), eltype(T), str)

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

# Todo: These should be made more generic, work for all SingleCU types

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

const _Bytes = Union{UInt8,Int8}

# don't make unnecessary copies when passing substrings to C functions
cconvert(::Type{Ptr{T}}, str::SubString{<:Str{<:Byte_CSEs}}) where {T<:_Bytes} = str
cconvert(::Type{Ptr{UInt16}}, str::SubString{<:Str{<:Word_CSEs}}) = str
cconvert(::Type{Ptr{UInt32}}, str::SubString{<:Str{<:Quad_CSEs}}) = str

unsafe_convert(::Type{Ptr{Int8}},   s::MaybeSub{<:Str{<:Byte_CSEs}}) =
    reinterpret(Ptr{Int8}, pointer(s))
unsafe_convert(::Type{Ptr{UInt8}},  s::MaybeSub{<:Str{<:Byte_CSEs}}) = pointer(s)
unsafe_convert(::Type{Ptr{UInt16}}, s::MaybeSub{<:Str{<:Word_CSEs}}) = pointer(s)
unsafe_convert(::Type{Ptr{UInt32}}, s::MaybeSub{<:Str{<:Quad_CSEs}}) = pointer(s)

unsafe_convert(::Type{Ptr{Text1Chr}}, str::MaybeSub{<:Str{<:Byte_CSEs}}) =
    reinterpret(Ptr{T}, pointer(str))
unsafe_convert(::Type{Ptr{Text2Chr}}, str::MaybeSub{<:Str{<:Word_CSEs}}) =
    reinterpret(Ptr{T}, pointer(str))
unsafe_convert(::Type{Ptr{Text4Chr}}, str::MaybeSub{<:Str{<:Quad_CSEs}}) =
    reinterpret(Ptr{T}, pointer(str))

unsafe_convert(::Type{Ptr{Cvoid}},  s::MaybeSub{<:Str{C}}) where {C} =
    reinterpret(Ptr{Cvoid}, unsafe_convert(Ptr{codeunit(C)}, s))

function _reverse(::SingleCU, ::Type{C}, len, str::Str{C}) where {C<:CSE}
    len < 2 && return str
    @preserve str begin
        pnt = pointer(str)
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

function _reverse(::MultiCU, ::Type{C}, len, str) where {C<:CSE}
    @inbounds ((t = nextind(str, 0)) > len || nextind(str, t) > len) && return str
    @preserve str _reverse(MultiCU(), C, len, pointer(str))
end

reverse(str::MaybeSub{T}) where {C<:CSE,T<:Str{C}} =
    _reverse(CodePointStyle(T), C, ncodeunits(str), str)
