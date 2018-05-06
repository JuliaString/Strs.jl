#=
Search functions for Str strings

Copyright 2018 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on julia/base/strings/search.jl
=#

"""
    find(Fwd, pattern, string::AbstractString, start::Integer)

Find the next occurrence of `pattern` in `string` starting at position `start`.
`pattern` can be either a string, character, function, or regular expression
(in which case `string` must be of type `String`, or a `Str` type whose codeunit size is == 1,
or a `SubString` of one of those types)

The return value is a range of indexes where the matching sequence is found, such that
`s[find(Fwd, x, s, i)] == x`:

`find(Fwd, "substring", string, i)` = `start:end` such that
`string[start:end] == "substring"`, or `0:-1` if unmatched.

# Examples
```jldoctest
julia> find(Fwd, "z", "Hello to the world", 1)
0:-1

julia> find(Fwd, "o", "Hello to the world", 6)
8:8

julia> find(Fwd, "Julia", "JuliaLang", 2)
1:5

julia> find(Fwd, "z", "Hello to the world")
0:-1

julia> find(Fwd, "Julia", "JuliaLang")
1:5
```
"""
find(::Type{Fwd}, pat, str, pos)

"""
    find(First, pattern, string::AbstractString)

Find the first occurrence of `pattern` in `string` starting at position `start`.
`pattern` can be either a string, character, function, or regular expression
(in which case `string` must be of type `String`, or a `Str` type whose codeunit size is == 1,
or a `SubString` of one of those types)

The return value is a range of indexes where the matching sequence is found, such that
`s[find(Fwd, x, s, i)] == x`

`find(Fwd, "substring", string, i)` = `start:end` such that
`string[start:end] == "substring"`, or `0:-1` if unmatched.

# Examples
```jldoctest
julia> find(First, "z", "Hello to the world")
0:-1

julia> find(First, "z", "Hello to the world")
0:-1

julia> find(First, "Julia", "JuliaLang")
1:5
```
"""
find(::Type{First}, pat, str)

"""
    find(Rev, pattern::AbstractString, string::AbstractString, start::Integer)

Find the previous occurrence of `pattern` in `string` starting at position `start`.

The return value is a range of indexes where the matching sequence is found, such that
`s[find(Rev, x, s, i)] == x`

`find(Rev, "substring", string, i)` = `start:end` such that
`string[start:end] == "substring"`, or `0:-1` if unmatched.

# Examples
```jldoctest
julia> find(Rev, "z", "Hello to the world", 18)
0:-1

julia> find(Rev, "o", "Hello to the world", 18)
15:15

julia> find(Rev, "Julia", "JuliaLang", 6)
1:5
```
"""
find(::Type{Rev}, pat, str, pos)

"""
    find(Last, pattern::AbstractString, string::AbstractString)

Find the previous occurrence of `pattern` in `string`

The return value is a range of indexes where the matching sequence is found, such that
`s[find(First, x, s)] == x`

`find(First, "substring", string, i)` = `start:end` such that
`string[start:end] == "substring"`, or `0:-1` if unmatched.

# Examples
```jldoctest
julia> find(Last, "o", "Hello to the world")
15:15

julia> find(Last, "Julia", "JuliaLang")
1:5
```
"""
find(::Type{Last}, pat, str)

const _not_found = 0:-1

found(::Type{<:AbstractString}, v) = v != 0
find_result(::Type{<:AbstractString}, v) = v

@static if !V6_COMPAT

nothing_sentinel(i) = first(i) == 0 ? nothing : i
Base.findfirst(a, b::Str)   = nothing_sentinel(find(First, a, b))
Base.findlast(a, b::Str)    = nothing_sentinel(find(Last, a, b))
Base.findnext(a, b::Str, i) = nothing_sentinel(find(Fwd, a, b, i))
Base.findprev(a, b::Str, i) = nothing_sentinel(find(Rev, a, b, i))
Base.findfirst(a::Str, b::AbstractString)   = nothing_sentinel(find(First, a, b))
Base.findlast(a::Str, b::AbstractString)    = nothing_sentinel(find(Last, a, b))
Base.findnext(a::Str, b::AbstractString, i) = nothing_sentinel(find(Fwd, a, b, i))
Base.findprev(a::Str, b::AbstractString, i) = nothing_sentinel(find(Rev, a, b, i))

end

function find(::Type{D}, fun::Function, str::AbstractString, pos::Integer) where {D<:Direction}
    pos < Int(D===Fwd) && (@boundscheck boundserr(str, pos); return 0)
    if pos > (len = ncodeunits(str))
        @boundscheck pos > len+1 && boundserr(str, pos)
        return 0
    end
    @inbounds is_valid(str, pos) || index_error(str, pos)
    _srch_pred(D(), fun, str, pos)
end

find(::Type{First}, fun::Function, str::AbstractString) =
    _srch_pred(Fwd(), fun, str, 1)
find(::Type{Last},  fun::Function, str::AbstractString) =
    _srch_pred(Rev(), fun, str, lastindex(str))

find(::Type{Next}, pat, str::AbstractString, pos) = find(Fwd, pat, str, pos)
find(::Type{Prev}, pat, str::AbstractString, pos) = find(Rev, pat, str, pos)

# Invert order of function and findop, to allow use of do blocks
find(pat::Function, ::Type{D}, str::AbstractString) where {D<:FindOp} = find(D, pat, str)
find(pat::Function, ::Type{D}, str::AbstractString, pos) where {D<:FindOp} = find(D, pat, str, pos)

find(::Type{D}, pred::P, str::AbstractString,
    pos::Integer) where {P<:Fix2{Union{typeof(==),typeof(isequal)}, <:AbsChar}, D<:Direction} =
    find(D, pred.x, str, pos)

function find(::Type{D}, ch::AbsChar, str::AbstractString, pos::Integer) where {D<:Direction}
    pos < Int(D===Fwd) && (@boundscheck boundserr(str, pos); return 0)
    if pos > (len = ncodeunits(str))
        @boundscheck pos > len+1 && boundserr(str, pos)
        return 0
    end
    # Only check if MultiCU
    is_multi(str) && (@inbounds is_valid(str, pos) || index_error(str, pos))
    # Check here if ch is valid for the type of string
    is_valid(eltype(str), ch) ? _srch_cp(D(), EncodingStyle(str), str, ch, pos, len) : 0
end

_get_dir(::Type{First}) = Fwd()
_get_dir(::Type{Last})  = Rev()

find(::Type{D}, ch::AbsChar, str::AbstractString) where {D<:Union{First,Last}} =
    ((len = ncodeunits(str)) == 0 || !is_valid(eltype(str), ch) ? 0
     : _srch_cp(_get_dir(D), EncodingStyle(str), str, ch,
                D === First ? 1 : lastindex(str), len))

function find(::Type{D}, needle::AbstractString, str::AbstractString,
              pos::Integer) where {D<:Direction}
    # Check for fast case of a single codeunit (should check for single character also)
    if (slen = ncodeunits(str)) == 0
        # Special case for empty string
        @boundscheck pos == 1 || pos == 0 || boundserr(str, pos)
        return ifelse(isempty(needle), 1:0, _not_found)
    end
    pos < 1 && (@boundscheck boundserr(str, pos) ; return _not_found)
    pos > slen && (@boundscheck pos > slen + 1 && boundserr(str, pos) ; return _not_found)
    @inbounds is_valid(str, pos) || index_error(str, pos)
    (tlen = ncodeunits(needle)) == 0 && return pos:pos-1
    (cmp = CanContain(str, needle)) === NoCompare() && return _not_found
    @inbounds ch, nxt = next(needle, 1)
    is_valid(eltype(str), ch) || return _not_found
    # Check if single character
    if nxt > tlen
        pos = _srch_cp(D(), EncodingStyle(str), str, ch, pos, slen)
        return pos == 0 ? _not_found : (pos:pos)
    end
    _srch_strings(D(), cmp, str, needle, ch, nxt, pos, slen, tlen)
end

function find(::Type{T}, needle::AbstractString, str::AbstractString) where {T<:Union{First,Last}}
    # Check for fast case of a single codeunit (should check for single character also)
    (slen = ncodeunits(str)) == 0 && return ifelse(isempty(needle), 1:0, _not_found)
    pos = T === First ? 1 : thisind(str, slen)
    (tlen = ncodeunits(needle)) == 0 && return pos:(pos-1)
    (cmp = CanContain(str, needle)) === NoCompare() && return _not_found
    @inbounds ch, nxt = next(needle, 1)
    is_valid(eltype(str), ch) || return _not_found
    # Check if single character
    if nxt > tlen
        pos = _srch_cp(_get_dir(T), EncodingStyle(str), str, ch, pos, slen)
        return pos:(pos - (pos == 0))
    end
    _srch_strings(_get_dir(T), cmp, str, needle, ch, nxt, pos, slen, tlen)
end

## Lower level search functions

@inline function _srch_pred(::Fwd, testf, str, pos)
    len = ncodeunits(str)
    while pos <= len
        @inbounds ch = str[pos]
        testf(ch) && return pos
        pos = nextind(str, pos)
    end
    0
end

@inline function _srch_pred(::Rev, testf, str, pos)
    while pos > 0
        @inbounds ch = str[pos]
        testf(ch) && break
        pos = prevind(str, pos)
    end
    pos
end

# AbstractString implementations of the generic find interfaces

@inline function _srch_codeunit(::Fwd, beg::Ptr{T}, cu::T, pos, len) where {T<:CodeUnitTypes}
    if sizeof(Cwchar_t) == sizeof(T) || T == UInt8
        pnt = _fwd_memchr(bytoff(beg, pos - 1), cu, len - pos + 1)
        pnt == C_NULL ? 0 : chrdiff(pnt, beg) + 1
    else
        beg -= sizeof(T)
        pnt = bytoff(beg, pos)
        fin = bytoff(beg, len)
        while pnt <= fin
            get_codeunit(pnt) == cu && return chrdiff(pnt, beg)
            pnt += sizeof(T)
        end
        0
    end
end

@inline function _srch_codeunit(::Rev, beg::Ptr{UInt8}, cu::UInt8, pos)
    pnt = _rev_memchr(beg, cu, pos)
    #println("beg=$beg, cu=$cu, pos=$pos => $pnt")
    pnt == C_NULL ? 0 : chrdiff(pnt, beg) + 1
end

@inline function _srch_codeunit(::Rev, beg::Ptr{T}, cu::T, pos) where {T<:Union{UInt16,UInt32}}
    pnt = bytoff(beg, pos)
    while (pnt -= sizeof(T)) >= beg && get_codeunit(pnt) != cu ; end
    #println("beg=$beg, cu=$cu, T=$T, pos=$pos => $pnt")
    chrdiff(pnt, beg) + 1
end

# _srch_cp is only called with values that are valid for that string type,
# and checking as already been done on the position (pos)
# These definitions only work for SingleCU types
_srch_cp(::Fwd, ::SingleCU, str::T, cp::AbsChar, pos, len) where {T<:Str} =
    @preserve str _srch_codeunit(Fwd(), pointer(str), cp%codeunit(T), pos, len)
_srch_cp(::Rev, ::SingleCU, str::T, cp::AbsChar, pos, len) where {T<:Str} =
    @preserve str _srch_codeunit(Rev(), pointer(str), cp%codeunit(T), pos)

function _srch_cp(::Fwd, cus, str, cp, pos, len)
    @inbounds while pos <= len
        str[pos] == cp && return pos
        pos = nextind(str, pos)
    end
    0
end

function _srch_cp(::Rev, cus, str, cp, pos, len)
    @inbounds while pos > 0
        str[pos] == cp && return pos
        pos = prevind(str, pos)
    end
    0
end

# Substring searching

# Todo: make use of CompareStyle trait to improve performance

"""Compare two strings, starting at nxtstr and nxtsub"""
@inline function _cmp_str(str, strpos, endpos, sub, subpos, endsub)
    while strpos <= endpos
        c, strnxt = next(str, strpos)
        d, subpos = next(sub, subpos)
        c == d || break
        subpos > endsub && return strpos
        strpos = strnxt
    end
    0
end

function _srch_strings(::Fwd, ::CompareStyle, str, needle, ch, subpos, pos, slen, tlen)
    cu = EncodingStyle(str)
    while (pos = _srch_cp(Fwd(), cu, str, ch, pos, slen)) != 0
        nxt = nextind(str, pos)
        res = _cmp_str(str, nxt, slen, needle, subpos, tlen)
        res == 0 || return pos:res
        pos = nxt
        pos > slen && break
    end
    _not_found
end

function _srch_strings(::Rev, ::CompareStyle, str, needle, ch, nxtsub, pos, slen, tlen)
    cu = EncodingStyle(str)
    while (prv = prevind(str, pos)) != 0 &&
        (loc = _srch_cp(Rev(), cu, str, ch, prv, slen)) != 0
        res = _cmp_str(str, nextind(str, loc), slen, needle, nxtsub, tlen)
        res == 0 || res > pos || return loc:res
        pos = loc
    end
    _not_found
end

_search_bloom_mask(ch) = UInt64(1) << (ch & 63)
_check_bloom_mask(mask, pnt, off) = (mask & _search_bloom_mask(get_codeunit(pnt, off))) == 0

function _srch_str_bloom(::Fwd, str, spnt, npnt, ch, pos, slen, tlen)
    tlast = get_codeunit(npnt, tlen)
    bloom_mask = _search_bloom_mask(tlast)
    skip = tlen - 1
    for j in 1:tlen-1
        cu = get_codeunit(npnt, j)
        bloom_mask |= _search_bloom_mask(cu)
        cu == tlast && (skip = tlen - j - 1)
    end

    pos -= 1
    while pos <= slen
        if get_codeunit(spnt, pos + tlen) == tlast
            pos += 1
            # check candidate
            j = 0
            while j < tlen - 1 && get_codeunit(spnt, pos + j) == get_codeunit(npnt, j + 1)
                j += 1
            end

            # match found
            j == tlen - 1 && return pos:_thisind(EncodingStyle(str), str, slen, spnt, pos + j)

            # no match, try to rule out the next character
            if pos <= slen && _check_bloom_mask(bloom_mask, spnt, pos + tlen)
                pos += tlen
            else
                pos += skip
            end
        elseif (pos += 1) <= slen && _check_bloom_mask(bloom_mask, spnt, pos + tlen)
            pos += tlen
        end
    end

    _not_found
end

function _srch_str_bloom(::Rev, str, spnt, npnt, ch, pos, slen, tlen)
    (slen < tlen || pos <= 0) && return 0

    tfirst = is_multi(str) ? get_codeunit(npnt) : codepoint(ch)
    bloom_mask = UInt64(0)
    skip = tlen - 1
    for j in skip:-1:0
        cu = get_codeunit(npnt, j)
        bloom_mask |= _search_bloom_mask(cu)
        cu == tfirst && j > 0 && (skip = j - 1)
    end

    pos = min(pos, slen) - tlen
    while pos >= 0
        if get_codeunit(spnt, pos) == tfirst
            # check candidate
            j = 1
            while j < tlen && get_codeunit(spnt, pos + j) == get_codeunit(npnt, j)
                j += 1
            end

            # match found
            j == tlen && return i

            # no match, try to rule out the next character
            pos -= ((pos > 0 && bloom_mask & _search_bloom_mask(get_codeunit(spnt, pos)) == 0)
                  ? tlen : skip)
        elseif pos > 0
            (bloom_mask & _search_bloom_mask(get_codeunit(spnt, pos)) == 0) && (pos -= tlen)
        end
        pos -= 1
    end

    0
end

# This should work for compatible CSEs, like ASCII & Latin, ASCII & UTF8, etc.
# See equals trait
_srch_strings(::Type{D}, ::Union{ByteCompare,WidenCompare}, str, needle,
              ch, nxtsub, pos, slen, tlen) where {D<:Direction} =
                  @preserve str needle _srch_str_bloom(D(), str, pointer(str), pointer(needle),
                                                       ch, pos, slen, tlen)

_occurs_in(needle, hay) = first(find(First, needle, hay)) != 0

# Avoid type piracy
occurs_in(needle::AbstractString, hay::Str) = _occurs_in(needle, hay)
occurs_in(needle::Str, hay::AbstractString) = _occurs_in(needle, hay)
occurs_in(needle::Chr, hay::AbstractString) = _occurs_in(needle, hay)
occurs_in(needle::Char, hay::Str)           = _occurs_in(needle, hay)
occurs_in(needle::Str, hay::Str)            = _occurs_in(needle, hay)

in(chr::Chr, str::AbstractString) = _occurs_in(chr, str)
in(chr::AbsChar,   str::Str)      = _occurs_in(chr, str)
in(pat::Str, str::AbstractString) = _occurs_in(pat, str)
in(pat::AbstractString, str::Str) = _occurs_in(pat, str)
in(pat::Str, str::Str)            = _occurs_in(pat, str)
