# This file includes code originally from Julia.
# License is MIT: https://julialang.org/license

export find_next, find_first, find_prev, find_last, found, find_result

"""
    find_next(pattern::AbstractString, string::AbstractString, start::Integer)
    find_next(pattern::Regex, string::String, start::Integer)

Find the next occurrence of `pattern` in `string` starting at position `start`.
`pattern` can be either a string, or a regular expression, in which case `string`
must be of type `String`.

The return value is a range of indexes where the matching sequence is found, such that
`s[find_next(x, s, i)] == x`:

`find_next("substring", string, i)` = `start:end` such that
`string[start:end] == "substring"`, or `0:-1` if unmatched.

# Examples
```jldoctest
julia> find_next("z", "Hello to the world", 1)
0:-1

julia> find_next("o", "Hello to the world", 6)
8:8

julia> find_next("Julia", "JuliaLang", 2)
1:5
```
"""
function find_next end

"""
    find_first(pattern::AbstractString, string::AbstractString)
    find_first(pattern::Regex, string::AbstractString)

Find the first occurrence of `pattern` in `string`. Equivalent to
[`find_next(pattern, string, 1)`](@ref).

# Examples
```jldoctest
julia> find_first("z", "Hello to the world")
0:-1

julia> find_first("Julia", "JuliaLang")
1:5
```
"""
function find_first end

"""
    find_prev(pattern::AbstractString, string::AbstractString, start::Integer)
    find_prev(pattern::Regex, string::String, start::Integer)

Find the previous occurrence of `pattern` in `string` starting at position `start`.
`pattern` can be either a string, or a regular expression, in which case `string`
must be of type `String`.

The return value is a range of indexes where the matching sequence is found, such that
`s[find_prev(x, s, i)] == x`:

`find_prev("substring", string, i)` = `start:end` such that
`string[start:end] == "substring"`, or `0:-1` if unmatched.

# Examples
```jldoctest
julia> find_prev("z", "Hello to the world", 18)
0:-1

julia> find_prev("o", "Hello to the world", 18)
15:15

julia> find_prev("Julia", "JuliaLang", 6)
1:5
```
"""
function find_prev  end

"""
    find_last(pattern::AbstractString, string::AbstractString)
    find_last(pattern::Regex, string::String)

Find the last occurrence of `pattern` in `string`. Equivalent to
[`find_last(pattern, string, lastindex(s))`](@ref).

# Examples
```jldoctest
julia> find_last("o", "Hello to the world")
15:15

julia> find_first("Julia", "JuliaLang")
1:5
```
"""
function find_last  end

find_next(pat::Regex, str, pos::Integer) = findnext(pat, str, pos)
find_prev(pat::Regex, str, pos::Integer) = findprev(pat, str, pos)
find_first(pat::Regex, str) = findfirst(pat, str)
find_last(pat::Regex, str)  = findlast(pat, str)

found(::Type{<:AbstractString}, v) = v != 0
find_result(::Type{<:AbstractString}, v) = v

@static if VERSION < v"0.7.0-DEV"
const base_fwd_srch_chr = Base.searchindex
const base_fwd_srch_str = Base.searchindex
const base_rev_srch_chr = Base.rsearchindex
const base_rev_srch_str = Base.rsearchindex

export EqualTo, equalto, OccursIn, occursin

struct EqualTo{T} <: Function
    x::T

    EqualTo(x::T) where {T} = new{T}(x)
end

(f::EqualTo)(y) = isequal(f.x, y)

"""
    equalto(x)

Create a function that compares its argument to `x` using [`isequal`](@ref); i.e. returns
`y->isequal(x,y)`.

The returned function is of type `Base.EqualTo`. This allows dispatching to
specialized methods by using e.g. `f::Base.EqualTo` in a method signature.
"""
const equalto = EqualTo

struct OccursIn{T} <: Function
    x::T

    OccursIn(x::T) where {T} = new{T}(x)
end

(f::OccursIn)(y) = y in f.x

"""
    occursin(x)

Create a function that checks whether its argument is [`in`](@ref) `x`; i.e. returns
`y -> y in x`.

The returned function is of type `Base.OccursIn`. This allows dispatching to
specialized methods by using e.g. `f::Base.OccursIn` in a method signature.
"""
const occursin = OccursIn
else

const base_fwd_srch_chr = Base._searchindex
const base_fwd_srch_str = Base._searchindex
const base_rev_srch_chr = Base._rsearchindex
const base_rev_srch_str = Base._rsearchindex

    import Base: EqualTo, equalto, OccursIn, occursin

#=
    nothing_sentinel(i) = i == 0 ? nothing : i
    Base.findnext(a, b, i) = nothing_sentinel(find_next(a, b, i))
    Base.findfirst(a, b)   = nothing_sentinel(find_first(a, b))
    Base.findprev(a, b, i) = nothing_sentinel(find_prev(a, b, i))
    Base.findlast(a, b)    = nothing_sentinel(find_last(a, b))
=#
end

const AbsChar = Union{Char, CodePoint} # replace with AbstractChar when in base
const ByteStrings     = Union{String, ByteStr}

find_next(pred::EqualTo{<:AbsChar}, str::AbstractString, pos::Integer) =
    _fwd_srch_chr(str, pred.x, pos)
find_prev(pred::EqualTo{<:AbsChar}, str::AbstractString, pos::Integer) =
    _rev_srch_chr(str, pred.x, pos)

find_next(ch::AbsChar, str::AbstractString, pos::Integer) = _fwd_srch_chr(str, ch, pos)
find_prev(ch::AbsChar, str::AbstractString, pos::Integer) = _rev_srch_chr(str, ch, pos)

find_first(a, b) = find_next(a, b, 1)
find_last(a,  b) = find_prev(a, b, lastindex(b))

const index_error = Base.string_index_err

# AbstractString implementation of the generic find_next interface
function find_next(testf::Function, str::AbstractString, pos::Integer)
    if pos < 1
        @boundscheck pos < 0 && boundserr(str, pos)
        return 0
    end
    len = ncodeunits(str)
    if pos > len
        @boundscheck pos > len + 1 && boundserr(str, pos)
        return 0
    end
    isvalid(str, pos) || index_error(str, pos)
    for (j, d) in pairs(SubString(str, pos))
        testf(d) && return pos + j - 1
    end
    0
end

# AbstractString implementation of the generic find_prev interface
function find_prev(testf::Function, str::AbstractString, pos::Integer)
    if pos < 1
        @boundscheck pos < 0 && boundserr(str, pos)
        return 0
    end
    len = ncodeunits(str)
    if pos > len
        @boundscheck pos > len + 1 && boundserr(str, pos)
        return 0
    end
    isvalid(str, pos) || index_error(str, pos)
    while pos > 0
        @inbounds ch = str[pos]
        testf(ch) && break
        pos = prevind(str, pos)
    end
    pos
end

_fwd_srch_codeunit(ptr::Ptr{UInt8}, cu::UInt8, fin::Ptr{UInt8}) =
    _fwd_srch_codeunit(ptr, cu, fin-pnt)
_rev_srch_codeunit(ptr::Ptr{UInt8}, cu::UInt8, fin::Ptr{UInt8}) =
    _rev_srch_codeunit(ptr, cu, fin-pnt)

@inline function _fwd_srch_codeunit(pnt::Ptr{UInt8}, cu::UInt8, pos, len)
    res = _fwd_memchr(pnt + pos - 1, cu, len - pos + 1)
    res == C_NULL ? 0 : Int(res - pnt + 1)
end

@inline function _rev_srch_codeunit(pnt::Ptr{UInt8}, cu::UInt8, pos)
    res = _rev_memchr(pnt, cu, pos)
    res == C_NULL ? 0 : Int(res - pnt + 1)
end

_fwd_srch_codeunit(str::ByteStr, cu::UInt8, pos) =
    _fwd_srch_codeunit(_pnt(str), cu, pos, _len(str))
_rev_srch_codeunit(str::ByteStr, cu::UInt8, pos) =
    _rev_srch_codeunit(_pnt(str), cu, pos)

_fwd_srch_codeunit(str::QuadStr, cu::UInt32, pos) =
    _fwd_srch_codeunit(_pnt(str), cu%UInt32, pos, _len(str))
_rev_srch_codeunit(str::QuadStr, cu::UInt32, pos) =
    _rev_srch_codeunit(_pnt(str), cu%UInt32, pos)

function _fwd_srch_chr(str, ch, pos::Integer)
    if pos < 1
        @boundscheck boundserr(str, pos)
        return 0
    end
    len = ncodeunits(str)
    if pos > len
        @boundscheck pos > len + 1 && boundserr(str, pos)
        return 0
    end
    isvalid(str, pos) || index_error(str, pos)
    # Check here if ch is valid for the type of string
    cpt = codepoint_type(typeof(str))
    isvalid(cpt, ch) ? _fwd_srch_codeunit(str, tobase(ch%cpt), pos) : 0
end

function _rev_srch_chr(str, ch, pos::Integer)
    if pos < 1
        @boundscheck pos < 0 && boundserr(str, pos)
        return 0
    end
    len = ncodeunits(str)
    if pos > len
        @boundscheck pos > len + 1 && boundserr(str, pos)
        return 0
    end
    isvalid(str, pos) || index_error(str, pos)
    # Check first if ch is valid for the type of string
    cpt = codepoint_type(typeof(str))
    isvalid(cpt, ch) ? _rev_srch_codeunit(str, tobase(ch%cpt), pos) : 0
end

_fwd_srch_chr(str::AbstractString, ch::Char, pos::Integer) =
    base_fwd_srch_chr(str, ch, pos)
_rev_srch_chr(str::AbstractString, ch::Char, pos::Integer) =
    base_rev_srch_chr(str, ch, pos)
_fwd_srch_chr(str::String, ch::CodePoint, pos::Integer) =
    base_fwd_srch_chr(str, ch%Char, pos)
_rev_srch_chr(str::String, ch::CodePoint, pos::Integer) =
    base_rev_srch_chr(str, ch%Char, pos)
_fwd_srch_chr(str::Str, ch::Char, pos::Integer) =
    _fwd_srch_chr(str, ch%UInt32, pos)
_rev_srch_chr(str::Str, ch::Char, pos::Integer) =
    _rev_srch_chr(str, ch%UInt32, pos)

# Substring searching

# Note: these definitions, from Base, are incorrect, because the indexing of the needle
# doesn't necessarily match that of the string being searched

function find_next(needle::AbstractString, str::AbstractString, pos::Integer)
    idx = _fwd_srch_str(str, needle, pos)
    idx:(idx - 1 + ((!isempty(needle) && idx > 0) ? lastindex(needle) : 0))
end
function find_prev(needle::AbstractString, str::AbstractString, pos::Integer)
    idx = _rev_srch_str(str, needle, pos)
    idx:(idx - 1 + ((!isempty(needle) && idx > 0) ? lastindex(needle) : 0))
end

_fwd_srch_str(str::String, needle::String, pos::Integer) = base_fwd_srch_str(str, needle, pos)
_rev_srch_str(str::String, needle::String, pos::Integer) = base_rev_srch_str(str, needle, pos)

_fwd_srch_cp(::CodeUnitSingle, str, cp, pos) = _fwd_srch_codeunit(str, cp, pos)
_rev_srch_cp(::CodeUnitSingle, str, cp, pos) = _rev_srch_codeunit(str, cp, pos)

function _fwd_srch_cp(::CodeUnitMulti, str, cp, pos)
    len = _len(str)
    @inbounds while pos <= len
        tobase(str[pos]) == cp && return pos
        pos = nextind(str, pos)
    end
    0
end

function _rev_srch_cp(::CodeUnitMulti, str, cp, pos)
    @inbounds while pos > 0
        tobase(str[pos]) == cp && return pos
        pos = prevind(str, pos)
    end
    0
end

function _fwd_srch_str(str::AbstractString, needle::AbstractString, pos::Integer)
    isempty(needle) &&
        return 1 <= pos <= nextind(str, lastindex(str)) ? pos : boundserr(str, pos)
    ch, trest = Iterators.peel(needle)
    # Check first if ch is valid for the type of string
    str_cp_typ = codepoint_type(typeof(str))
    isvalid(str_cp_typ, ch) || return 0
    cbase = tobase(ch)%basetype(str_cp_typ)
    cstyle = CodePointStyle(str)
    # We want to search the abstract string for a *codepoint*
    while (pos = _fwd_srch_cp(cstyle, str, cbase, pos)) != 0
        ii = nextind(str, pos)
        a = Iterators.Stateful(trest)
        matched = all(Base.splat(==), zip(SubString(str, ii), a))
        isempty(a) && matched && break
        pos = ii
    end
    pos
end

_search_bloom_mask(ch) = UInt64(1) << (ch & 63)
_check_bloom_mask(mask, pnt, off) = (mask & _search_bloom_mask(get_codeunit(pnt, off))) == 0

const UTF8Strings = Union{Str{<:ASCIICSE}, Str{<:UTF8CSE}}

function _fwd_srch_str(str::UTF8Strings, needle::UTF8Strings, pos::Integer)
    # Check for fast case of a single codeunit (should check for single character also)
    tlen = ncodeunits(needle)
    tlen == 1 &&
        return _fwd_srch_chr(str, get_codeunit(needle), pos)
    slen = ncodeunits(str)
    tlen == 0 && return 1 <= pos <= slen+1 ? pos : 0
    slen == 0 && return 0
    diff = slen - tlen
    slen < tlen && return 0
    slen == tlen && pos == 1 && return (_memcmp(str, needle, slen) == 0 ? 1 : 0)
    (pos -= 1) <= diff || return 0

    spnt = _pnt(str)
    npnt = _pnt(needle)

    tlast = get_codeunit(npnt, tlen)

    bloom_mask = UInt64(0)
    skip = tlen - 1
    for j in 1:tlen
        bloom_mask |= _search_bloom_mask(get_codeunit(npnt, j))
        get_codeunit(npnt, j) == tlast && j < tlen && (skip = tlen - j - 1)
    end
    
    while pos <= diff
        if get_codeunit(spnt, pos + tlen) == tlast
            pos += 1

            # check candidate
            j = 0
            while j < tlen - 1 && get_codeunit(spnt, pos + j) == get_codeunit(npnt, j + 1)
                j += 1
            end

            # match found
            j == tlen - 1 && return pos

            # no match, try to rule out the next character
            if pos <= diff && _check_bloom_mask(bloom_mask, spnt, pos + tlen)
                pos += tlen
            else
                pos += skip
            end
        elseif (pos += 1) <= diff && _check_bloom_mask(bloom_mask, spnt, pos + tlen)
            pos += tlen
        end
    end

    0
end

function _rev_srch_str(str::AbstractString, needle::AbstractString, pos::Integer)
    isempty(needle) &&
        return 1 <= pos <= nextind(str, lastindex(str)) ? pos : boundserr(str, pos)
    ch, trest = Iterators.peel(Iterators.reverse(needle))
    # Check first if ch is valid for the type of string
    str_cp_typ = codepoint_type(typeof(str))
    isvalid(str_cp_typ, ch) || return 0
    cbase = tobase(ch)%basetype(str_cp_typ)
    cstyle = CodePointStyle(str)
    while (pos = _rev_srch_cp(cstyle, str, cbase, pos)) != 0
        ii = prevind(str, pos)
        a = Iterators.Stateful(trest)
        b = Iterators.Stateful(Iterators.reverse(pairs(SubString(str, 1, ii))))
        all(Base.splat(==), zip(a, (x[2] for x in b))) && isempty(a) &&
            return isempty(b) ? 1 : nextind(str, popfirst!(b)[1])
        pos = ii
    end
    pos
end

function _rev_srch_str(str::UTF8Strings, needle::UTF8Strings, pos::Integer)
    # Check for fast case of a single codeunit
    tlen = ncodeunits(needle)
    tlen == 1 && return _rev_srch_chr(str, get_codeunit(needle), pos)
    slen = ncodeunits(str)
    tlen == 0 && return pos > slen ? 0 : (pos == 0 ? 1 : pos)
    slen == 0 && return 0
    slen < tlen && return 0
    pos <= 0 && return 0
    slen == tlen && pos == 1 && return (_memcmp(str, needle, slen) == 0 ? 1 : 0)
    pos = min(pos, slen) - tlen + 1

    spnt = _pnt(str)
    npnt = _pnt(needle)

    bloom_mask = UInt64(0)
    skip = tlen - 1
    tfirst = get_codeunit(npnt)
    for j in tlen:-1:1
        bloom_mask |= _search_bloom_mask(get_codeunit(npnt, j))
        get_codeunit(npnt, j) == tfirst && j > 1 && (skip = j - 2)
    end

    while pos > 0
        if get_codeunit(spnt, pos) == tfirst
            # check candidate
            j = 0
            while (j += 1) < tlen && get_codeunit(spnt, pos + j) == get_codeunit(npnt, j + 1) ; end

            # match found
            j == tlen && return pos

            # no match, try to rule out the next character
            pos -= ((pos > 1 && _check_bloom_mask(bloom_mask, spnt, pos - 1)) ? tlen : skip) + 1
        elseif pos > 1 && _check_bloom_mask(bloom_mask, spnt, pos -= 1)
            pos -= 1
        else
            pos -= 1
        end
    end
    0
end

contains(haystack::Str, needle::AbsChar)        = _fwd_srch_chr(haystack, needle, 1) != 0
contains(haystack::Str, needle::AbstractString) = _fwd_srch_str(haystack, needle, 1) != 0
contains(haystack::AbstractString, needle::CodePoint) = _fwd_srch_chr(haystack, needle, 1) != 0
contains(haystack::AbstractString, needle::Str) = _fwd_srch_str(haystack, needle, 1) != 0

in(ch::AbsChar, str::AbstractString) = base_fwd_srch_chr(str, ch%Char, 1) != 0
in(ch::AbsChar, str::Str) = _fwd_srch_chr(str, ch, 1) != 0

in(a::Str, b::AbstractString) = contains(b, a)
in(a::AbstractString, b::Str) = contains(b, a)
in(a::Str, b::Str)            = contains(b, a)
