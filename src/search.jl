#=
Search functions for Str strings

Copyright 2018 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on julia/base/strings/search.jl
=#

abstract type FindOp end
abstract type Direction <: FindOp end
struct Fwd <: Direction end
struct Rev <: Direction end

"""
    find(Fwd, pattern::Union{Regex,AbstractString}, string::AbstractString, start::Integer=1)

Find the next occurrence of `pattern` in `string` starting at position `start`.
`pattern` can be either a string, or a regular expression, in which case `string`
must be of type `String`, `ASCIIStr`, `UTF8Str` (or a `SubString` of one of those types)
dir can be either Fwd or Rev

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
    find(Rev, pattern::AbstractString, string::AbstractString, start::Integer=lastindex(string))

Find the previous occurrence of `pattern` in `string` starting at position `start`.

The return value is a range of indexes where the matching sequence is found, such that
`s[find(Rev, x, s, i)] == x`:

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

julia> find(Rev, "o", "Hello to the world")
15:15

julia> find(Rev, "Julia", "JuliaLang")
1:5
```
"""
find(::Type{Rev}, pat, str, pos)

const _not_found = 0:-1

found(::Type{<:AbstractString}, v) = v != 0
find_result(::Type{<:AbstractString}, v) = v

@static if !V6_COMPAT

nothing_sentinel(i) = i == 0 ? nothing : i
Base.findnext(a, b::Str, i) = nothing_sentinel(find(Fwd, a, b, i))
Base.findfirst(a, b::Str)   = nothing_sentinel(find(Fwd, a, b))
Base.findprev(a, b::Str, i) = nothing_sentinel(find(Rev, a, b, i))
Base.findlast(a, b::Str)    = nothing_sentinel(find(Rev, a, b))
Base.findnext(a::Str, b::AbstractString, i) = nothing_sentinel(find(Fwd, a, b, i))
Base.findfirst(a::Str, b::AbstractString)   = nothing_sentinel(find(Fwd, a, b))
Base.findprev(a::Str, b::AbstractString, i) = nothing_sentinel(find(Rev, a, b, i))
Base.findlast(a::Str, b::AbstractString)    = nothing_sentinel(find(Rev, a, b))

end

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

function find(::Type{D}, fun::Function, str::AbstractString, pos::Integer) where {D<:Direction}
    if pos < 1 || pos > ncodeunits(str)
        @boundscheck boundserr(str, pos)
        return 0
    end
    @inbounds is_valid(str, pos) || index_error(str, pos)
    _srch_pred(D(), fun, str, pos)
end

const PatType = Union{Function, AbsChar, AbstractString, Regex}

find(::Type{Fwd}, pat::PatType, str::AbstractString) = find(Fwd, pat, str, 1)
find(::Type{Rev}, pat::PatType, str::AbstractString) = find(Rev, pat, str, lastindex(str))

find(pat::PatType, ::Type{D}, str::AbstractString) where {D<:Direction} = find(D, pat, str)

# AbstractString implementations of the generic find interfaces

# Defined with function first, for do syntax
find(fun::Function, ::Type{D}, str::AbstractString, pos::Integer) where {D<:Direction} =
    find(D(), fun, str, pos)

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
# These definitions only work for CodeUnitSingle types
_srch_cp(::Fwd, ::CodeUnitSingle, str::T, cp::AbsChar, pos, len) where {T<:Str} =
    @preserve str _srch_codeunit(Fwd(), _pnt(str), cp%codeunit(T), pos, len)
_srch_cp(::Rev, ::CodeUnitSingle, str::T, cp::AbsChar, pos, len) where {T<:Str} =
    @preserve str _srch_codeunit(Rev(), _pnt(str), cp%codeunit(T), pos)

function _srch_cp(::Fwd, cus, str, cp, pos, len)
    @inbounds while pos <= len
        str[pos] == cp && return pos
        pos = nextind(str, pos)
    end
    0
end

function _srch_cp(::Rev, cus, str, cp, pos, len)
    #print("_srch_cp(::Rev, $cus, \"$str\", '$(Char(cp))', $pos, $len)")
    @inbounds while pos > 0
        #print("\n\tpos=$pos, $(str[pos])")
        str[pos] == cp && return pos
        pos = prevind(str, pos)
    end
    #println(" => 0")
    0
end

find(::Type{D}, pred::P, str::AbstractString,
    pos::Integer) where {P<:Fix2{Union{typeof(==),typeof(isequal)}, <:AbsChar}, D<:Direction} =
    find(D, pred.x, str, pos)

function find(::Type{D}, ch::AbsChar, str::AbstractString, pos::Integer) where {D<:Direction}
    if pos < 1
        @boundscheck (pos == 0 && isempty(str)) || boundserr(str, pos)
        return 0
    end
    len = ncodeunits(str)
    if pos > len
        @boundscheck (len == 0 && pos == 1) || boundserr(str, pos)
        return 0
    end
    # Only check if CodeUnitMulti
    (cus = CodePointStyle(str)) === CodeUnitMulti() &&
        (@inbounds is_valid(str, pos) || index_error(str, pos))
    #println("find(::$D, '$ch', \"$str\", $pos) => $(is_valid(eltype(str), ch))")
    # Check here if ch is valid for the type of string
    is_valid(eltype(str), ch) ? _srch_cp(D(), cus, str, ch, pos, len) : 0
end

# Substring searching

# Todo: make use of CompareStyle trait to improve performance

"""Compare two strings, starting at nxtstr and nxtsub"""
@inline function _cmp_str(str, strpos, sub, subpos)
    while !done(str, strpos)
        c, strnxt = next(str, strpos)
        d, subpos = next(sub, subpos)
        c == d || break
        done(sub, subpos) && return strpos
        strpos = strnxt
    end
    0
end

function _srch_strings(::Fwd, ::CompareStyle, str, needle, ch, nxtsub, pos, slen, tlen)
    cu = CodePointStyle(str)
    @inbounds while (pos = _srch_cp(Fwd(), cu, str, ch, pos, slen)) != 0
        nxt = nextind(str, pos)
        res = _cmp_str(str, nxt, needle, nxtsub)
        res == 0 || return pos:res
        pos = nxt
        done(str, nxt) && break
    end
    _not_found
end

function _srch_strings(::Rev, ::CompareStyle, str, needle, ch, nxtsub, pos, slen, tlen)
    @inbounds begin
        prv = prevind(str, pos)
        prv == 0 && return _not_found
        cu = CodePointStyle(str)
        while (pos = _srch_cp(Rev(), cu, str, ch, prv, slen)) != 0
            res = _cmp_str(str, nextind(str, pos), needle, nxtsub)
            res == 0 || return pos:res
            (prv = prevind(str, pos)) == 0 && break
        end
    end
    _not_found
end

_search_bloom_mask(ch) = UInt64(1) << (ch & 63)
_check_bloom_mask(mask, pnt, off) = (mask & _search_bloom_mask(get_codeunit(pnt, off))) == 0

function _srch_str_bloom(str, spnt, npnt, ch, pos, slen, tlen)
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
            j == tlen - 1 && return pos:_thisind(CodePointStyle(str), str, slen, spnt, pos + j)

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

# This should work for compatible CSEs, like ASCII & Latin, ASCII & UTF8, etc.
# See equals trait
_srch_strings(::Fwd, ::Union{ByteCompare,WidenCompare}, str, needle, ch, nxtsub, pos, slen, tlen) =
    @preserve str needle _srch_str_bloom(str, _pnt(str), _pnt(needle), ch, pos, slen, tlen)

function find(::Type{D}, needle::AbstractString, str::AbstractString,
              pos::Integer) where {D<:Direction}
    # Check for fast case of a single codeunit (should check for single character also)
    slen = ncodeunits(str)
    if slen == 0
        # Special case for empty string
        @boundscheck pos == 1 || pos == 0 || boundserr(str, pos)
        return ifelse(isempty(needle), 1:0, _not_found)
    end
    if !(1 <= pos <= slen)
        @boundscheck boundserr(str, pos)
        return _not_found
    end
    @inbounds is_valid(str, pos) || index_error(str, pos)
    tlen = ncodeunits(needle)
    tlen == 0 && return pos:pos-1
    (cmp = CanContain(str, needle)) === NoCompare() && return _not_found
    @inbounds ch, nxt = next(needle, 1)
    is_valid(eltype(str), ch) || return _not_found
    # Check if single character
    if nxt > tlen
        pos = _srch_cp(D(), CodePointStyle(str), str, ch, pos, slen)
        return pos == 0 ? _not_found : (pos:pos)
    end
    _srch_strings(D(), cmp, str, needle, ch, nxt, pos, slen, tlen)
end

@static if V6_COMPAT
    contains(hay::AbstractString, chr::AbsChar)    = first(find(Fwd, chr, hay)) != 0
    contains(hay::AbstractString, pat::Regex)      = first(find(Fwd, pat, hay)) != 0
    contains(hay::AbstractString, pat::Regex, pos) = first(find(Fwd, pat, hay, pos)) != 0
else
    # Avoid type piracy
    contains(hay::Str, chr::Char)                  = first(find(Fwd, chr, hay)) != 0
    contains(hay::Str, pat::Regex)                 = first(find(Fwd, pat, hay)) != 0
    contains(hay::Str, pat::Regex, pos)            = first(find(Fwd, pat, hay, pos)) != 0
    contains(hay::AbstractString, chr::CodePoint)  = first(find(Fwd, chr, hay)) != 0
end
contains(hay::Str, str::Str)            = first(find(Fwd, str, hay)) != 0
contains(hay::Str, str::AbstractString) = first(find(Fwd, str, hay)) != 0
contains(hay::AbstractString, str::Str) = first(find(Fwd, str, hay)) != 0

in(chr::CodePoint, str::AbstractString) = contains(str, chr)
in(chr::AbsChar,   str::Str)            = contains(str, chr)
in(pat::Str, str::Str)                  = contains(str, pat)
in(pat::Str, str::AbstractString)       = contains(str, pat)
in(pat::AbstractString, str::Str)       = contains(str, pat)
