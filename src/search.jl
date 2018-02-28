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
    find_first(pattern::Regex, string::String)

Find the first occurrence of `pattern` in `string`. Equivalent to
[`find_next(pattern, string, firstindex(s))`](@ref).

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
const base_fwd_search = Base.searchindex
const base_rev_search = Base.rsearchindex

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

const base_fwd_search = Base._searchindex
const base_rev_search = Base._rsearchindex

    import Base: EqualTo, equalto, OccursIn, occursin

#=
    nothing_sentinel(i) = i == 0 ? nothing : i
    Base.findnext(a, b, i) = nothing_sentinel(find_next(a, b, i))
    Base.findfirst(a, b)   = nothing_sentinel(find_first(a, b))
    Base.findprev(a, b, i) = nothing_sentinel(find_prev(a, b, i))
    Base.findlast(a, b)    = nothing_sentinel(find_last(a, b))
=#
end

const Bytes = Union{UInt8, Int8}
const ByteVec = Vector{<:Bytes}
const AbsChars = Union{Char, AbstractChar}
const StrOrByteVec = Union{ByteStr, ByteVec}

_fwd_search(str::String, t::String, pos::Integer) = base_fwd_search(str, t, pos)
_rev_search(str::String, t::String, pos::Integer) = base_rev_search(str, t, pos)

find_next(pred::EqualTo{<:AbstractChar}, str::AbstractString, pos::Integer) =
    _fwd_search(str, pred.x, pos)
find_prev(pred::EqualTo{<:AbstractChar}, str::AbstractString, pos::Integer) =
    _rev_search(str, pred.x, pos)

# Returns index
function find_next(pred::EqualTo{<:AbsChars}, str::Str, pos::Integer)
    if pos < 1
        @boundscheck pos < 0 && boundserr(str, pos)
        return 0
    elseif pos > (siz = sizeof(str))
        @boundscheck pos > siz + 1 && boundserr(str, pos)
        return 0
    end
    @inbounds isvalid(str, pos) || unierror(UTF_ERR_INVALID_INDEX, str, pos)
    ch = pred.x
    isascii(ch) && return _fwd_search(str, ch % UInt8, pos)
    while (pos = _fwd_search(str, first_utf8_byte(ch), pos)) != 0 && str[pos] != ch
        pos = nextind(str, pos)
    end
    pos
end

# Returns index
function find_prev(pred::EqualTo{<:AbsChars}, str::Str, pos::Integer)
    ch = pred.x
    isascii(ch) && return _rev_search(str, ch % UInt8, pos)
    byt = first_utf8_byte(ch)
    while (pos = _rev_search(str, byt, pos)) != 0 && str[pos] != ch
        pos = prevind(str, pos)
    end
    pos
end

find_first(a, b) = find_next(a, b, 1)
find_last(a,  b) = find_prev(a, b, lastindex(b))

# Returns index
find_first(pred::EqualTo{<:Bytes}, a::StrOrByteVec)            = _fwd_search(a, pred.x)
find_next(pred::EqualTo{<:Bytes}, a::StrOrByteVec, i::Integer) = _fwd_search(a, pred.x, i)
find_last(pred::EqualTo{<:Bytes}, a::StrOrByteVec)             = _rev_search(a, pred.x)
find_prev(pred::EqualTo{<:Bytes}, a::StrOrByteVec, i::Integer) = _rev_search(a, pred.x, i)

# AbstractString implementation of the generic find_next interface
function find_next(testf::Function, str::AbstractString, pos::Integer)
    if pos < 1
        @boundscheck pos < 0 && boundserr(str, pos)
        return 0
    end
    n = ncodeunits(str)
    if pos > n
        @boundscheck pos > n + 1 && boundserr(str, pos)
        return 0
    end
    isvalid(str, pos) || unierror(UTF_ERR_INVALID_INDEX, str, pos)
    for (j, d) in pairs(SubString(str, pos))
        testf(d) && return pos + j - 1
    end
    0
end

function find_next(t::AbstractString, str::AbstractString, pos::Integer)
    idx = _fwd_search(str, t, pos)
    idx:(idx - 1 + ((!isempty(t) && idx > 0) ? lastindex(t) : 0))
end
function find_prev(t::AbstractString, str::AbstractString, pos::Integer)
    idx = _rev_search(str, t, pos)
    idx:(idx - 1 + ((!isempty(t) && idx > 0) ? lastindex(t) : 0))
end

_fwd_search(str::UnicodeByteStrings, ch::CodeUnitTypes, pos::Integer) =
    ch <= typemax(codepoint_type(str)) ? _fwd_search(_data(str), ch%UInt8, pos) : 0

_rev_search(str::UnicodeByteStrings, ch::CodeUnitTypes, pos::Integer) =
    ch <= typemax(codepoint_type(str)) ? _rev_search(_data(str), ch%UInt8, pos) : 0

function _fwd_search(str::ByteStr, b::Bytes, pos::Integer=1)
    if pos < 1
        @boundscheck boundserr(a, pos)
        return 0
    end
    n = sizeof(a)
    if pos > n
        @boundscheck pos > n + 1 && boundserr(a, pos)
        return 0
    end
    p = pointer(a)
    q = ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p + pos - 1, b, n - pos + 1)
    q == C_NULL ? 0 : Int(q - p + 1)
end

#=
_fwd_search(a::StrOrByteVec, b::AbsChars, pos::Integer = 1) =
    (isascii(b)
     ? _fwd_search(a, b%UInt8, pos)
     : _fwd_search(a, unsafe_wrap(Vector{UInt8}, string(b)), pos))

_fwd_search(a::String, b::AbsChars, pos::Integer = 1) =
    (isascii(b)
     ? base_fwd_search(a, b%UInt8, pos)
     : base_fwd_search(a, unsafe_wrap(Vector{UInt8}, string(b)), pos))
=#

function _rev_search(a::StrOrByteVec, b::Bytes, pos::Integer = sizeof(a))
    if pos < 1
        @boundscheck pos < 0 && boundserr(a, pos)
        return 0
    end
    n = sizeof(a)
    if pos > n
        @boundscheck pos > n + 1 && boundserr(a, pos)
        return 0
    end
    p = pointer(a)
    q = ccall(:memrchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), p, b, pos)
    q == C_NULL ? 0 : Int(q - p + 1)
end

#=
_rev_search(a::StrOrByteVec, ch::AbsChars, pos::Integer = length(a)) =
    (isascii(b)
     ? _rev_search(a, ch%UInt8, pos)
     : _rev_search(a, unsafe_wrap(Vector{UInt8}, string(ch)), pos))

_rev_search(a::String, ch::AbsChars, pos::Integer = length(a)) =
    (isascii(b)
     ? base_rev_search(a, ch%UInt8, pos)
     : base_rev_search(a, unsafe_wrap(Vector{UInt8}, string(ch)), pos))
=#

_fwd_search(a::StrOrByteVec, ch::ByteChars, pos::Integer = 1) =
    _fwd_search(a, ch%UInt8, pos)
_rev_search(a::StrOrByteVec, ch::ByteChars, pos::Integer = length(a)) =
    _rev_search(a, ch%UInt8, pos)

function _fwd_search(str::StrOrByteVec, t::AbstractString, pos::Integer)
    isempty(t) &&
        return 1 <= pos <= nextind(str, lastindex(str)) ? pos : boundserr(str, pos)
    t1, trest = Iterators.peel(t)
    while (pos = _fwd_search(str, t1, pos)) != 0
        ii = nextind(str, pos)
        a = Iterators.Stateful(trest)
        matched = all(splat(==), zip(SubString(str, ii), a))
        isempty(a) && matched && break
        pos = ii
    end
    pos
end

function _fwd_search(str::AbstractString, ch::AbsChars, pos::Integer)
    isascii(ch) && return _fwd_search(str, ch % UInt8, pos)
    if pos < 1
        @boundscheck pos < 0 && boundserr(a, pos)
        return 0
    elseif pos > (lst = lastindex(str))
        @boundscheck pos > lst + 1 && boundserr(str, pos)
        return 0
    end
    @inbounds isvalid(str, pos) || unierror(UTF_ERR_INVALID_INDEX, str, pos)
    b1 = first_utf8_byte(ch)
    while (pos = _fwd_search(str, b1, pos)) != 0 && str[pos] != ch
        pos = nextind(str, pos)
    end
    pos
end

_search_bloom_mask(c) = UInt64(1) << (c & 63)

const UTF8Strings = Union{Str{<:ASCIICSE}, Str{<:UTF8CSE}}

function _fwd_search(str::UTF8Strings, t::UTF8Strings, pos::Integer)
    # Check for fast case of a single byte (check for single character also)
    lastindex(t) == 1 && return _fwd_search(str, get_codeunit(t), pos)
    _fwd_search(unsafe_wrap(Vector{UInt8}, str), unsafe_wrap(Vector{UInt8}, t), pos)
end

function _fwd_search(s::ByteVec, m, t::ByteVec, n, i::Integer)
    n == 0 && return 1 <= i <= m+1 ? max(1, i) : 0
    m == 0 && return 0
    n == 1 && return _fwd_search(s, get_codeunit(t), p)
    w = m - n
    m < n  && return 0
    i - 1 > w && return 0

    bloom_mask = UInt64(0)
    skip = n - 1
    tlast = get_codeunit(t, n)
    for j in 1:n
        bloom_mask |= _search_bloom_mask(get_codeunit(t, j))
        if get_codeunit(t,j) == tlast && j < n
            skip = n - j - 1
        end
    end

    i -= 1
    while i <= w
        if get_codeunit(s, i + n) == tlast
            # check candidate
            j = 0
            while j < n - 1 && get_codeunit(s, i + j + 1) == get_codeunit(t, j + 1)
                j += 1
            end

            # match found
            j == n - 1 && return i+1

            # no match, try to rule out the next character
            if i < w && (bloom_mask & _search_bloom_mask(get_codeunit(s, i + n + 1))) == 0
                i += n
            else
                i += skip
            end
        elseif i < w && (bloom_mask & _search_bloom_mask(get_codeunit(s, i + n + 1))) == 0
            i += n
        end
        i += 1
    end

    0
end

_fwd_search(s::ByteVec, t::ByteVec, i::Integer) =
    _fwd_search(s, sizeof(s), t, sizeof(t), i)

# AbstractString implementation of the generic find_prev interface
function find_prev(testf::Function, str::AbstractString, pos::Integer)
    if pos < 1
        @boundscheck pos < 0 && boundserr(str, pos)
        return 0
    end
    n = ncodeunits(str)
    if pos > n
        @boundscheck pos > n + 1 && boundserr(str, pos)
        return 0
    end
    r = reverse(str)
    j = find_next(testf, r, reverseind(r, pos))
    j == 0 ? 0 : reverseind(str, j)
end

function _rev_search(str::AbstractString, t::AbstractString, pos::Integer)
    isempty(t) &&
        return 1 <= pos <= nextind(str, lastindex(str)) ? pos : boundserr(str, pos)
    t1, trest = Iterators.peel(Iterators.reverse(t))
    while (pos = _rev_search(str, t1, pos)) != 0
        ii = prevind(str, pos)
        a = Iterators.Stateful(trest)
        b = Iterators.Stateful(Iterators.reverse(pairs(SubString(str, 1, ii))))
        matched = all(splat(==), zip(a, (x[2] for x in b)))
        if matched && isempty(a)
            isempty(b) && return firstindex(str)
            return nextind(str, popfirst!(b)[1])
        end
        pos = ii
    end
    pos
end

function _rev_search(str::UTF8Strings, t::UTF8Strings, pos::Integer)
    # Check for fast case of a single byte
    siz = sizeof(t)
    siz == 1 && return _rev_search(str, get_codeunit(str), pos)
    if lastindex(t) != 0
        j = str <= (ncodeunits(str) ? nextind(str, pos)-1 : pos)
        return _rev_search(unsafe_wrap(Vector{UInt8}, str), unsafe_wrap(Vector{UInt8}, t), j)
    end
    pos > sizeof(str) ? 0 : (pos == 0 ? 1 : pos)
end

function _rev_search(s::ByteVec, m, t::ByteVec, n, k::Integer)
    n == 0 && return 0 <= k <= m ? max(k, 1) : 0
    m == 0 && return 0
    n == 1 && return _rev_search(s, get_codeunit(t, 1), k)
    m < n  && return 0
    k <= 0 && return 0

    bloom_mask = UInt64(0)
    skip = n - 1
    tfirst = get_codeunit(t,1)
    for j in n:-1:1
        bloom_mask |= _search_bloom_mask(get_codeunit(t,j))
        get_codeunit(t,j) == tfirst && j > 1 && (skip = j - 2)
    end

    i = min(k - n + 1, m - n + 1)
    while i > 0
        if get_codeunit(s,i) == tfirst
            # check candidate
            j = 0
            while (j += 1) < n && get_codeunit(s, i+j) == get_codeunit(t,j+1) ; end

            # match found
            j == n && return i

            # no match, try to rule out the next character
            if i > 1 && bloom_mask & _search_bloom_mask(get_codeunit(s,i-1)) == 0
                i -= n
            else
                i -= skip
            end
        elseif i > 1
            if bloom_mask & _search_bloom_mask(get_codeunit(s, i-1)) == 0
                i -= n
            end
        end
        i -= 1
    end
    0
end

_rev_search(s::ByteVec, t::ByteVec, k::Integer) =
    _rev_search(s, sizeof(s), t, sizeof(t), k)

"""
    contains(haystack::AbstractString, needle::Union{AbstractString,Regex,Char})

Determine whether the second argument is a substring of the first. If `needle`
is a regular expression, checks whether `haystack` contains a match.

# Examples
```jldoctest
julia> contains("JuliaLang is pretty cool!", "Julia")
true

julia> contains("JuliaLang is pretty cool!", 'a')
true

julia> contains("aba", r"a.a")
true

julia> contains("abba", r"a.a")
false
```
"""
function contains end

contains(haystack::Str, needle::Union{AbstractString, AbsChars, CodeUnitTypes, Int8}) =
    _fwd_search(haystack, needle, firstindex(haystack)) != 0
contains(haystack::AbstractString, needle::Union{Str, AbstractChar}) =
    _fwd_search(haystack, needle, firstindex(haystack)) != 0

in(ch::AbstractChar, str::AbstractString) = base_fwd_search(str, ch%Char, 1) != 0
in(ch::AbstractChar, str::Str)            = _fwd_search(str, ch, 1) != 0
in(ch::Char, str::Str)                    = _fwd_search(str, ch%UInt32%Text4Chr, 1) != 0

in(a::Str, b::Str) = contains(b, a)
in(a::Str, b::AbstractString) = contains(b, a)
in(a::AbstractString, b::Str) = contains(b, a)
