# This file includes code originally from Julia.
# License is MIT: https://julialang.org/license

export found, find_result, fnd
export Dir, Fwd, Rev

abstract type Dir end
struct Fwd <: Dir end
struct Rev <: Dir end

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
fnd(::Type{Fwd}, pat, str, pos)

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
fnd(::Type{Rev}, pat, str, pos)

const _not_found = 0:-1

found(::Type{<:AbstractString}, v) = v != 0
find_result(::Type{<:AbstractString}, v) = v

@static if VERSION < v"0.7.0-DEV"

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

import Base: EqualTo, equalto, OccursIn, occursin

nothing_sentinel(i) = i == 0 ? nothing : i
Base.findnext(a, b::Str, i) = nothing_sentinel(fnd(Fwd, a, b, i))
Base.findfirst(a, b::Str)   = nothing_sentinel(fnd(Fwd, a, b))
Base.findprev(a, b::Str, i) = nothing_sentinel(fnd(Rev, a, b, i))
Base.findlast(a, b::Str)    = nothing_sentinel(fnd(Rev, a, b))
Base.findnext(a::Str, b::AbstractString, i) = nothing_sentinel(fnd(Fwd, a, b, i))
Base.findfirst(a::Str, b::AbstractString)   = nothing_sentinel(fnd(Fwd, a, b))
Base.findprev(a::Str, b::AbstractString, i) = nothing_sentinel(fnd(Rev, a, b, i))
Base.findlast(a::Str, b::AbstractString)    = nothing_sentinel(fnd(Rev, a, b))

end

@inline function _srch_pred(::Fwd, testf, str, pos)
    for (j, d) in pairs(SubString(str, pos))
        testf(d) && return pos + j - 1
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

function fnd(::Type{D}, fun::Function, str::AbstractString, pos::Integer) where {D<:Dir}
    if pos < 1 || pos > ncodeunits(str)
        @boundscheck boundserr(str, pos)
        return 0
    end
    @inbounds isvalid(str, pos) || index_error(str, pos)
    _srch_pred(D(), fun, str, pos)
end

const PatType = Union{Function, AbsChar, AbstractString, Regex}

fnd(::Type{Fwd}, pat::PatType, str::AbstractString) = fnd(Fwd, pat, str, 1)
fnd(::Type{Rev}, pat::PatType, str::AbstractString) = fnd(Rev, pat, str, lastindex(str))

fnd(pat::PatType, ::Type{D}, str::AbstractString) where {D<:Dir} = fnd(D, pat, str)

# AbstractString implementations of the generic find interfaces

# Defined with function first, for do syntax
fnd(fun::Function, ::Type{D}, str::AbstractString, pos::Integer) where {D<:Dir} =
    fnd(D(), fun, str, pos)

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

fnd(::Type{D}, pred::EqualTo{<:AbsChar}, str::AbstractString, pos::Integer) where {D<:Dir} =
    fnd(D, pred.x, str, pos)

function fnd(::Type{D}, ch::AbsChar, str::AbstractString, pos::Integer) where {D<:Dir}
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
    (cus = CodePointStyle(str)) === CodeUnitMulti() && (@inbounds !isvalid(str, pos)) &&
        index_error(str, pos)
    #println("fnd(::$D, '$ch', \"$str\", $pos) => $(isvalid(eltype(str), ch))")
    # Check here if ch is valid for the type of string
    isvalid(eltype(str), ch) ? _srch_cp(D(), cus, str, ch, pos, len) : 0
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
    while (pos = _srch_cp(Fwd(), cu, str, ch, pos, slen)) != 0
        nxt = nextind(str, pos)
        res = _cmp_str(str, nxt, needle, nxtsub)
        res == 0 || return pos:res
        #= if res != 0
            println("$(typeof(str)), $(typeof(needle)), \"$str\", \"$needle\", '$ch', " *
                    "nxtsub=$nxtsub, pos=$pos, slen=$slen, tlen=$tlen => $j, $(pos+j)")
            return pos:res
        end=#
        pos = nxt
        done(str, nxt) && break
    end
    _not_found
end

function _srch_strings(::Rev, ::CompareStyle, str, needle, ch, nxtsub, pos, slen, tlen)
    # We don't know if tlen and slen are even compatible
    # This should be changed to be efficient if both are CodeUnitSingle
    # We are always checking 2 or more characters here
    prv = prevind(str, pos)
    prv == 0 && return _not_found
    cu = CodePointStyle(str)
    while (pos = _srch_cp(Rev(), cu, str, ch, prv, slen)) != 0
        res = _cmp_str(str, nextind(str, pos), needle, nxtsub)
        res == 0 || return pos:res
        #= if res != 0
            println("$(typeof(str)), $(typeof(needle)), \"$str\", \"$needle\", '$ch', " *
                    "nxtsub=$nxtsub, pos=$pos, slen=$slen, tlen=$tlen => $j, $(pos+j)")
            return pos:res
        end=#
        (prv = prevind(str, pos)) == 0 && break
    end
    _not_found
end

_search_bloom_mask(ch) = UInt64(1) << (ch & 63)
_check_bloom_mask(mask, pnt, off) = (mask & _search_bloom_mask(get_codeunit(pnt, off))) == 0

# This should work for compatible CSEs, like ASCII & Latin, ASCII & UTF8, etc.
# See equals trait
function _srch_strings(::Fwd, ::Union{ByteCompare,WidenCompare}, str, needle,
                       ch, nxtsub, pos, slen, tlen)
    spnt = _pnt(str)
    npnt = _pnt(needle)

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
            j == tlen - 1 && return pos:thisind(str, pos + j)

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

function fnd(::Type{D}, needle::AbstractString, str::AbstractString, pos::Integer) where {D<:Dir}
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
    @inbounds isvalid(str, pos) || index_error(str, pos)
    tlen = ncodeunits(needle)
    tlen == 0 && return pos:pos-1
    (cmp = CanContain(str, needle)) === NoCompare() && return _not_found
    @inbounds ch, nxt = next(needle, 1)
    isvalid(eltype(str), ch) || return _not_found
    # Check if single character
    if nxt > tlen
        pos = _srch_cp(D(), CodePointStyle(str), str, ch, pos, slen)
        return pos == 0 ? _not_found : (pos:pos)
    end
    _srch_strings(D(), cmp, str, needle, ch, nxt, pos, slen, tlen)
end

@static if VERSION < v"0.7.0-DEV"
    contains(hay::AbstractString, chr::AbsChar)   = first(fnd(Fwd, chr, hay)) != 0
else
    # Avoid type piracy
    contains(hay::Str, chr::Char)                 = first(fnd(Fwd, chr, hay)) != 0
    contains(hay::AbstractString, chr::CodePoint) = first(fnd(Fwd, chr, hay)) != 0
end
contains(hay::Str, str::Str)            = first(fnd(Fwd, str, hay)) != 0
contains(hay::Str, str::AbstractString) = first(fnd(Fwd, str, hay)) != 0
contains(hay::AbstractString, str::Str) = first(fnd(Fwd, str, hay)) != 0

in(chr::CodePoint, str::AbstractString) = contains(str, chr)
in(chr::AbsChar,   str::Str)            = contains(str, chr)
in(pat::Str, str::Str)                  = contains(str, pat)
in(pat::Str, str::AbstractString)       = contains(str, pat)
in(pat::AbstractString, str::Str)       = contains(str, pat)

using Base.PCRE

using Base: DEFAULT_COMPILER_OPTS, DEFAULT_MATCH_OPTS
import Base: Regex, match

const Regex_CSEs = Union{ASCIICSE,Latin_CSEs,Binary_CSEs}

cvt_compile(::Type{<:CSE}, co)  = cvt_compile(UTF8CSE, co)
cvt_match(::Type{<:CSE}, co)    = cvt_match(UTF8CSE, co)
cvt_compile(::Type{Regex_CSEs}, co) = co & ~PCRE.UTF
cvt_match(::Type{Regex_CSEs}, co)   = co & ~PCRE.NO_UTF_CHECK
cvt_compile(::Type{UTF8CSE}, co)  = co | PCRE.NO_UTF_CHECK | PCRE.UTF
cvt_match(::Type{UTF8CSE}, co)    = co | PCRE.NO_UTF_CHECK

Regex(pattern::Str{C}, co, mo) where {C<:Byte_CSEs} =
    Regex(String(pattern), cvt_compile(C, co), mo)
Regex(pattern::Str{C}) where {C<:Byte_CSEs} =
    Regex(String(pattern),
          cvt_compile(C, DEFAULT_COMPILER_OPTS), cvt_match(C, DEFAULT_MATCH_OPTS))

function _update_compiler_opts(flags)
    options = DEFAULT_COMPILER_OPTS
    for f in flags
        options |= f=='i' ? PCRE.CASELESS  :
                   f=='m' ? PCRE.MULTILINE :
                   f=='s' ? PCRE.DOTALL    :
                   f=='x' ? PCRE.EXTENDED  :
                   throw(ArgumentError("unknown regex flag: $f"))
    end
    options
end

Regex(pattern::Str{C}, flags::AbstractString) where {C<:Byte_CSEs} =
    Regex(pattern, cvt_compile(C, _update_compile_opts(flags)), cvt_match(C, DEFAULT_MATCH_OPTS))

struct RegexMatchStr{T<:AbstractString}
    match::T
    captures::Vector{Union{Nothing,T}}
    offset::Int
    offsets::Vector{Int}
    regex::Regex
end

function show(io::IO, m::RegexMatchStr)
    print(io, "RegexMatchStr(")
    show(io, m.match)
    idx_to_capture_name = PCRE.capture_names(m.regex.regex)
    if !isempty(m.captures)
        print(io, ", ")
        for i = 1:length(m.captures)
            # If the capture group is named, show the name.
            # Otherwise show its index.
            capture_name = get(idx_to_capture_name, i, i)
            print(io, capture_name, "=")
            show(io, m.captures[i])
            if i < length(m.captures)
                print(io, ", ")
            end
        end
    end
    print(io, ")")
end

# Capture group extraction
getindex(m::RegexMatchStr, idx::Integer) = m.captures[idx]
function getindex(m::RegexMatchStr, name::Symbol)
    idx = PCRE.substring_number_from_name(m.regex.regex, name)
    idx <= 0 && error("no capture group named $name found in regex")
    m[idx]
end

getindex(m::RegexMatchStr, name::AbstractString) = m[Symbol(name)]

"""
Check if the compile flags match the current search

If not, free up old compilation, and recompile
For better performance, the Regex object should hold spots for 5 compiled regexes,
for 8-bit, UTF-8, 16-bit, UTF-16, and 32-bit code units
"""
function check_compile(::Type{C}, regex::Regex) where {C<:CSE}
    # ASCII is compatible with all (current) types, don't recompile
    re = regex.regex
    C == ASCIICSE && re != C_NULL && return
    oldopt = regex.compile_options
    cvtcomp = cvt_compile(C, oldopt)
    if cvtcomp != oldopt
        regex.compile_options = cvtcomp
        re == C_NULL || (PCRE.free_re(re); regex.regex = re = C_NULL)
        regex.match_data == C_NULL ||
            (PCRE.free_match_data(regex.match_data); regex.match_data = C_NULL)
    end
    if re == C_NULL
        regex.regex = re = PCRE.compile(regex.pattern, cvtcomp)
        PCRE.jit_compile(re)
        regex.match_data = PCRE.create_match_data(re)
        regex.ovec = PCRE.get_ovec(regex.match_data)
    end
    nothing
end

function _match(::Type{C}, re, str, idx, add_opts) where {C<:CSE}
    check_compile(C, re)
    PCRE.exec(re.regex, str, idx-1, cvt_match(C, re.match_options | add_opts), re.match_data) ||
        return nothing
    ovec = re.ovec
    n = div(length(ovec),2) - 1
    mat = SubString(str, ovec[1]+1, prevind(str, ovec[2]+1))
    cap = Union{Nothing,SubString{typeof(str)}}[ovec[2i+1] == PCRE.UNSET ?
                                               nothing :
                                               SubString(str, ovec[2i+1]+1,
                                                         prevind(str, ovec[2i+2]+1)) for i=1:n]
    off = Int[ ovec[2i+1]+1 for i=1:n ]
    RegexMatchStr(mat, cap, Int(ovec[1]+1), off, re)
end

match(re::Regex, str::Str{C}, idx::Integer, add_opts::UInt32=UInt32(0)) where {C<:CSE} =
    error("Regex not supported yet on strings with codeunit == UInt16 or UInt32")
match(re::Regex, str::Str{C}, idx::Integer, add_opts::UInt32=UInt32(0)) where {C<:Regex_CSEs} =
    _match(C, re, str, Int(idx), add_opts)
match(re::Regex, str::Str{UTF8CSE}, idx::Integer, add_opts::UInt32=UInt32(0)) =
    _match(UTF8CSE, re, str, Int(idx), add_opts)
match(re::Regex, str::Str{_LatinCSE}, idx::Integer, add_opts::UInt32=UInt32(0)) =
    _match(LatinCSE, re, Str{LatinCSE}(str), Int(idx), add_opts)


function fnd(::Type{Fwd}, re::Regex,
             str::Union{AbstractString,SubString{AbstractString}}, idx::Integer)
    if idx > ncodeunits(str)
        @boundscheck boundserr(str, idx)
        return _not_found
    end
    C = cse(str)
    check_compile(C, re)
    (PCRE.exec(re.regex, str, idx-1, cvt_match(C, re.match_options), re.match_data)
     ? ((Int(re.ovec[1])+1):prevind(str,Int(re.ovec[2])+1)) : _not_found)
end
