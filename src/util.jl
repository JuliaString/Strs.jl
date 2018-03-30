#=
Utility functions for Str strings

Copyright 2018 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based initially on julia/test/strings/util.jl
=#

using Base: Chars, _default_delims

# starts with and ends with predicates

@static if false

#Todo: Make fast version for Str, using CompareStyle & Contains

function starts_with(a::AbstractString, b::AbstractString)
    a, b = Iterators.Stateful(a), Iterators.Stateful(b)
    all(splat(==), zip(a, b)) && is_empty(b)
end
starts_with(str::AbstractString, chars::Chars) = !is_empty(str) && first(str) in chars

function ends_with(a::AbstractString, b::AbstractString)
    a = Iterators.Stateful(Iterators.reverse(a))
    b = Iterators.Stateful(Iterators.reverse(b))
    all(splat(==), zip(a, b)) && is_empty(b)
end
ends_with(str::AbstractString, chars::Chars) = !is_empty(str) && last(str) in chars

end # if false

starts_with(a::MaybeSub{<:Str{C}}, b::MaybeSub{<:Str{C}}) where {C<:CSE} =
    (len = _len(b)) <= _len(a) && (@preserve a b _memcmp(_pnt(a), _pnt(b), len)) == 0

ends_with(a::MaybeSub{<:Str{C}}, b::MaybeSub{<:Str{C}}) where {C<:CSE} =
    (lenb = _len(b)) <= (lena = _len(a)) &&
    (@preserve a b _memcmp(_pnt(a) + lena - lenb, _pnt(b), lenb)) == 0

@static if false
function chop(s::AbstractString; head::Integer = 0, tail::Integer = 1)
    SubString(s, nextind(s, firstindex(s), head), prevind(s, lastindex(s), tail))
end
end # if false

function chomp(str::MaybeSub{<:Str})
    (len = ncodeunits(str)) != 0 && @preserve str begin
        pnt = pointer(str)
        get_codeunit(pnt, len) == 0xa &&
            (len -= (len > 1 && get_codeunit(pnt, len - 1) == 0x0d) + 1)
    end
    SubString(str, 1, thisind(str, len))
end

@static if false
function lstrip(s::AbstractString, chars::Chars=_default_delims)
    e = lastindex(s)
    for (i, c) in pairs(s)
        !(c in chars) && return SubString(s, i, e)
    end
    SubString(s, e+1, e)
end

function rstrip(s::AbstractString, chars::Chars=_default_delims)
    for (i, c) in Iterators.reverse(pairs(s))
        c in chars || return SubString(s, 1, i)
    end
    SubString(s, 1, 0)
end

strip(s::AbstractString) = lstrip(rstrip(s))
strip(s::AbstractString, chars::Chars) = lstrip(rstrip(s, chars), chars)

end # if false

## string padding functions ##

# Todo: make these not build two strings, and return the correct type (i.e. of the first argument)

function _lpad(cnt, pad, str)
    cnt, rem = divrem(cnt, length(pad))
    rem == 0 ? string(pad^cnt, str) : string(pad^cnt, first(pad, rem), str)
end
lpad(str::MaybeSub{<:Str}, cnt::Integer, pad::AbstractString) =
    (cnt -= length(str)) <= 0 ? str : _lpad(cnt, pad, str)
lpad(ch::CodePoint, cnt::Integer, pad::AbstractString) =
    (cnt -= 1) <= 0 ? string(ch) : _lpad(cnt, pad, ch)
lpad(str::MaybeSub{<:Str}, cnt::Integer, pad::AbsChar=' ') =
    (cnt -= length(str)) <= 0 ? str : string(pad^cnt, str)
lpad(ch::CodePoint, cnt::Integer, pad::AbstractChar=' ') =
    (cnt -= 1) <= 0 ? string(ch) : string(pad^cnt, ch)

function _rpad(cnt, pad, str)
    cnt, rem = divrem(cnt, length(pad))
    rem == 0 ? string(str, pad^cnt) : string(str, pad^cnt, first(pad, rem))
end
rpad(str::MaybeSub{<:Str}, cnt::Integer, pad::AbstractString) =
    (cnt -= length(str)) <= 0 ? str : _rpad(cnt, pad, str)
rpad(ch::CodePoint, cnt::Integer, pad::AbstractString) =
    (cnt -= 1) <= 0 ? string(ch) : _rpad(cnt, pad, ch)
rpad(str::MaybeSub{<:Str}, cnt::Integer, pad::AbsChar=' ') =
    (cnt -= length(str)) <= 0 ? str : string(str, pad^cnt)
rpad(ch::CodePoint, cnt::Integer, pad::AbsChar=' ') =
    (cnt -= 1) <= 0 ? string(ch) : string(ch, pad^cnt)

const SetOfChars = Union{Tuple{Vararg{<:AbstractChar}},
                         AbstractVector{<:AbstractChar},
                         Set{<:AbstractChar}}

split(str::MaybeSub{T}, splitter; limit::Integer=0, keep::Bool=true) where {T<:Str} =
    _split(str, splitter, limit, keep, SubString{T}[])
split(str::MaybeSub{T}, splitter::SetOfChars; limit::Integer=0, keep::Bool=true) where {T<:Str} =
    _split(str, in(splitter), limit, keep, SubString{T}[])
split(str::MaybeSub{T}, splitter::AbstractChar;
      limit::Integer=0, keep::Bool=true) where {T<:Str} =
          _split(str, ==(splitter), limit, keep, SubString{T}[])

function _split(str::MaybeSub{T}, splitter, limit::Integer,
                keep_empty::Bool, strs::Array) where {T<:Str}
    pos = 1
    lst = lastindex(str)
    res = find(First, splitter, str)
    if res != 0:-1
        j, k = first(res), nextind(str, last(res))
        while 0 < j <= lst && length(strs) != limit - 1
            if pos < k
                (keep_empty || pos < j) &&
                    push!(strs, SubString(str, pos, prevind(str, j)))
                pos = k
            end
            (k <= j) && (k = nextind(str, j))
            res = find(Fwd, splitter, str, k)
            res == 0:-1 && break
            j, k = first(res), nextind(str, last(res))
        end
    end
    (keep_empty || pos <= lst) ? push!(strs, SubString(str, pos)) : strs
end

# a bit oddball, but standard behavior in Perl, Ruby & Python:
split(str::MaybeSub{<:Str}) = split(str, _default_delims; limit=0, keep=false)

rsplit(str::MaybeSub{T}, splitter; limit::Integer=0, keep::Bool=true) where {T<:Str} =
    _rsplit(str, splitter, limit, keep, SubString{T}[])
rsplit(str::MaybeSub{T}, splitter::SetOfChars; limit::Integer=0, keep::Bool=true) where {T<:Str} =
    _rsplit(str, occurs_in(splitter), limit, keep, SubString{T}[])
rsplit(str::MaybeSub{T}, splitter::AbstractChar; limit::Integer=0, keep::Bool=true) where {T<:Str} =
    _rsplit(str, ==(splitter), limit, keep, SubString{T}[])

function _rsplit(str::MaybeSub{T}, splitter, limit::Integer,
                 keep_empty::Bool, strs::Array) where {T<:Str}
    res = find(Last, splitter, str)
    j, k = first(res), last(res)
    pos = lastindex(str)
    while j > 0 && k > 0 && length(strs) != limit-1
        (keep_empty || k < pos) && pushfirst!(strs, SubString(str, nextind(str, k), pos))
        (pos = prevind(str, j)) > 0 || break
        res = find(Rev, splitter, str, pos)
        j, k = first(res), last(res)
    end
    (keep_empty || pos > 0) && pushfirst!(strs, SubString(str, 1, pos))
    strs
end

_replace(io, repl, str, r, pattern) =
    print(io, repl)
_replace(io, repl::Function, str, r, pattern) =
    print(io, repl(SubString(str, first(r), last(r))))
_replace(io, repl::Function, str, r, pattern::Function) =
    print(io, repl(str[first(r)]))

replace(str::MaybeSub{<:Str}, pat_repl::Pair{<:AbstractChar}; count::Integer=typemax(Int)) =
    replace(str, ==(first(pat_repl)) => last(pat_repl); count=count)
replace(str::MaybeSub{<:Str}, pat_repl::Pair{<:SetOfChars}; count::Integer=typemax(Int)) =
    replace(str, in(first(pat_repl)) => last(pat_repl), count=count)

# Todo: this is using print, but it should be changed to make sure that everything is done via
# writes (i.e. no translation to UTF-8)
function replace(str::MaybeSub{T}, pat_repl::Pair; count::Integer=typemax(Int)) where {T<:Str}
    pattern, repl = pat_repl
    count == 0 && return str
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))
    pos = 1
    lst = lastindex(str)
    res = find(First, pattern, str)
    # Just return the string if not found
    (j = first(res)) == 0 && return str

    out = IOBuffer(sizehint=floor(Int, 1.2 * sizeof(str)))
    while true
        k = last(res)
        if pos == 1 || pos <= k
            print(out, SubString(str, pos, thisind(str, j - 1)))
            #unsafe_write(out, pointer(str, pos), UInt(j - pos))
            _replace(out, repl, str, res, pattern)
        end
        if k < j
            pos = j
            j > lst && break
            k = nextind(str, j)
        else
            pos = k = nextind(str, k)
        end
        (count -= 1) > 0 || break
        k > lst && break
        res = find(Fwd, pattern, str, k)
        (j = first(res)) == 0 && break
    end
    print(out, SubString(str, pos))
    convert(T, String(take!(out)))
end
