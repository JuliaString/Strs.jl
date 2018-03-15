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

function startswith(a::AbstractString, b::AbstractString)
    a, b = Iterators.Stateful(a), Iterators.Stateful(b)
    all(splat(==), zip(a, b)) && isempty(b)
end
startswith(str::AbstractString, chars::Chars) = !isempty(str) && first(str) in chars

function endswith(a::AbstractString, b::AbstractString)
    a = Iterators.Stateful(Iterators.reverse(a))
    b = Iterators.Stateful(Iterators.reverse(b))
    all(splat(==), zip(a, b)) && isempty(b)
end
endswith(str::AbstractString, chars::Chars) = !isempty(str) && last(str) in chars

end # if false

startswith(a::Str{C}, b::Str{C}) where {C<:CSE} =
    (len = _len(b)) <= _len(a) && _memcmp(_pnt(a), _pnt(b), len) == 0

endswith(a::Str{C}, b::Str{C}) where {C<:CSE} =
    (lenb = _len(b)) <= (lena = _len(a)) && _memcmp(_pnt(a) + lena - lenb, _pnt(b), lenb) == 0

@static if false
function chop(s::AbstractString; head::Integer = 0, tail::Integer = 1)
    SubString(s, nextind(s, firstindex(s), head), prevind(s, lastindex(s), tail))
end
end # if false

function chomp(str::Str)
    len, pnt = _lenpnt(str)
    SubStr(str, 1,
           ((len <= 0 || get_codeunit(pnt + len - 1) != 0x0a)
            ? len : (len - 1 + (len < 2 || get_codeunit(pnt + len - 2) != 0x0d))))
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
lpad(str::Str, cnt::Integer, pad::AbstractString) =
    (cnt -= length(str)) <= 0 ? str : _lpad(cnt, pad, str)
lpad(ch::CodePoint, cnt::Integer, pad::AbstractString) =
    cnt < 1 ? string(ch) : _lpad(cnt, pad, ch)
lpad(str::Str, cnt::Integer, pad::AbsChar=' ') =
    (len = length(str)) > cnt ? str : string(pad^cnt, str)
lpad(ch::CodePoint, cnt::Integer, pad::AbstractChar=' ') =
    cnt < 1 ? string(ch) : string(pad^cnt, ch)

function _rpad(cnt, pad, str)
    cnt, rem = divrem(cnt, length(pad))
    rem == 0 ? string(str, pad^cnt) : string(str, pad^cnt, first(pad, rem))
end
rpad(str::Str, cnt::Integer, pad::AbstractString) =
    (cnt -= length(str)) <= 0 ? str : _rpad(cnt, pad, str)
rpad(ch::CodePoint, cnt::Integer, pad::AbstractString) =
    cnt < 1 ? string(ch) : _rpad(cnt, pad, ch)
rpad(str::Str, cnt::Integer, pad::AbsChar=' ') =
    (len = length(str)) > cnt ? str : string(pad^cnt, str)
rpad(ch::CodePoint, cnt::Integer, pad::AbsChar=' ') =
    cnt < 1 ? string(ch) : string(ch, pad^cnt)

split(str::T, splitter;
      limit::Integer=0, keep::Bool=true) where {T<:Str} =
    _split(str, splitter, limit, keep, T <: SubString ? T[] : SubString{T}[])
split(str::T, splitter::Union{Tuple{Vararg{<:AbstractChar}},
                              AbstractVector{<:AbstractChar},
                              Set{<:AbstractChar}};
      limit::Integer=0, keep::Bool=true) where {T<:Str} =
    _split(str, in(splitter), limit, keep, T <: SubString ? T[] : SubString{T}[])
split(str::T, splitter::AbstractChar;
      limit::Integer=0, keep::Bool=true) where {T<:Str} =
    _split(str, ==(splitter), limit, keep, T <: SubString ? T[] : SubString{T}[])

function _split(str::T, splitter, limit::Integer, keep_empty::Bool, strs::Array) where {T<:Str}
    i = 1
    n = lastindex(str)
    r = find(Fwd, splitter, str)
    if r != 0:-1
        j, k = first(r), nextind(str,last(r))
        while 0 < j <= n && length(strs) != limit-1
            if i < k
                (keep_empty || i < j) &&
                    push!(strs, SubString(str,i,prevind(str,j)))
                i = k
            end
            (k <= j) && (k = nextind(str,j))
            r = find(Fwd, splitter, str, k)
            r == 0:-1 && break
            j, k = first(r), nextind(str,last(r))
        end
    end
    if keep_empty || !done(str,i)
        push!(strs, SubString(str,i))
    end
    strs
end

# a bit oddball, but standard behavior in Perl, Ruby & Python:
split(str::Str) = split(str, _default_delims; limit=0, keep=false)

rsplit(str::T, splitter; limit::Integer=0, keep::Bool=true) where {T<:Str} =
    _rsplit(str, splitter, limit, keep, T <: SubString ? T[] : SubString{T}[])
rsplit(str::T, splitter::Union{Tuple{Vararg{<:AbstractChar}},
                               AbstractVector{<:AbstractChar},Set{<:AbstractChar}};
       limit::Integer=0, keep::Bool=true) where {T<:Str} =
  _rsplit(str, occursin(splitter), limit, keep, T <: SubString ? T[] : SubString{T}[])
rsplit(str::T, splitter::AbstractChar;
       limit::Integer=0, keep::Bool=true) where {T<:Str} =
  _rsplit(str, ==(splitter), limit, keep, T <: SubString ? T[] : SubString{T}[])

function _rsplit(str::Str, splitter, limit::Integer, keep_empty::Bool, strs::Array)
    n = lastindex(str)
    r = find(Rev, splitter, str, n)
    j, k = first(r), last(r)
    while j > 0 && k > 0 && length(strs) != limit-1
        (keep_empty || k < n) && pushfirst!(strs, SubString(str, nextind(str, k), n))
        n = prevind(str, j)
        r = find(Rev, splitter, str, n)
        j, k = first(r), last(r)
    end
    (keep_empty || n > 0) && pushfirst!(strs, SubString(str,1,n))
    strs
end

_replace(io, repl, str, r, pattern) =
    print(io, repl)
_replace(io, repl::Function, str, r, pattern) =
    print(io, repl(SubString(str, first(r), last(r))))
_replace(io, repl::Function, str, r, pattern::Function) =
    print(io, repl(str[first(r)]))

replace(str::Str, pat_repl::Pair{<:AbstractChar}; count::Integer=typemax(Int)) =
    replace(str, ==(first(pat_repl)) => last(pat_repl); count=count)

replace(str::Str, pat_repl::Pair{<:Union{Tuple{Vararg{<:AbstractChar}},
                                         AbstractVector{<:AbstractChar},Set{<:AbstractChar}}};
        count::Integer=typemax(Int)) =
    replace(str, in(first(pat_repl)) => last(pat_repl), count=count)

function replace(str::Str, pat_repl::Pair; count::Integer=typemax(Int))
    pattern, repl = pat_repl
    count == 0 && return str
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))
    n = 1
    i = 1
    e = lastindex(str)
    r = fnd(Fwd, pattern, str)
    (j = first(r)) == 0 && return str
    # Just return the string if not found

    out = IOBuffer(sizehint=floor(Int, 1.2 * sizeof(str)))
    while true
        k = last(r)
        if i == 1 || i <= k
            println("$i $k $(pointer(str, i)), $(j-i)")
            unsafe_write(out, pointer(str, i), UInt(j-i))
            _replace(out, repl, str, r, pattern)
        end
        if k < j
            i = j
            j > e && break
            k = nextind(str, j)
        else
            i = k = nextind(str, k)
        end
        r = fnd(Fwd, pattern, str, k)
        println("$r $i $j $k $n")
        j = first(r)
        j == 0 && break
        n == count && break
        n += 1
    end
    print(out, SubString(str, i))
    String(take!(out))
end

# TODO: allow transform as the first argument to replace?

ascii_err() = throw(ArgumentError("Not a valid ASCII string"))
ascii(str::Union{T,SubString{T}}) where {T<:Str{CSE}} =
    isascii(str) ? ASCIIStr(str) : ascii_err()
ascii(str::Union{T,SubString{T}}) where {T<:Str{SubSet_CSEs}} = ascii_err()
ascii(str::Union{T,SubString{T}}) where {T<:Str{ASCIICSE}} = str

