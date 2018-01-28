# Copyright 2018 Gandalf Software, Inc. (Scott Paul Jones)
# Licensed under MIT License, see LICENSE.md

function _cmp(::ByteCompare, a, b)
    a === b && return 0
    asiz, bsiz = sizeof(a), sizeof(b)
    res = ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, min(asiz, bsiz))
    res < 0 ? -1 : res > 0 ? 1 : cmp(asiz, bsiz)
end

function _cmp(::UTF16Compare, a, b)
    a === b && return 0
    asiz, bsiz = sizeof(a), sizeof(b)
    res = ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, min(asiz, bsiz))
    res < 0 ? -1 : res > 0 ? 1 : cmp(asiz, bsiz)
end

function _cpcmp(a::T, b) where {C<:CSE,T<:Str{C}}
    len, pnt = _lenpnt(a)
    fin = pnt + sizeof(a)
    pos = start(b)
    while pnt < fin
        done(b, pos) && return 1
        c1, pnt = _nextcp(C, pnt)
        ch, pos = next(b, pos)
        c2 = ch%UInt32
        c1 == c2 || return ifelse(c1 < c2, -1, 1)
    end
    ifelse(done(b, pos), 0, -1)
end

_cmp(::CodePointCompare, a::Str, b::AbstractString) = _cpcmp(a, b)
_cmp(::CodePointCompare, a::AbstractString, b::Str) = -_cpcmp(b, a)

function _cmp(::CodePointCompare, a::S, b::T) where {CSE1,CSE2,S<:Str{CSE1},T<:Str{CSE2}}
    len1, pnt1 = _lenpnt(a)
    fin1 = pnt1 + sizeof(a)
    len2, pnt2 = _lenpnt(b)
    fin2 = pnt2 + sizeof(b)
    while pnt1 < fin1
        pnt2 < fin2 || return 1
        c1, pnt1 = _nextcp(CSE1, pnt1)
        c2, pnt2 = _nextcp(CSE2, pnt2)
        c1 != c2 && return ifelse(c1 < c2, -1, 1)
    end
    ifelse(pnt2 < fin2, -1, 0)
end

cmp(a::Str, b::AbstractString) = _cmp(CompareStyle(a, b), a, b)
cmp(a::AbstractString, b::Str) = _cmp(CompareStyle(a, b), a, b)
cmp(a::Str, b::Str)            = _cmp(CompareStyle(a, b), a, b)

# Todo: handle comparisons of UTF16 specially, to compare first non-matching character
# as if comparing Char to Char, to get ordering correct when dealing with > 0xffff non-BMP
# characters

_fasteq(a, b) =
    (siz = sizeof(a)) == sizeof(b) &&
    ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, siz) == 0

function _cpeq(a::T, b) where {C<:CSE, T<:Str{C}}
    len, pnt = _lenpnt(a)
    fin = pnt + sizeof(a)
    pos = start(b)
    while pnt < fin
        done(b, pos) && return false
        c1, pnt = _nextcp(C, pnt)
        ch, pos = next(b, pos)
        c2 = ch%UInt32
        c1 == c2 || return false
    end
    true
end

function _cpeq(a::S, b::T) where {CSE1<:CSE, CSE2<:CSE, S<:Str{CSE1}, T<:Str{CSE2}}
    len1, pnt1 = _lenpnt(a)
    fin1 = pnt1 + sizeof(a)
    len2, pnt2 = _lenpnt(b)
    fin2 = pnt2 + sizeof(b)
    while pnt1 < fin1
        pnt2 < fin2 || return false
        c1, pnt1 = _nextcp(CSE1, pnt1)
        c2, pnt2 = _nextcp(CSE2, pnt2)
        c1 == c2 || return false
    end
    true
end

# This can be speeded up in the future with SSE/AVX instructions to unpack bytes,
# or to mask chunks of characters first to see if there are any too large in the wider of the two
function _wideneq(a::S, b::T) where {S<:Str,T<:Str}
    (len = _len(a)) == _len(b) || return false
    pnt1 = _pnt(a)
    pnt2 = _pnt(b)
    fin  = pnt1 + sizeof(a)
    while pnt1 < fin
        get_codeunit(pnt1) == get_codeunit(pnt2) || return false
        pnt1 += sizeof(codeunit(S))
        pnt2 += sizeof(codeunit(T))
    end
    true
end

_iseq(::NotEquals,       a, b) = false
_iseq(::ByteEquals,      a, b) = _fasteq(a, b)
_iseq(::ASCIIEquals,     a, b) = _cpeq(a, b)
_iseq(::WidenEquals,     a, b) = _wideneq(a, b)
_iseq(::CodePointEquals, a, b) = _cpeq(a, b)

==(a::AbstractString, b::Str) = _iseq(EqualsStyle(a, b), a, b)
==(a::Str, b::AbstractString) = _iseq(EqualsStyle(a, b), a, b)

#=
# Handle cases where it's known by the types that can't be equal
# (should do this better, it's a simple pattern)
==(a::ASCIIStr, b::T)  where {T<:Union{_LatinStr,_UCS2Str,_UTF32Str}} = false
==(a::T, b::ASCIIStr)  where {T<:Union{_LatinStr,_UCS2Str,_UTF32Str}} = false
==(a::_LatinStr, b::T) where {T<:Union{ASCIIStr,_UCS2Str,_UTF32Str}}  = false
==(a::T, b::_LatinStr) where {T<:Union{ASCIIStr,_UCS2Str,_UTF32Str}}  = false
==(a::_UCS2Str, b::T)  where {T<:Union{ASCIIStr,_LatinStr,_UTF32Str}} = false
==(a::T, b::_UCS2Str)  where {T<:Union{ASCIIStr,_LatinStr,_UTF32Str}} = false
==(a::_UTF32Str, b::T) where {T<:Union{ASCIIStr,_LatinStr,UCS2Str}}   = false
==(a::T, b::_UTF32Str) where {T<:Union{ASCIIStr,_LatinStr,UCS2Str}}   = false
=#

isless(a::AbstractString, b::Str) = cmp(a, b) < 0
isless(a::Str, b::AbstractString) = cmp(a, b) < 0
isless(a::Str, b::Str)            = cmp(a, b) < 0
