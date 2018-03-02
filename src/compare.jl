# Copyright 2018 Gandalf Software, Inc. (Scott Paul Jones)
# Licensed under MIT License, see LICENSE.md

_fwd_memchr(ptr::Ptr{UInt8}, byt::UInt8, len::Integer) =
    ccall(:memchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), ptr, byt, len)
_rev_memchr(ptr::Ptr{UInt8}, byt::UInt8, len::Integer) =
    ccall(:memrchr, Ptr{UInt8}, (Ptr{UInt8}, Int32, Csize_t), ptr, byt, len)

const (WidChr,OthChr) = @static sizeof(Cwchar_t) == 4 ? (UInt32,UInt16) : (UInt16,UInt32)

_fwd_memchr(ptr::Ptr{WidChr}, wchr::WidChr, len::Integer) =
    ccall(:wmemchr, Ptr{Cvoid}, (Ptr{Cvoid}, Int32, Csize_t), ptr, wchr, len)

function _fwd_memchr(pnt::Ptr{T}, wchr::T, fin::Ptr{T}) where {T<:OthChr}
    while pnt < fin
        get_codeunit(pnt) == ch && return pnt
        pnt += sizeof(T)
    end
    C_NULL
end

function _rev_memchr(beg::Ptr{T}, ch::T, pnt::Ptr{T}) where {T<:Union{UInt16,UInt32}}
    while (pnt -= sizeof(T)) >= beg
        get_codeunit(pnt) == ch && return pnt
    end
    C_NULL
end
_fwd_memchr(pnt::Ptr{T}, wchr::T, len::Integer) where {T<:OthChr} =
    _fwd_memchr(beg, ch, beg + pos * sizeof(T))
_rev_memchr(beg::Ptr{T}, ch::T, pos::Integer) where {T<:Union{UInt16,UInt32}} =
    _rev_memchr(beg, ch, beg + pos * sizeof(T))

_memcmp(a::Ptr{UInt8}, b::Ptr{UInt8}, len) =
    ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt), a, b, len)
_memcmp(a::Ptr{OthChr}, b::Ptr{OthChr}, len) =
    ccall(:memcmp, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, UInt), a, b, len)
_memcmp(a::Ptr{WidChr}, b::Ptr{WidChr}, len) =
    ccall(:wmemcmp, Int32, (Ptr{Cvoid}, Ptr{Cvoid}, UInt), a, b, len>>2)

_memcmp(a::Union{String, ByteStr}, b::Union{String, ByteStr}, siz) = _memcmp(_pnt(a), _pnt(b), siz)
_memcmp(a::WordStr, b::WordStr, siz) = _memcmp(_pnt(a), _pnt(b), siz)
_memcmp(a::QuadStr, b::QuadStr, siz) = _memcmp(_pnt(a), _pnt(b), siz)


function _cmp(::ByteCompare, a, b)
    asiz, bsiz = sizeof(a), sizeof(b)
    apnt, bpnt = pointer(a), pointer(b)
    asiz == bsiz && return apnt == bpnt ? 0 : _memcmp(apnt, bpnt, asiz)
    res = _memcmp(apnt, bpnt, min(asiz, bsiz))
    res < 0 ? -1 : res > 0 ? 1 : cmp(asiz, bsiz)
end

@inline adjust_utf16(ch) = ch - ifelse(ch < 0xe000, 0xb800, 0xe000)

_cmp_utf16(c1::UInt16, c2::UInt16) =
    ((c1 < 0xd800 || c2 < 0xd800)
     ? ifelse(c1 > c2, 1, -1)
     : ifelse(adjust_utf16(c1) > adjust_utf16(c2), 1, -1))

# This needs to handle the last word specially, if one is a surrogate pair and the other isn't
# It should be optimized to test at least 64 bits at a time for equality
function _memcmp16(apnt, bpnt, len)
    while len > 0
        (c1 = get_codeunit(apnt)) == (c2 = get_codeunit(bpnt)) || return _cmp_utf16(c1, c2)
        apnt += 2
        bpnt += 2
        len -= 1
    end
    0
end

function _cmp(::UTF16Compare, a, b)
    asiz, apnt = _lenpnt(a)
    bsiz, bpnt = _lenpnt(b)
    if asiz < bsiz
        ifelse(_memcmp16(apnt, bpnt, asiz) <= 0, -1, 1)
    elseif asiz > bsiz
        ifelse(_memcmp16(apnt, bpnt, bsiz) < 0, -1, 1)
    elseif apnt != bpnt
        _memcmp16(apnt, bpnt, asiz)
    else
        0
    end
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

_fasteq(a, b) = (siz = sizeof(a)) == sizeof(b) && _memcmp(a, b, siz) == 0

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
==(a::Str, b::Str)            = _iseq(EqualsStyle(a, b), a, b)

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
