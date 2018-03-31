# Copyright 2018 Gandalf Software, Inc. (Scott Paul Jones)
# Licensed under MIT License, see LICENSE.md

function _cmp(::ByteCompare, a, b)
    asiz = ncodeunits(a)
    apnt = pointer(a)
    bsiz = ncodeunits(b)
    bpnt = pointer(b)
    asiz == bsiz && return apnt === bpnt ? 0 : _memcmp(apnt, bpnt, asiz)
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
    fin = bytoff(apnt, len)
    while apnt < fin
        (c1 = get_codeunit(apnt)) == (c2 = get_codeunit(bpnt)) || return _cmp_utf16(c1, c2)
        apnt += 2
        bpnt += 2
    end
    0
end

function _cmp(::UTF16Compare, a, b)
    asiz = ncodeunits(a)
    apnt = pointer(a)
    bsiz = ncodeunits(b)
    bpnt = pointer(b)
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

@inline _lenpntfin(str) = (ncodeunits(str), pointer(str), pointer(str) + sizeof(str))

@inline function _cpcmp(a::MaybeSub{T}, b) where {C<:CSE,T<:Str{C}}
    len, pnt, fin = _lenpntfin(a)
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

_cmp(::CodePointCompare, a::MaybeSub{<:Str}, b::AbstractString) = _cpcmp(a, b)
_cmp(::CodePointCompare, a::AbstractString, b::MaybeSub{<:Str}) = -_cpcmp(b, a)

function _cmp(::CodePointCompare,
              a::MaybeSub{S}, b::MaybeSub{T}) where {CSE1,CSE2,S<:Str{CSE1},T<:Str{CSE2}}
    len1, pnt1, fin1 = _lenpntfin(a)
    len2, pnt2, fin2 = _lenpntfin(b)
    while pnt1 < fin1
        pnt2 < fin2 || return 1
        c1, pnt1 = _nextcp(CSE1, pnt1)
        c2, pnt2 = _nextcp(CSE2, pnt2)
        c1 != c2 && return ifelse(c1 < c2, -1, 1)
    end
    ifelse(pnt2 < fin2, -1, 0)
end

cmp(a::MaybeSub{<:Str}, b::AbstractString)  = @preserve a _cmp(CompareStyle(a, b), a, b)
cmp(a::AbstractString,  b::MaybeSub{<:Str}) = @preserve b _cmp(CompareStyle(a, b), a, b)
cmp(a::MaybeSub{<:Str}, b::MaybeSub{<:Str}) = @preserve a b _cmp(CompareStyle(a, b), a, b)

# Todo: handle comparisons of UTF16 specially, to compare first non-matching character
# as if comparing Char to Char, to get ordering correct when dealing with > 0xffff non-BMP
# characters

@inline _fasteq(a, b) = (len = ncodeunits(a)) == ncodeunits(b) && _memcmp(a, b, len) == 0

function _cpeq(a::MaybeSub{T}, b) where {C<:CSE, T<:Str{C}}
    len, pnt, fin = _lenpntfin(a)
    pos = start(b)
    while pnt < fin
        done(b, pos) && return false
        c1, pnt = _nextcp(C, pnt)
        ch, pos = next(b, pos)
        c1 == codepoint(ch) || return false
    end
    true
end

_cpeq(a, b::MaybeSub{T}) where {C<:CSE, T<:Str{C}} = _cpeq(b, a)

function _cpeq(a::MaybeSub{<:Str{C1}}, b::MaybeSub{<:Str{C2}}) where {C1<:CSE, C2<:CSE}
    len1, pnt1, fin1 = _lenpntfin(a)
    len2, pnt2, fin2 = _lenpntfin(b)
    while pnt1 < fin1
        pnt2 < fin2 || return false
        c1, pnt1 = _nextcp(C1, pnt1)
        c2, pnt2 = _nextcp(C2, pnt2)
        c1 == c2 || return false
    end
    true
end

# This can be speeded up in the future with SSE/AVX instructions to unpack bytes,
# or to mask chunks of characters first to see if there are any too large in the wider of the two
function _wideneq(a::MaybeSub{S}, b::MaybeSub{T}) where {S<:Str,T<:Str}
    (len = ncodeunits(a)) == ncodeunits(b) || return false
    pnt1 = pointer(a)
    pnt2 = pointer(b)
    fin  = pnt1 + sizeof(a)
    while pnt1 < fin
        get_codeunit(pnt1) == get_codeunit(pnt2) || return false
        pnt1 += sizeof(codeunit(S))
        pnt2 += sizeof(codeunit(T))
    end
    true
end

_iseq(::NoCompare,        a, b) = false
_iseq(::ByteCompare,      a, b) = _fasteq(a, b)
_iseq(::WordCompare,      a, b) = _fasteq(a, b)
_iseq(::UTF16Compare,     a, b) = _fasteq(a, b)
_iseq(::WidenCompare,     a, b) = _wideneq(a, b)
_iseq(::ASCIICompare,     a, b) = _cpeq(a, b) # This can be optimized later
_iseq(::CodePointCompare, a, b) = _cpeq(a, b)

==(a::AbstractString,  b::MaybeSub{<:Str}) = @preserve b _iseq(EqualsStyle(a, b), a, b)
==(a::MaybeSub{<:Str}, b::AbstractString)  = @preserve a _iseq(EqualsStyle(a, b), a, b)
==(a::MaybeSub{<:Str}, b::MaybeSub{<:Str}) = @preserve a b _iseq(EqualsStyle(a, b), a, b)

isless(a::AbstractString,  b::MaybeSub{<:Str}) = cmp(a, b) < 0
isless(a::MaybeSub{<:Str}, b::AbstractString)  = cmp(a, b) < 0
isless(a::MaybeSub{<:Str}, b::MaybeSub{<:Str}) = cmp(a, b) < 0
