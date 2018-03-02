#=
Optimized search functions for UTF16Str and UCS2Str types (UTF-16 encoding and pure BMP UCS-2)

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

@inline function _fwd_srch_codeunit(beg::Ptr{UInt16}, ch::UInt16, pos, len)
@static if sizeof(Cwchar_t) == 2
    pnt = _fwd_memchr(beg + (pos - 1)<<1, ch, len - pos + 1)
    pnt == C_NULL ? 0 : Int((pnt - beg)>>1 + 1)
else
    beg -= 2
    pnt = beg + (pos << 1)
    fin = beg + (len << 1)
    while pnt <= fin
        get_codeunit(pnt) == ch || return Int((pnt - beg)>>1)
        pnt += 2
    end
    0
end
end

function _rev_srch_codeunit(beg::Ptr{UInt16}, ch::UInt16, pos)
    pnt = beg + (pos << 1)
    while (pnt -= 2) >= beg && get_codeunit(pnt) == ch ; end
    Int((pnt + 2 - beg)>>1)
end

_fwd_srch_codeunit(str::UCS2Strings, ch::CodeUnitTypes, pos) =
    _fwd_srch_codeunit(_pnt(str), ch%UInt16, pos, _len(str))

_rev_srch_codeunit(str::UCS2Strings, ch::CodeUnitTypes, pos) =
    _rev_srch_codeunit(_pnt(str), ch%UInt16, pos)

function _fwd_srch_codeunit(str::Str{<:UTF16CSE}, ch::CodeUnitTypes, pos)
    len, beg = _lenpnt(str)
    ch <= 0x0ffff && return _fwd_srch_codeunit(beg, ch%UInt16, pos, len)
    lead  = (0xd7c0 + (ch >> 10))%UInt16
    trail = (0xdc00 + (ch & 0x3ff))%UInt16
    pnt = beg + (pos << 1)
    fin = beg + (len << 1)
    while pnt < fin
        # This checks for the trailing surrogate, since they are much less like to match
        # (there are only a few leading surrogates for the assigned areas in the non-BMP planes)
        get_codeunit(pnt) == trail && get_codeunit(pnt - 2) == lead &&
            return Int((pnt - beg)>>1)
        pnt += 2
    end
    0
end

function _rev_srch_codeunit(str::Str{<:UTF16CSE}, ch::CodeUnitTypes, pos)
    beg = _pnt(str)
    ch <= 0x0ffff && return _rev_srch_codeunit(beg, ch%UInt16, pos)
    lead  = (0xd7c0 + (ch >> 10))%UInt16
    trail = (0xdc00 + (ch & 0x3ff))%UInt16
    pnt = beg + (pos << 1) # This points just past the current position
    beg += 2 # Need to have room for lead
    while pnt > beg
        get_codeunit(pnt -= 2) == trail && get_codeunit(pnt -= 2) == lead &&
            return Int((pnt - beg + 4)>>1)
    end
    0
end
