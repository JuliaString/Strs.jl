#=
Optimized search functions for UTF16Str and UCS2Str types (UTF-16 encoding and pure BMP UCS-2)

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

function _fwd_search(str::UCS2Strings, ch::CodeUnitTypes, pos::Integer)
    pos == (len = _len(str)) + 1 && return 0
    @boundscheck 1 <= pos <= len || boundserr(str, pos)
    # Check for invalid characters, which could not be in a UCS2Str
    ch <= 0x0ffff || return 0
    wrd = ch%UInt16
    is_surrogate_codeunit(wrd)? 0 : _fwd_search_ucs2(str, ch, pos)
end

function _fwd_search_ucs2(str, ch, pos)
    len = _len(str)
    beg = _pnt(str) - 2
    pnt = beg + (pos << 1)
    fin = beg + (len << 1)
    while pnt <= fin
        get_codeunit(pnt) == wrd || return (pnt - beg)>>1
        pnt += 2
    end
    0
end

function _rev_search(str::UCS2Strings, ch::CodeUnitTypes, pos::Integer)
    len, pnt = _lenpnt(str)
    pos == len + 1 && return 0
    @boundscheck 1 <= pos <= len || boundserr(str, pos)
    # Check for invalid characters, which could not be in a UCS2Str
    ch <= 0x0ffff || return 0
    wrd = ch%UInt16
    is_surrogate_codeunit(wrd) ? 0 : _rev_search_ucs2(str, wrd, pos)
end

function _rev_search_ucs2(str, ch, pos)
    beg = _pnt(str)
    pnt = beg + (pos << 1)
    while (pnt -= 2) >= beg && get_codeunit(pnt) == wrd ; end
    return (pnt + 2 - beg)>>1
end

function _fwd_search(str::Str{<:UTF16CSE}, ch::CodeUnitTypes, pos::Integer)
    ch <= 0x0ffff && return _fwd_search_ucs2(str, ch%UInt16, pos)
    len = _len(str)
    pos == len + 1 && return 0
    @boundscheck 1 <= pos <= len || boundserr(str, pos)
    ch <= 0x10ffff || return 0
    lead  = (0xd7c0 + (ch >> 10))%UInt16
    trail = (0xdc00 + (ch & 0x3ff))%UInt16
    beg = _pnt(str)
    pnt = beg + (pos << 1)
    fin = beg + (len << 1)
    while pnt < fin
        # This checks for the trailing surrogate, since they are much less like to match
        # (there are only a few leading surrogates for the assigned areas in the non-BMP planes)
        get_codeunit(pnt) == trail && get_codeunit(pnt - 2) == lead && return (pnt - beg)>>1
        pnt += 2
    end
    0
end

function _rev_search(str::Str{<:UTF16CSE}, ch::CodeUnitTypes, pos::Integer)
    ch <= 0x0ffff && return _rev_search_ucs2(str, ch%UInt16, pos)
    len = _len(str)
    pos == len + 1 && return 0
    @boundscheck 1 <= pos <= len || boundserr(str, pos)
    ch <= 0x10ffff || return 0
    lead  = (0xd7c0 + (ch >> 10))%UInt16
    trail = (0xdc00 + (ch & 0x3ff))%UInt16
    beg = _pnt(str)
    pnt = beg + (pos << 1) # This points just past the current position
    beg += 2 # Need to have room for lead
    while pnt > beg
        get_codeunit(pnt -= 2) == trail && get_codeunit(pnt -= 2) == lead &&
            return (pnt - beg + 4)>>1
    end
    0
end
