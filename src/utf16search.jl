#=
Optimized search functions for UTF16Str and UCS2Str types (UTF-16 encoding and pure BMP UCS-2)

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

function _fwd_search(str::UCS2Strings, ch::CodeUnitTypes, pos::Integer)
    pos == (len = _len(str)) + 1 && return 0
    @boundscheck pos <= pos <= len || boundserr(str, pos)
    # Check for invalid characters, which could not be in a UCS2Str
    ch <= 0x0ffff || return 0
    wrd = ch%UInt16
    is_surrogate_codeunit(wrd) && return 0
    beg = _pnt(str) - 2
    pnt = beg + (pos << 1)
    fin = beg + (len << 1)
    @inbounds while pnt <= fin
        get_codeunit(pnt) == wrd && return (pnt - beg)>>1
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
    is_surrogate_codeunit(wrd) && return 0
    @inbounds while pos > 0
        get_codeunit(pnt, pos) == wrd && return pos
        pos -= 1
    end
    0
end

function _fwd_search(str::Str{<:UTF16CSE}, ch::CodeUnitTypes, pos::Integer)
    ch <= 0x0ffff && return _fwd_search(str, ch%UInt16, pos)
    len, pnt = _lenpnt(str)
    pos == len + 1 && return 0
    @boundscheck 1 <= pos <= len || boundserr(str, pos)
    ch <= 0x10ffff || return 0
    wrd  = (0xd7c0 + (ch >> 10))%UInt16
    surr = (0xdc00 + (ch & 0x3ff))%UInt16
    @inbounds while pos < len
        if get_codeunit(pnt, pos) == wrd
            get_codeunit(pnt, pos + 1) == surr && return pos
            pos += 1
        end
        pos += 1
    end
    0
end

function _rev_search(str::Str{<:UTF16CSE}, ch::CodeUnitTypes, pos::Integer)
    ch <= 0x0ffff && return _rev_search(str, ch%UInt16, pos)
    len, pnt = _lenpnt(str)
    pos == len + 1 && return 0
    @boundscheck 1 <= pos <= len || boundserr(str, pos)
    ch <= 0x10ffff || return 0
    wrd  = (0xd7c0 + (ch >> 10))%UInt16
    surr = (0xdc00 + (ch & 0x3ff))%UInt16
    @inbounds while pos > 1
        get_codeunit(pnt, pos) == surr && get_codeunit(pnt, pos -= 1) == wrd && return pos
        pos -= 1
    end
    0
end
