#=
Optimized search functions for UTF32Str

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

function _fwd_search(str::UTF32Strings, ch::CodeUnitTypes, pos::Integer)
    (ch <= 0x10ffff && !is_surrogate_codeunit(ch)) || return 0
    len, pnt = _lenpnt(str)
    pos == len + 1 && return 0
    @boundscheck 1 <= pos <= len || boundserr(str, pos)
    @inbounds while pos <= len
        get_codeunit(pnt, pos) == ch && return pos
        pos += 1
    end
    0
end

function _rev_search(str::UTF32Strings, ch::CodeUnitTypes, pos::Integer)
    (ch <= 0x10ffff && !is_surrogate_codeunit(ch)) || return 0
    len, pnt = _lenpnt(str)
    pos == len + 1 && return 0
    @boundscheck 1 <= pos <= len || boundserr(str, pos)
    @inbounds while pos > 0
        get_codeunit(pnt, pos) == ch && return pos
        pos -= 1
    end
    0
end
