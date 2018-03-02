#=
Optimized search functions for UTFStr (UTF-8 encoding)

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#
function _fwd_srch_codeunit(str::Str{<:UTF8CSE}, ch::CodeUnitTypes, pos)
    len, beg = _lenpnt(str)
    is_valid_continuation(get_codeunit(pnt, pos)) && index_error(str, pos)
    ch < 0x80 && return _fwd_srch_codeunit(pnt, ch%UInt8, pos, len)
    pnt = beg + pos - 1
    fin = beg + len
    if ch <= 0x7ff
        b1, b2 = get_utf8_2(ch)
        while (pnt = _fwd_memchr(pnt + 1, b2, fin)) != C_NULL
            eq_bytes(pnt - 1, b1) && return pnt - beg
        end
    elseif ch <= 0xffff
        b1, b2, b3 = get_utf8_3(ch)
        pnt += 1
        while (pnt = _fwd_memchr(pnt + 1, b3, fin)) != C_NULL
            eq_bytes(pnt - 2, b1, b2) && return pnt - beg - 1
        end
    else
        b1, b2, b3, b4 = get_utf8_4(ch)
        pnt += 2
        while (pnt = _fwd_memchr(pnt + 1, b4, fin)) != 0
            eq_bytes(pnt - 3, b1, b2, b3) && return pnt - beg - 2
        end
    end
    0
end

function _rev_srch_codeunit(str::Str{<:UTF8CSE}, ch::CodeUnitTypes, pos)
    beg = _pnt(str)
    pnt = beg + pos
    is_valid_continuation(get_codeunit(pnt-1)) && index_error(str, pos)
    ch < 0x80 && return _rev_srch_codeunit(beg, ch%UInt8, pos)
    if ch <= 0x7ff
        b1, b2 = get_utf8_2(ch)
        beg += 1
        while (pnt = _rev_memchr(beg, b2, pnt)) != C_NULL
            eq_bytes(pnt, b1) && return pnt - beg + 1
            pnt -= 1 > beg || break
        end
    elseif ch <= 0xffff
        b1, b2, b3 = get_utf8_3(ch)
        beg += 2
        while (pnt = _rev_memchr(beg, b3, pnt)) != C_NULL
            eq_bytes(pnt, b1, b2) && return pnt - beg + 2
            pnt -= 1 > beg || break
        end
    else
        b1, b2, b3, b4 = get_utf8_4(ch)
        beg += 3
        while (pnt = _rev_memchr(beg, b4, pnt)) != C_NULL
            eq_bytes(pnt, b1, b2, b3) && return pnt - beg + 3
            pnt -= 1 > beg || break
        end
    end
    0
end
