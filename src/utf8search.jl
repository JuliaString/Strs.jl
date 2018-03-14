#=
Optimized search functions for UTFStr (UTF-8 encoding)

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#
function _srch_cp(::Fwd, ::CodeUnitMulti, str::Str{UTF8CSE}, cp::AbsChar, pos, len)
    @preserve str begin
        (ch = tobase(cp)) < 0x80 && return _srch_codeunit(Fwd(), _pnt(str), ch%UInt8, pos, len)
        beg = _pnt(str)
        pnt = beg + pos - 1
        fin = beg + len
        if ch <= 0x7ff
            b1, b2 = get_utf8_2(ch)
            while (pnt = _fwd_memchr(pnt + 1, b2, fin)) != C_NULL
                eq_bytes(pnt - 1, b1) && return Int(pnt - beg)
            end
        elseif ch <= 0xffff
            b1, b2, b3 = get_utf8_3(ch)
            pnt += 1
            while (pnt = _fwd_memchr(pnt + 1, b3, fin)) != C_NULL
                eq_bytes(pnt - 2, b1, b2) && return Int(pnt - beg) - 1
            end
        else
            b1, b2, b3, b4 = get_utf8_4(ch)
            pnt += 2
            while (pnt = _fwd_memchr(pnt + 1, b4, fin)) != C_NULL
                eq_bytes(pnt - 3, b1, b2, b3) && return Int(pnt - beg) - 2
            end
        end
        0
    end
end

function _srch_cp(::Rev, ::CodeUnitMulti, str::Str{UTF8CSE}, cp::AbsChar, pos, len)
    @preserve str begin
        (ch = tobase(cp)) < 0x80 && return _srch_codeunit(Rev(), _pnt(str), ch%UInt8, pos)
        init = beg = _pnt(str)
        pnt = beg + @inbounds nextind(str, pos) - 1
        if ch <= 0x7ff
            b1, b2 = get_utf8_2(ch)
            beg += 1
            while pnt > beg && (pnt = _rev_memchr(beg, b2, pnt + 1)) != C_NULL
                eq_bytes(pnt - 1, b1) && return Int(pnt - init)
            end
        elseif ch <= 0xffff
            b1, b2, b3 = get_utf8_3(ch)
            beg += 2
            while pnt > beg && (pnt = _rev_memchr(beg, b3, pnt + 1)) != C_NULL
                eq_bytes(pnt - 2, b1, b2) && return Int(pnt - 1 - init)
            end
        else
            b1, b2, b3, b4 = get_utf8_4(ch)
            start = beg + 3
            while pnt > beg && (pnt = _rev_memchr(beg, b4, pnt + 1)) != C_NULL
                eq_bytes(pnt - 3, b1, b2, b3) && return Int(pnt - 2 - init)
            end
        end
    0
    end
end
