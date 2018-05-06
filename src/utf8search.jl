#=
Optimized search functions for UTFStr (UTF-8 encoding)

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#
function _srch_cp(::Fwd, ::MultiCU, str::Str{UTF8CSE}, cp::AbsChar, pos, len)
    @preserve str begin
        (ch = codepoint(cp)) < 0x80 &&
            return _srch_codeunit(Fwd(), pointer(str), ch%UInt8, pos, len)
        beg = pointer(str)
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

function _srch_cp(::Rev, ::MultiCU, str::Str{UTF8CSE}, cp::AbsChar, pos, len)
    @preserve str begin
        (ch = codepoint(cp)) < 0x80 && return _srch_codeunit(Rev(), pointer(str), ch%UInt8, pos)
        init = beg = pointer(str)
        @inbounds pnt = beg + nextind(str, pos)
        if ch <= 0x7ff
            pnt > (beg += 1) || return 0
            b1, b2 = get_utf8_2(ch)
            while (pnt = _rev_memchr(beg, b2, pnt - beg)) != C_NULL
                eq_bytes(pnt - 1, b1) && return Int(pnt - init)
                pnt > beg || break
            end
        elseif ch <= 0xffff
            pnt > (beg += 2) || return 0
            b1, b2, b3 = get_utf8_3(ch)
            while (pnt = _rev_memchr(beg, b3, pnt - beg)) != C_NULL
                eq_bytes(pnt - 2, b1, b2) && return Int(pnt - 1 - init)
                pnt > beg || break
            end
        else
            pnt > (beg += 3) || return 0
            b1, b2, b3, b4 = get_utf8_4(ch)
            while (pnt = _rev_memchr(beg, b4, pnt - beg)) != C_NULL
                eq_bytes(pnt - 3, b1, b2, b3) && return Int(pnt - 2 - init)
                pnt > beg || break
            end
        end
    0
    end
end
