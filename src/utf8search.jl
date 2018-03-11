#=
Optimized search functions for UTFStr (UTF-8 encoding)

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#
function _srch_utf8_fwd(beg, ch, pnt, fin)
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
        println("beg=$beg, pnt=$pnt, fin=$fin, ch=$ch, $b1,$b2,$b3,$b4")
        while (pnt = _fwd_memchr(pnt + 1, b4, fin)) != C_NULL
            println(" => $pnt")
            eq_bytes(pnt - 3, b1, b2, b3) && return Int(pnt - beg) - 2
        end
    end
    0
end

function _srch_cp(::Fwd, str::Str{UTF8CSE}, cp::AbsChar, pos, len)
    beg = _pnt(str)
    is_valid_continuation(get_codeunit(beg, pos)) && index_error(str, pos)
    ((ch = tobase(cp)) < 0x80
     ? _srch_codeunit(Fwd(), beg, ch%UInt8, pos, len)
     : _srch_utf8_fwd(beg, ch, beg + pos - 1, beg + len))
end

function _srch_utf8_rev(beg, ch, pnt)
    if ch <= 0x7ff
        b1, b2 = get_utf8_2(ch)
        beg += 1
        while (pnt = _rev_memchr(beg, b2, pnt)) != C_NULL
            eq_bytes(pnt, b1) && return Int(pnt - beg) + 1
            pnt -= 1 > beg || break
        end
    elseif ch <= 0xffff
        b1, b2, b3 = get_utf8_3(ch)
        beg += 2
        while (pnt = _rev_memchr(beg, b3, pnt)) != C_NULL
            eq_bytes(pnt, b1, b2) && return Int(pnt - beg) + 2
            pnt -= 1 > beg || break
        end
    else
        b1, b2, b3, b4 = get_utf8_4(ch)
        beg += 3
        while (pnt = _rev_memchr(beg, b4, pnt)) != C_NULL
            eq_bytes(pnt, b1, b2, b3) && return Int(pnt - beg) + 3
            pnt -= 1 > beg || break
        end
    end
    0
end

function _srch_cp(::Rev, str::Str{UTF8CSE}, cp::AbsChar, pos, len)
    beg = _pnt(str)
    is_valid_continuation(get_codeunit(beg + pos - 1)) && index_error(str, pos)
    ((ch = tobase(cp)) < 0x80
     ? _srch_codeunit(Rev(), beg, ch%UInt8, pos)
     : _srch_utf8_rev(beg, ch, beg + pos))
end
