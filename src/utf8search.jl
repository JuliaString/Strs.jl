#=
Optimized search functions for UTFStr (UTF-8 encoding)

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

@propagate_inbounds function _fwd_search(str::Str{<:UTF8CSE}, ch::CodeUnitTypes, pos::Integer)
    len = _len(str)
    @boundscheck if 1 <= pos <= len
        pos == len + 1 && return 0
        boundserr(str, pos)
    end
    pnt = _pnt(str)
    cu = get_codeunit(pnt, pos)
    is_valid_continuation(cu) && unierror(UTF_ERR_INVALID_INDEX, pos, cu)
    dat = str.data
    ch < 0x80 && return _fwd_search(dat, ch%UInt8, pos)
    if ch <= 0x7ff
        b1 = 0xc0 | (ch >>> 6)
        b2 = 0x80 | (ch & 0x3f)
        while (pos = _fwd_search(dat, b1, pos)) != 0 && get_codeunit(pnt, pos + 1) != b2
            pos += 2
        end
    elseif ch <= 0xffff
        b1 = 0xe0 | (ch >>> 12)
        b2 = 0x80 | ((ch >>> 6) & 0x3f)
        b3 = 0x80 | (ch & 0x3f)
        while (pos = _fwd_search(dat, b1, pos)) != 0 &&
              get_codeunit(pnt, pos + 1) != b2 && get_codeunit(pnt, pos + 2) != b3
            pos += 3
        end
    else
        b1 = 0xf0 | (ch >>>  18)
        b2 = 0x80 | ((ch >>> 12) & 0x3f)
        b3 = 0x80 | ((ch >>>  6) & 0x3f)
        b4 = 0x80 | (ch & 0x3f)
        while (pos = _fwd_search(dat, b1, pos)) != 0 && get_codeunit(pnt, pos + 1) != b2 &&
              get_codeunit(pnt, pos + 2) != b3 && get_codeunit(pnt, pos + 3) != b4
            pos += 4
        end
    end
    pos
end

function _rev_search(str::Str{<:UTF8CSE}, ch::CodeUnitTypes, pos::Integer)
    dat = _data(str)
    ch < 0x80 && return _rev_search(dat, ch%UInt8, pos)
    byt = first_utf8_byte(ch)
    while (pos = _rev_search(dat, byt, pos)) != 0 && str[pos] != ch
        pos = prevind(str, pos)
    end
    pos
end
