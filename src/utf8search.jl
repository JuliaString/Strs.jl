#=
Optimized search functions for UTFStr (UTF-8 encoding)

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

@propagate_inbounds function search(str::Str{<:UTF8CSE}, ch::UInt32, pos::Integer)
    len = _len(str)
    @boundscheck if 1 <= pos <= len
        pos == len + 1 && return 0
        boundserr(str, pos)
    end
    pnt = _pnt(str)
    cu = get_codeunit(pnt, pos)
    is_valid_continuation(cu) && unierror(UTF_ERR_INVALID_INDEX, pos, cu)
    dat = str.data
    ch < 0x80 && return search(dat, ch%UInt8, pos)
    if ch <= 0x7ff
        b1 = 0xc0 | (ch >>> 6)
        b2 = 0x80 | (ch & 0x3f)
        while (pos = search(dat, b1, pos)) != 0
            get_codeunit(pnt, pos + 1) == b2 && break
            pos += 2
        end
    elseif ch <= 0xffff
        b1 = 0xe0 | (ch >>> 12)
        b2 = 0x80 | ((ch >>> 6) & 0x3f)
        b3 = 0x80 | (ch & 0x3f)
        while (pos = search(dat, b1, pos)) != 0
            get_codeunit(pnt, pos + 1) == b2 && get_codeunit(pnt, pos + 2) == b3 && break
            pos += 3
        end
    else
        b1 = 0xf0 | (ch >>>  18)
        b2 = 0x80 | ((ch >>> 12) & 0x3f)
        b3 = 0x80 | ((ch >>>  6) & 0x3f)
        b4 = 0x80 | (ch & 0x3f)
        while (pos = search(dat, b1, pos)) != 0
            get_codeunit(pnt, pos + 1) == b2 &&
                get_codeunit(pnt, pos + 2) == b3 &&
                get_codeunit(pnt, pos + 3) == b4 &&
                break
            pos += 4
        end
    end
    pos
end

function rsearch(str::Str{<:UTF8CSE}, ch::UInt32, pos::Integer)
    dat = _data(str)
    ch < 0x80 && return rsearch(dat, ch%UInt8, pos)
    byt = first_utf8_byte(ch)
    while (pos = rsearch(dat, byt, pos)) != 0 && str[pos] != ch
        pos = prevind(str, pos)
    end
    pos
end
