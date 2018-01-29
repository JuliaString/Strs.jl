#=
Case folding for UTF-16 encoded strings

Copyright 2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

# These are more complex, and maybe belong in a separate UTF16Str.jl package

function _lower(::Type{UTF16Str}, beg, off, len)
    buf, out = _allocate(UInt16, len)
    unsafe_copyto!(out, beg, len)
    fin = out + (len<<1)
    out += off
    while out < fin
        ch = get_codeunit(out)
        if ch <= 0x7f
            _isupper_a(ch) && set_codeunit!(out, ch += 0x20)
        elseif ch <= 0xff
            _isupper_l(ch) && set_codeunit!(out, ch += 0x20)
        elseif is_surrogate_trail(ch)
            # pick up previous code unit (lead surrogate)
            cp = get_supplementary(get_codeunit(out - 2), ch)
            if _isupper_u(cp)
                cp = _lowercase_u(cp)
                set_codeunit!(out - 2, (0xd7c0 + (cp >>> 10))%UInt16)
                set_codeunit!(out,     (0xdc00 + (cp & 0x3ff))%UInt16)
            end
        elseif _isupper_u(ch)
            set_codeunit!(out, _lowercase_u(ch))
        end
        out += 2
    end
    Str(UTF16CSE, buf)
end

function lowercase(str::UTF16Str)
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        prv = pnt
        (ch > 0xd7ff # May be surrogate pair
         ? _isupper_u(ch > 0xdfff ? ch%UInt32 : get_supplementary(ch, get_codeunit(pnt += 2)))
         : (isascii(ch) ? _isupper_a(ch) : (islatin(ch) ? _isupper_l(ch) : _isupper_u(ch)))) &&
             return _lower(UTF16Str, beg, prv-beg, _len(str))
        pnt += 2
    end
    str
end

function _upper(::Type{UTF16Str}, beg, off, len)
    buf, out = _allocate(UInt16, len)
    unsafe_copyto!(out, beg, len)
    fin = out + (len<<1)
    out += off
    while out < fin
        ch = get_codeunit(out)
        if islatin(ch)
            if _islower_al(ch)
                set_codeunit!(out, ch -= 0x20)
            elseif ch == 0xb5
                set_codeunit!(out, 0x39c)
            elseif ch == 0xff
                set_codeunit!(out, 0x178)
            end
        elseif is_surrogate_trail(ch)
            # pick up previous code unit (lead surrogate)
            cp = get_supplementary(get_codeunit(out - 2), ch)
            if _cat(cp) == Uni.LL
                cp = _uppercase_u(cp)
                set_codeunit!(out - 2, (0xd7c0 + (cp >>> 10))%UInt16)
                set_codeunit!(out,     (0xdc00 + (cp & 0x3ff))%UInt16)
            end
        elseif _cat(ch) == Uni.LL
            set_codeunit!(out, _uppercase_u(ch))
        end
        out += 2
    end
    Str(UTF16CSE, buf)
end

function uppercase(str::UTF16Str)
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        prv = pnt
        (ch > 0xd7ff # May be surrogate pair
         ? _islower_u(ch > 0xdfff ? ch%UInt32 : get_supplementary(ch, get_codeunit(pnt += 2)))
         : _can_upper_ch(ch)) &&
             return _upper(UTF16Str, beg, prv-beg, _len(str))
        pnt += 2
    end
    str
end
