#=
Case folding for UTF-16 encoded strings

Copyright 2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

# These are more complex, and maybe belong in a separate UTF16Str.jl package

function _lower(::Type{<:Str{UTF16CSE}}, beg, off, len)
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
                w1, w2 = get_utf16(_lowercase_u(cp))
                set_codeunit!(out - 2, w1)
                set_codeunit!(out,     w2)
            end
        elseif _isupper_u(ch)
            set_codeunit!(out, _lowercase_u(ch))
        end
        out += 2
    end
    Str(UTF16CSE, buf)
end

function lowercase(str::Str{UTF16CSE})
    @preserve str begin
        pnt = beg = pointer(str)
        fin = beg + sizeof(str)
        while pnt < fin
            ch = get_codeunit(pnt)
            prv = pnt
            (ch > 0xd7ff # May be surrogate pair
             ? _isupper_u(ch > 0xdfff ? ch%UInt32 : get_supplementary(ch, get_codeunit(pnt += 2)))
             : (is_ascii(ch)
                ? _isupper_a(ch)
                : (is_latin(ch) ? _isupper_l(ch) : _isupper_u(ch)))) &&
                    return _lower(UTF16Str, beg, prv-beg, ncodeunits(str))
            pnt += 2
        end
    end
    str
end

function _upper(::Type{<:Str{UTF16CSE}}, beg, off, len)
    buf, out = _allocate(UInt16, len)
    unsafe_copyto!(out, beg, len)
    fin = out + (len<<1)
    out += off
    while out < fin
        ch = get_codeunit(out)
        if is_ascii(ch)
            _islower_a(ch) && set_codeunit!(out, ch -= 0x20)
        elseif is_latin(ch)
            if _can_upper_l(ch)
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
                w1, w2 = get_utf16(_uppercase_u(cp))
                set_codeunit!(out - 2, w1)
                set_codeunit!(out,     w2)
            end
        elseif _cat(ch) == Uni.LL
            set_codeunit!(out, _uppercase_u(ch))
        end
        out += 2
    end
    Str(UTF16CSE, buf)
end

function uppercase(str::UTF16Str)
    @preserve str begin
        pnt = beg = pointer(str)
        fin = beg + sizeof(str)
        while pnt < fin
            ch = get_codeunit(pnt)
            prv = pnt
            (ch > 0xd7ff # May be surrogate pair
             ? _islower_u(ch > 0xdfff ? ch%UInt32 : get_supplementary(ch, get_codeunit(pnt += 2)))
             : _can_upper_ch(ch)) &&
                 return _upper(UTF16Str, beg, prv-beg, ncodeunits(str))
            pnt += 2
        end
    end
    str
end
