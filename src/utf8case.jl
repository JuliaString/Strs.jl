#=
Copyright 2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

# These are more complex case folding functions, and maybe belong in a separate UTF8Str.jl package

# Note: these only check for cases in Unicode where 2 byte sequences
# could expand to 3 byte sequences.  In the standard Unicode tables,
# that is the only expansion that occurs for upper or lower case

function _lower_utf8(beg, off, len)
    # Note, the final length may be larger or smaller
    buf, out = _allocate(UInt8, len)
    unsafe_copyto!(out, beg, off)
    fin = beg + len
    pnt = beg + off
    outend = out + len
    out += off
    while pnt < fin
        ch = get_codeunit(pnt)
        if ch < 0x80
            set_codeunit!(out, ch + (_isupper_a(ch) << 5))
            out += 1
        elseif ch < 0xc4
            ch = (ch << 6) | (get_codeunit(pnt += 1) & 0x3f)
            out = output_utf8_2byte!(out, ch + (_isupper_l(ch) << 5))
        elseif ch < 0xe0
            # 2 byte
            c16 = get_utf8_2byte(pnt += 1, ch)
            if _isupper_u(c16)
                c16 = _lowercase_u(c16)
                # Check if still 2 byte, could increase to 3 byte, decrease to 1 byte
                if c16 < 0x80
                    set_codeunit!(out, c16%UInt8)
                    out += 1
                elseif c16 < 0x800
                    out = output_utf8_2byte!(out, c16)
                else
                    # Check to see if we need to resize
                    diff = (outend - out - 3) - (fin - pnt - 1)
                    if diff < 0
                        outend -= diff
                        resize!(buf, outend - out)
                        out = pointer(buf)
                        outend = out + sizeof(buf)
                    end
                    out = output_utf8_3byte!(out, c16)
                end
            else
                out = output_utf8_2byte!(out, c16)
            end
        elseif ch < 0xf0
            # 3 byte
            c16 = get_utf8_3byte(pnt += 2, ch)
            if _isupper_u(c16)
                c16 = _lowercase_u(c16)
                # Check if still 3 byte, could drop to 2 byte
                if c16 < 0x800
                    out = output_utf8_2byte!(out, c16)
                else
                    out = output_utf8_3byte!(out, c16)
                end
            else
                out = output_utf8_3byte!(out, c16)
            end
        else
            # 4 byte
            c32 = get_utf8_4byte(pnt += 3, ch)
            _isupper_u(c32) && (c32 = _lowercase_u(c32))
            out = output_utf8_4byte!(out, c32)
        end
        pnt += 1
    end
    out < outend && (buf = resize!(buf, out - pointer(buf)))
    Str(UTF8CSE, buf)
end

function _upper_utf8(beg, off, len)
    # Note, the final length may be larger or smaller
    buf, out = _allocate(UInt8, len)
    unsafe_copyto!(out, beg, off)
    fin = beg + len
    pnt = beg + off
    outend = out + len
    out += off
    while pnt < fin
        ch = get_codeunit(pnt)
        if ch < 0x80
            set_codeunit!(out, ch - (_islower_a(ch)<<5))
            out += 1
        elseif ch < 0xc4
            ch = (ch << 6) | (get_codeunit(pnt += 1) & 0x3f)
            c16 = ch == 0xb5 ? 0x39c : (ch == 0xff ? 0x178 : (ch - _can_upper_l(ch)<<5)%UInt16)
            out = output_utf8_2byte!(out, c16)
        elseif ch < 0xe0
            # 2 byte
            c16 = get_utf8_2byte(pnt += 1, ch)
            if _islower_u(c16)
                c16 = _uppercase_u(c16)
                # Check if still 2 byte, could increase to 3 byte, or decrease to 1 byte
                if c16 < 0x80
                    set_codeunit!(out, c16%UInt8)
                    out += 1
                elseif c16 < 0x800
                    out = output_utf8_2byte!(out, c16)
                else
                    # Increasing from 2 to 3 bytes, check to see if we need to resize
                    diff = (outend - out - 3) - (fin - pnt - 1)
                    if diff < 0
                        outend -= diff
                        resize!(buf, outend - out)
                        out = pointer(buf)
                        outend = out + sizeof(buf)
                    end
                    out = output_utf8_3byte!(out, c16)
                end
            else
                out = output_utf8_2byte!(out, c16)
            end
        elseif ch < 0xf0
            # 3 byte
            c16 = get_utf8_3byte(pnt += 2, ch)
            if _islower_u(c16)
                c16 = _uppercase_u(c16)
                # Check if still 3 byte, uppercase form could drop to 2 byte
                if c16 < 0x800
                    out = output_utf8_2byte!(out, c16)
                else
                    out = output_utf8_3byte!(out, c16)
                end
            else
                out = output_utf8_3byte!(out, c16)
            end
        else
            # 4 byte
            c32 = get_utf8_4byte(pnt += 3, ch)
            _islower_u(c32) && (c32 = _uppercase_u(c32))
            out = output_utf8_4byte!(out, c32)
        end
        pnt += 1
    end
    out < outend && (buf = resize!(buf, out - pointer(buf)))
    Str(UTF8CSE, buf)
end

function lowercase(str::Str{UTF8CSE})
    @preserve str begin
        pnt = beg = pointer(str)
        fin = beg + sizeof(str)
        while pnt < fin
            ch = get_codeunit(pnt)
            prv = pnt
            (ch < 0x80
             ? _isupper_a(ch)
             : (ch < 0xc4
                ? _isupper_l((ch << 6) | (get_codeunit(pnt += 1) & 0x3f))
                : _isupper_u(ch >= 0xf0
                             ? get_utf8_4byte(pnt += 3, ch)
                             : (ch < 0xe0
                                ? get_utf8_2byte(pnt += 1, ch)
                                : get_utf8_3byte(pnt += 2, ch))%UInt32))) &&
                                    return _lower_utf8(beg, prv-beg, ncodeunits(str))
            pnt += 1
        end
        str
    end
end

function uppercase(str::Str{UTF8CSE})
    @preserve str begin
        pnt = beg = pointer(str)
        fin = beg + sizeof(str)
        while pnt < fin
            ch = get_codeunit(pnt)
            prv = pnt
            (ch < 0x80
             ? _islower_a(ch)
             : (ch < 0xc4
                ? _can_upper_only_latin((ch << 6) | (get_codeunit(pnt += 1) & 0x3f))
                : _islower_u(ch >= 0xf0
                             ? get_utf8_4byte(pnt += 3, ch)
                             : (ch < 0xe0
                                ? get_utf8_2byte(pnt += 1, ch)
                                : get_utf8_3byte(pnt += 2, ch))%UInt32))) &&
                                    return _upper_utf8(beg, prv-beg, ncodeunits(str))
            pnt += 1
        end
        str
    end
end
