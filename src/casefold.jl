#=
Case folding for Unicode Str types

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

_lowercase_l(ch) = ifelse(_isupper_al(ch), ch + 0x20, ch)
_uppercase_l(ch) = ifelse(_can_upper(ch),  ch - 0x20, ch)

_lowercase(ch) = islatin(ch) ? _lowercase_l(ch) : _lowercase_u(ch)
_uppercase(ch) = islatin(ch) ? _uppercase_l(ch) : _uppercase_u(ch)
_titlecase(ch) = islatin(ch) ? _uppercase_l(ch) : _titlecase_u(ch)

lowercase(ch::T) where {T<:CodePointTypes} = T(_lowercase(tobase(ch)))
uppercase(ch::T) where {T<:CodePointTypes} = T(_uppercase(tobase(ch)))
titlecase(ch::T) where {T<:CodePointTypes} = T(_titlecase(tobase(ch)))

lowercase(ch::ASCIIChr) = ifelse(isupper(ch), ASCIIChr(ch + 0x20), ch)
uppercase(ch::ASCIIChr) = ifelse(islower(ch), ASCIIChr(ch - 0x20), ch)
titlecase(ch::ASCIIChr) = uppercase(ch)

function ucfirst(str::ASCIIStr)
    (len = _len(str)) == 0 && return str
    pnt = _pnt(str)
    ch = get_codeunit(pnt)
    _islower_a(ch) || return str
    out = _allocate(len)
    unsafe_copyto!(out, pnt, len)
    set_codeunit!(out, ch - 0x20)
    Str(ASCIICSE, out)
end

function lcfirst(str::ASCIIStr)
    (len = _len(str)) == 0 && return str
    pnt = _pnt(str)
    ch = get_codeunit(pnt)
    _isupper_a(ch) || return str
    out = _allocate(len)
    unsafe_copyto!(out, pnt, len)
    set_codeunit!(out, ch + 0x20)
    Str(ASCIICSE, out)
end

function _upper(::Type{ASCIIStr}, beg::Ptr{UInt8}, off, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    unsafe_copyto!(out, beg, len)
    out += off
    while out < fin
        ch = get_codeunit(out)
        _islower_a(ch) && set_codeunit!(out, ch - 0x20)
        out += 1
    end
    Str(ASCIICSE, buf)
end

function _lower(::Type{ASCIIStr}, beg::Ptr{UInt8}, off, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    unsafe_copyto!(out, beg, len)
    out += off
    while out < fin
        ch = get_codeunit(out)
        _isupper_a(ch) && set_codeunit!(out, ch + 0x20)
        out += 1
    end
    Str(ASCIICSE, buf)
end

function _upper(::Type{LatinStr}, beg::Ptr{UInt8}, off, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    unsafe_copyto!(out, beg, len)
    out += off
    while out < fin
        ch = get_codeunit(out)
        _can_upper(ch) && set_codeunit!(out, ch - 0x20)
        out += 1
    end
    Str(LatinCSE, buf)
end

function uppercase(str::ASCIIStr)
    (len = _len(str)) == 0 && return str
    pnt = beg = _pnt(str)
    fin = beg + len
    while pnt < fin
        _islower_a(get_codeunit(pnt)) && return _upper(ASCIIStr, beg, pnt-beg, len)
        pnt += 1
    end
    str
end

function lowercase(str::ASCIIStr)
    (len = _len(str)) == 0 && return str
    pnt = beg = _pnt(str)
    fin = beg + len
    while pnt < fin
        _isupper_a(get_codeunit(pnt)) && return _lower(ASCIIStr, beg, pnt-beg, len)
        pnt += 1
    end
    str
end

function ucfirst(str::LatinStr)
    (len = _len(str)) == 0 && return str
    pnt = _pnt(str)
    ch = get_codeunit(pnt)
    _can_upper(ch) || return str
    buf, out = _allocate(UInt8, len)
    set_codeunit!(out, ch - 0x20)
    len > 1 && unsafe_copyto!(out, pnt+1, len-1)
    Str(LatinCSE, buf)
end

# Special handling for characters that can't map into Latin1
function ucfirst(str::_LatinStr)
    (len = _len(str)) == 0 && return str
    pnt = _pnt(str)
    ch = get_codeunit(pnt)
    if _can_upper(ch)
        buf, out8 = _allocate(UInt8, len)
        set_codeunit!(out8, ch - 0x20)
        len > 1 && unsafe_copyto!(out8, pnt+1, len-1)
        Str(_LatinCSE, buf)
    elseif (ch == 0xb5) | (ch == 0xff)
        buf, out = _allocate(UInt16, len)
        set_codeunit!(out, ifelse(ch == 0xb5, 0x39c, 0x178))
        # Perform the widen operation on the rest (should be done via SIMD)
        @inbounds for i = 2:len
            set_codeunit!(out += 2, get_codeunit(pnt += 2)%UInt16)
        end
        Str(_UCS2CSE, buf)
    else
        str
    end
end

function lcfirst(str::T) where {T<:LatinStrings}
    (len = _len(str)) == 0 && return str
    pnt = _pnt(str)
    ch = get_codeunit(pnt)
    _isupper(ch) || return str
    buf, out = _allocate(UInt8, len)
    set_codeunit!(out, ch + 0x20)
    len > 1 && unsafe_copyto!(out, pnt+1, len-1)
    Str(cse(T), buf)
end

lowercase(ch::T) where {T<:LatinChars} = T(_lowercase_l(tobase(ch)))
uppercase(ch::LatinChr) = LatinChr(_uppercase_l(tobase(ch)))

# Special handling for case where this is just an optimization of the first 256 bytes of Unicode,
# and not the 8-bit ISO 8859-1 character set
function uppercase(ch::_LatinChr)
    cb = tobase(ch)
    _can_upper(cb) && return _LatinChr(cb - 0x20)
    # We don't uppercase 0xdf, the ß character
    cb == 0xb5 ? UCS2Chr(0x39c) : (cb == 0xff ? UCS2Chr(0x178) : ch)
end
titlecase(ch::LatinChars) = uppercase(ch)

function _upper(::Type{_LatinStr}, beg::Ptr{UInt8}, off, len)
    fin = beg + len
    cur = beg + off
    # Need to scan the rest of the string to see if _widenupper needs to be called
    while cur < fin
        ((ch = get_codeunit(cur)) == 0xb5) | (ch == 0xff) && return _widenupper(beg, off, len)
        cur += 1
    end
    buf, out = _allocate(UInt8, len)
    fin = out + len
    unsafe_copyto!(out, beg, len)
    out += off
    while out < fin
        ch = get_codeunit(out)
        _can_upper(ch) && set_codeunit!(out, ch - 0x20)
        out += 1
    end
    Str(_LatinCSE, buf)
end

function _widen!(dst::Ptr{T}, src::Ptr{S}, fin::Ptr{S}) where {T<:CodeUnitTypes, S<:CodeUnitTypes}
    while src < fin
        set_codeunit!(dst, get_codeunit(src)%T)
        dst += sizeof(T)
        src += sizeof(S)
    end
    nothing
end
const _narrow! = _widen!  # When this is optimized for SSE/AVX/etc. instructions, will be different

function _widenupper(beg::Ptr{UInt8}, off, len)
    buf, out = _allocate(UInt16, len)
    fin = out + len
    cur = beg + len
    _widen!(out, beg, cur)
    out += off
    while out < fin
        ch = get_codeunit(cur)
        if ch == 0xb5
            set_codeunit!(out, 0x39c)
        elseif ch == 0xff
            set_codeunit!(out, 0x178)
        else
            set_codeunit!(out, _can_upper(ch) ? ch - 0x20 : ch)
        end
        cur += 1
        out += 2
    end
    Str(_UCS2CSE, buf)
end

function uppercase(str::LatinStr)
    (len = _len(str)) == 0 && return str
    pnt = beg = _pnt(str)
    fin = beg + len
    while pnt < fin
        _can_upper(get_codeunit(pnt)) && return _upper(LatinStr, beg, pnt-beg, len)
        pnt += 1
    end
    str
end

function uppercase(str::_LatinStr)
    (len = _len(str)) == 0 && return str
    pnt = beg = _pnt(str)
    fin = beg + len
    while pnt < fin
        ch = get_codeunit(pnt)
        ((ch == 0xb5) | (ch == 0xff)) && return _widenupper(beg, pnt, len)
        _can_upper(ch) && return _upper(_LatinStr, beg, pnt-beg, len)
        pnt += 1
    end
    str
end

function _lower(::Type{T}, beg::Ptr{UInt8}, off, len) where {T<:LatinStrings}
    buf, out = _allocate(UInt8, len)
    fin = out + len
    unsafe_copyto!(out, beg, len)
    out += off
    while out < fin
        ch = get_codeunit(out)
        _isupper_al(ch) && set_codeunit!(out, ch + 0x20)
        out += 1
    end
    Str(cse(T), buf)
end

function lowercase(str::T) where {T<:LatinStrings}
    (len = _len(str)) == 0 && return str
    pnt = beg = _pnt(str)
    fin = beg + len
    while pnt < fin
        _isupper_al(get_codeunit(pnt)) && return _lower(T, beg, pnt-beg, len)
        pnt += 1
    end
    str
end

_can_upper_latin(ch) = _can_upper(ch) | (ch == 0xb5) | (ch == 0xff)
_can_upper_only_latin(ch) = _can_upper_l(ch) | (ch == 0xb5) | (ch == 0xff)

@inline _can_upper_ch(ch) =
    ch <= 0x7f ? _islower_a(ch) : (ch <= 0xff ? _can_upper_only_latin(ch) : _islower_u(ch))
@inline _can_lower_ch(ch) =
    ch <= 0x7f ? _isupper_a(ch) : (ch <= 0xff ? _isupper_l(ch) : _isupper_u(ch))

# result must have at least one character > 0xff, so if the only character(s)
# > 0xff became <= 0xff, then the result may need to be narrowed and returned as _LatinStr

function _lower(::Type{T}, beg, off, len) where {T<:_UCS2Str}
    CU = codeunit(T)
    buf, out = _allocate(CU, len)
    unsafe_copyto!(out, beg, len)
    fin = out + (len*sizeof(CU))
    out += off
    flg = false
    while out < fin
        ch = get_codeunit(out)
        if ch <= 0x7f
            _isupper_a(ch) && set_codeunit!(out, ch += 0x20)
        elseif ch <= 0xff
            _isupper_l(ch) && set_codeunit!(out, ch += 0x20)
        elseif _isupper_u(ch)
            ch = _lowercase_u(ch)
            flg = ch <= 0xff
            set_codeunit!(out, ch)
        end
        out += sizeof(CU)
    end
    if flg && islatin(buf)
        out = pointer(buf)
        buf = _allocate(len)
        _narrow!(pointer(buf), out, out + len)
        Str(_LatinCSE, buf)
    else
        Str(cse(T), buf)
    end
end

function _lower(::Type{T}, beg, off, len) where {T<:Union{UCS2Str,UTF32Strings}}
    CU = codeunit(T)
    buf, out = _allocate(CU, len)
    unsafe_copyto!(out, beg, len)
    fin = out + (len*sizeof(CU))
    out += off
    while out < fin
        ch = get_codeunit(out)
        if ch <= 0x7f
            _isupper_a(ch) && set_codeunit!(out, ch += 0x20)
        elseif ch <= 0xff
            _isupper_l(ch) && set_codeunit!(out, ch += 0x20)
        elseif _isupper_u(ch)
            set_codeunit!(out, _lowercase_u(ch))
        end
        out += sizeof(CU)
    end
    Str(cse(T), buf)
end

function lowercase(str::T) where {T<:Union{UCS2Strings, UTF32Strings}}
    CU = codeunit(T)
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        _can_lower_ch(get_codeunit(pnt)) && return _lower(T, beg, pnt-beg, _len(str))
        pnt += sizeof(CU)
    end
    str
end

function _upper(::Type{T}, beg, off, len) where {T<:Union{UCS2Strings,UTF32Strings}}
    CU = codeunit(T)
    buf, out = _allocate(CU, len)
    unsafe_copyto!(out, beg, len)
    fin = out + (len*sizeof(CU))
    out += off
    while out < fin
        ch = get_codeunit(out)
        if ch <= 0x7f
            _islower_a(ch) && set_codeunit!(out, ch -= 0x20)
        elseif ch > 0xff
            _islower_u(ch) && set_codeunit!(out, _uppercase_u(ch))
        elseif _can_upper(ch)
            set_codeunit!(out, ch -= 0x20)
        elseif ch == 0xb5
            set_codeunit!(out, 0x39c)
        elseif ch == 0xff
            set_codeunit!(out, 0x178)
        end
        out += sizeof(CU)
    end
    Str(cse(T), buf)
end

function uppercase(str::T) where {T<:Union{UCS2Strings, UTF32Strings}}
    CU = codeunit(T)
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        _can_upper_ch(get_codeunit(pnt)) && return _upper(T, beg, pnt-beg, _len(str))
        pnt += sizeof(CU)
    end
    str
end
