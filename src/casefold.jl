#=
Case folding for Unicode Str types

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

_lowercase_l(ch) = ifelse(_isupper_al(ch), ch + 0x20, ch)
_uppercase_l(ch) = ifelse(_can_upper(ch),  ch - 0x20, ch)

_lowercase(ch) = is_latin(ch) ? _lowercase_l(ch) : _lowercase_u(ch)
_uppercase(ch) = is_latin(ch) ? _uppercase_l(ch) : _uppercase_u(ch)
_titlecase(ch) = is_latin(ch) ? _uppercase_l(ch) : _titlecase_u(ch)

lowercase(ch::T) where {T<:Chr} = T(_lowercase(codepoint(ch)))
uppercase(ch::T) where {T<:Chr} = T(_uppercase(codepoint(ch)))
titlecase(ch::T) where {T<:Chr} = T(_titlecase(codepoint(ch)))

lowercase(ch::ASCIIChr) = ifelse(_isupper_a(ch), ASCIIChr(ch + 0x20), ch)
uppercase(ch::ASCIIChr) = ifelse(_islower_a(ch), ASCIIChr(ch - 0x20), ch)
titlecase(ch::ASCIIChr) = uppercase(ch)

function uppercase_first(str::MaybeSub{S}) where {C<:ASCIICSE,S<:Str{C}}
    (len = ncodeunits(str)) == 0 && return str
    @preserve str begin
        pnt = pointer(str)
        ch = get_codeunit(pnt)
        _islower_a(ch) || return str
        out = _allocate(len)
        unsafe_copyto!(out, pnt, len)
        set_codeunit!(out, ch - 0x20)
        Str(C, out)
    end
end

function lowercase_first(str::MaybeSub{S}) where {C<:ASCIICSE,S<:Str{C}}
    (len = ncodeunits(str)) == 0 && return str
    @preserve str begin
        pnt = pointer(str)
        ch = get_codeunit(pnt)
        _isupper_a(ch) || return str
        out = _allocate(len)
        unsafe_copyto!(out, pnt, len)
        set_codeunit!(out, ch + 0x20)
        Str(C, out)
    end
end

function _upper(::Type{C}, beg::Ptr{UInt8}, off, len) where {C<:ASCIICSE}
    buf, out = _allocate(UInt8, len)
    fin = out + len
    unsafe_copyto!(out, beg, len)
    out += off
    while out < fin
        ch = get_codeunit(out)
        _islower_a(ch) && set_codeunit!(out, ch - 0x20)
        out += 1
    end
    Str(C, buf)
end

function _lower(::Type{C}, beg::Ptr{UInt8}, off, len) where {C<:ASCIICSE}
    buf, out = _allocate(UInt8, len)
    fin = out + len
    unsafe_copyto!(out, beg, len)
    out += off
    while out < fin
        ch = get_codeunit(out)
        _isupper_a(ch) && set_codeunit!(out, ch + 0x20)
        out += 1
    end
    Str(C, buf)
end

function _upper(::Type{C}, beg::Ptr{UInt8}, off, len) where {C<:LatinCSE}
    buf, out = _allocate(UInt8, len)
    fin = out + len
    unsafe_copyto!(out, beg, len)
    out += off
    while out < fin
        ch = get_codeunit(out)
        _can_upper(ch) && set_codeunit!(out, ch - 0x20)
        out += 1
    end
    Str(C, buf)
end

function uppercase(str::MaybeSub{S}) where {C<:ASCIICSE,S<:Str{C}}
    (len = ncodeunits(str)) == 0 && return str
    @preserve str begin
        pnt = beg = pointer(str)
        fin = beg + len
        while pnt < fin
            _islower_a(get_codeunit(pnt)) && return _upper(C, beg, pnt-beg, len)
            pnt += 1
        end
    end
    str
end

function lowercase(str::MaybeSub{S}) where {C<:ASCIICSE,S<:Str{C}}
    (len = ncodeunits(str)) == 0 && return str
    @preserve str begin
        pnt = beg = pointer(str)
        fin = beg + len
        while pnt < fin
            _isupper_a(get_codeunit(pnt)) && return _lower(C, beg, pnt-beg, len)
            pnt += 1
        end
    end
    str
end

function uppercase_first(str::MaybeSub{S}) where {C<:LatinCSE,S<:Str{C}}
    (len = ncodeunits(str)) == 0 && return str
    @preserve str begin
        pnt = pointer(str)
        ch = get_codeunit(pnt)
        _can_upper(ch) || return str
        buf, out = _allocate(UInt8, len)
        set_codeunit!(out, ch - 0x20)
        len > 1 && unsafe_copyto!(out, pnt+1, len-1)
        Str(C, buf)
    end
end

# Special handling for characters that can't map into Latin1
function uppercase_first(str::MaybeSub{S}) where {C<:_LatinCSE,S<:Str{C}}
    (len = ncodeunits(str)) == 0 && return str
    @preserve str begin
        pnt = pointer(str)
        ch = get_codeunit(pnt)
        if _can_upper(ch)
            buf, out8 = _allocate(UInt8, len)
            set_codeunit!(out8, ch - 0x20)
            len > 1 && unsafe_copyto!(out8, pnt+1, len-1)
            Str(C, buf)
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
end

function lowercase_first(str::MaybeSub{S}) where {C<:Latin_CSEs,S<:Str{C}}
    (len = ncodeunits(str)) == 0 && return str
    @preserve str begin
        pnt = pointer(str)
        ch = get_codeunit(pnt)
        _isupper(ch) || return str
        buf, out = _allocate(UInt8, len)
        set_codeunit!(out, ch + 0x20)
        len > 1 && unsafe_copyto!(out, pnt+1, len-1)
        Str(C, buf)
    end
end

lowercase(ch::T) where {T<:LatinChars} = T(_lowercase_l(codepoint(ch)))
uppercase(ch::LatinChr) = LatinChr(_uppercase_l(codepoint(ch)))

# Special handling for case where this is just an optimization of the first 256 bytes of Unicode,
# and not the 8-bit ISO 8859-1 character set
function uppercase(ch::_LatinChr)
    cb = codepoint(ch)
    _can_upper(cb) && return _LatinChr(cb - 0x20)
    # We don't uppercase 0xdf, the ß character
    cb == 0xb5 ? UCS2Chr(0x39c) : (cb == 0xff ? UCS2Chr(0x178) : ch)
end
titlecase(ch::LatinChars) = uppercase(ch)

function _upper(::Type{C}, beg::Ptr{UInt8}, off, len) where {C<:_LatinCSE}
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
    Str(C, buf)
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
    fin = bytoff(out, len)
    cur = beg + off
    _widen!(out, beg, cur)
    out = bytoff(out, off)
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

function uppercase(str::MaybeSub{S}) where {C<:LatinCSE,S<:Str{C}}
    (len = ncodeunits(str)) == 0 && return str
    @preserve str begin
        pnt = beg = pointer(str)
        fin = beg + len
        while pnt < fin
            _can_upper(get_codeunit(pnt)) && return _upper(C, beg, pnt-beg, len)
            pnt += 1
        end
    end
    str
end

function uppercase(str::MaybeSub{S}) where {C<:_LatinCSE,S<:Str{C}}
    (len = ncodeunits(str)) == 0 && return str
    @preserve str begin
        pnt = beg = pointer(str)
        fin = beg + len
        while pnt < fin
            ch = get_codeunit(pnt)
            ((ch == 0xb5) | (ch == 0xff)) && return _widenupper(beg, pnt-beg, len)
            _can_upper(ch) && return _upper(C, beg, pnt-beg, len)
            pnt += 1
        end
    end
    str
end

function _lower(::Type{C}, beg::Ptr{UInt8}, off, len) where {C<:Latin_CSEs}
    buf, out = _allocate(UInt8, len)
    fin = out + len
    unsafe_copyto!(out, beg, len)
    out += off
    while out < fin
        ch = get_codeunit(out)
        _isupper_al(ch) && set_codeunit!(out, ch + 0x20)
        out += 1
    end
    Str(C, buf)
end

function lowercase(str::MaybeSub{S}) where {C<:Latin_CSEs,S<:Str{C}}
    (len = ncodeunits(str)) == 0 && return str
    @preserve str begin
        pnt = beg = pointer(str)
        fin = beg + len
        while pnt < fin
            _isupper_al(get_codeunit(pnt)) && return _lower(C, beg, pnt-beg, len)
            pnt += 1
        end
    end
    str
end

_can_upper_latin(ch)      = _can_upper(ch) | (ch == 0xb5) | (ch == 0xff)
_can_upper_only_latin(ch) = _can_upper_l(ch) | (ch == 0xb5) | (ch == 0xff)

@inline _can_upper_ch(ch) =
    ch <= 0x7f ? _islower_a(ch) : (ch <= 0xff ? _can_upper_only_latin(ch) : _islower_u(ch))
@inline _can_lower_ch(ch) =
    ch <= 0x7f ? _isupper_a(ch) : (ch <= 0xff ? _isupper_l(ch) : _isupper_u(ch))

# result must have at least one character > 0xff, so if the only character(s)
# > 0xff became <= 0xff, then the result may need to be narrowed and returned as _LatinStr

function _lower(::Type{C}, beg, off, len) where {C<:_UCS2CSE}
    CU = codeunit(C)
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
    if flg && is_latin(buf)
        out = pointer(buf)
        buf = _allocate(len)
        _narrow!(pointer(buf), out, out + len)
        Str(_LatinCSE, buf)
    else
        Str(C, buf)
    end
end

function _lower(::Type{C}, beg, off, len) where {C<:Union{UCS2CSE,UTF32_CSEs}}
    CU = codeunit(C)
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
    Str(C, buf)
end

function lowercase(str::MaybeSub{S}) where {C<:Union{UCS2_CSEs,UTF32_CSEs},S<:Str{C}}
    @preserve str begin
        CU = codeunit(C)
        pnt = beg = pointer(str)
        fin = beg + sizeof(str)
        while pnt < fin
            _can_lower_ch(get_codeunit(pnt)) && return _lower(C, beg, pnt-beg, ncodeunits(str))
            pnt += sizeof(CU)
        end
    end
    str
end

function _upper(::Type{C}, beg, off, len) where {C<:Union{UCS2_CSEs,UTF32_CSEs}}
    CU = codeunit(C)
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
    Str(C, buf)
end

function uppercase(str::MaybeSub{S}) where {C<:Union{UCS2_CSEs,UTF32_CSEs},S<:Str{C}}
    @preserve str begin
        CU = codeunit(C)
        pnt = beg = pointer(str)
        fin = beg + sizeof(str)
        while pnt < fin
            _can_upper_ch(get_codeunit(pnt)) && return _upper(C, beg, pnt-beg, ncodeunits(str))
            pnt += sizeof(CU)
        end
        str
    end
end
