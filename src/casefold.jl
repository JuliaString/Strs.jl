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

function _upper(::Type{ASCIIStr}, dat, i, len)
    out = copy(dat)
    @inbounds for j = i:len
        _islower_a(out[j]) && (out[j] -= 0x20)
    end
    Str(ASCIICSE, out)
end

function uppercase(str::ASCIIStr)
    len = _len(str)
    @inbounds for i = 1:len
        _islower_a(str[i]) && return _upper(ASCIIStr, str, i, len)
    end
    str
end

function _lower(::Type{ASCIIStr}, dat, i, len)
    out = copy(dat)
    @inbounds for j = i:len
        _isupper_a(out[j]) && (out[j] += 0x20)
    end
    Str(ASCIICSE, out)
end

function lowercase(str::ASCIIStr)
    @inbounds for i = 1:len
        _islower_a(str[i]) && return _upper(ASCIIStr, str, i, len)
    end
    str
end

function ucfirst(str::LatinStr)
    dat = _data(str)
    isempty(dat) && return str
    ch = get_codeunit(dat)
    if _can_upper(ch)
        out = copy(dat)
        out[1] = ch - 0x20
        Str(LatinCSE, out)
    else
        str
    end
end

# Special handling for characters that can't map into Latin1
function ucfirst(str::_LatinStr)
    (len = _len(str)) == 0 && return str
    pnt = _pnt(str)
    @inbounds ch = tobase(get_codeunit(pnt))
    if _can_upper(ch)
        out8 = _allocate(len)
        unsafe_copyto!(pointer(out8), pnt, len)
        set_codeunit!(out8, ch - 0x20)
        Str(_LatinStr, out8)
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
    dat = _data(str)
    (isempty(dat) || !isupper(get_codeunit(dat))) && return str
    @inbounds out = copy(dat)
    @inbounds out[1] += 0x20
    Str(cse(T), out)
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

function _upper(::Type{LatinStr}, dat, i, len)
    out = copy(dat)
    @inbounds for j = i:len
        _can_upper(out[j]) && (out[j] -= 0x20)
    end
    Str(LatinCSE, out)
end

function _upper(::Type{_LatinStr}, dat, i, len)
    # Need to scan the rest of the string to see if _widenupper needs to be called
    @inbounds begin
        for j = i:len
            ((ch = dat[j]) == 0xb5) | (ch == 0xff) && return _widenupper(dat, i, len)
        end
        out = copy(dat)
        for j = i:len
            _can_upper(out[j]) && (out[j] -= 0x20)
        end
    end
    Str(_LatinCSE, out)
end

function _widenupper(dat, i, len)
    buf, pnt = _allocate(UInt16, len)
    @inbounds for j = 1:i-1
        set_codeunit!(pnt, j, get_codeunit(dat, j))
    end
    @inbounds for j = i:len
        ch = get_codeunit(dat, j)
        if ch == 0xb5
            set_codeunit!(pnt, j, 0x39c)
        elseif ch == 0xff
            set_codeunit!(pnt, j, 0x178)
        else
            set_codeunit!(pnt, j, _can_upper(ch) ? ch - 0x20 : ch)
        end
    end
    Str(_UCS2CSE, buf)
end

function uppercase(str::LatinStr)
    len = _len(str)
    dat = _data(str)
    @inbounds for i = 1:len
        _can_upper(get_codeunit(dat, i)) && return _upper(LatinStr, dat, i, len)
    end
    str
end

function uppercase(str::_LatinStr)
    len = _len(str)
    dat = _data(str)
    @inbounds for i = 1:len
        ch = get_codeunit(dat, i)
        ((ch == 0xb5) | (ch == 0xff)) && return _widenupper(dat, i, len)
        _can_upper(ch) && return _upper(_LatinStr, dat, i, len)
    end
    str
end

function _lower(::Type{T}, dat, i) where {T<:LatinStrings}
    out = copy(dat)
    @inbounds for j = i:length(out)
        _isupper_al(out[j]) && (out[j] += 0x20)
    end
    Str(cse(T), out)
end

function lowercase(str::T) where {T<:LatinStrings}
    dat = _data(str)
    for i = 1:_len(str)
        _isupper_al(dat[i]) && return _lower(T, dat, i)
    end
    str
end

@inline _can_upper_ch(ch) =
    islatin(ch) ? (_can_upper(ch) | (ch == 0xb5) | (ch == 0xff)) : _islower_u(ch)

function _lower!(out::Ptr{T}, fin::Ptr{T}) where {T}
    while out < fin
        ch = get_codeunit(out)
        if islatin(ch)
            _isupper_al(ch) && set_codeunit!(out, ch += 0x20)
        elseif _check_mask(ch, Uni.LU, Uni.LT)
            set_codeunit!(out, _lowercase_u(ch))
        end
        out += sizeof(T)
    end
end

function _lower(::Type{T}, beg, pnt, len) where {T<:UCS2Strings}
    buf, out = _allocate(UInt16, len)
    unsafe_copyto!(out, beg, len)
    _lower!(out + (pnt - beg), out + (len<<1))
    Str(cse(T), buf)
end

function _lower(::Type{T}, beg, pnt, len) where {T<:UTF32Strings}
    buf, out = _allocate(UInt32, len)
    unsafe_copyto!(out, beg, len)
    _lower!(out + (pnt - beg), out + (len<<2))
    Str(cse(T), buf)
end

# Placeholders until I write some optimal code for these
function lowercase(str::UTF8Str)
    Str(UTF8CSE, _data(lowercase(String(str.data))))
end

function uppercase(str::UTF8Str)
    Str(UTF8CSE, _data(uppercase(String(str.data))))
end

function lowercase(str::UCS2Str)
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        (islatin(ch) ? _isupper_al(ch) : isupper(ch)) &&
            return _lower(UCS2Str, beg, pnt, _len(str))
        pnt += 2
    end
    str
end

function _upper!(out::Ptr{T}, fin::Ptr{T}) where {T}
    while out < fin
        ch = get_codeunit(out)
        if islatin(ch)
            if _can_upper(ch)
                set_codeunit!(out, ch -= 0x20)
            elseif ch == 0xb5
                set_codeunit!(out, 0x39c)
            elseif ch == 0xff
                set_codeunit!(out, 0x178)
            end
        elseif _cat(ch) == Uni.LL
            set_codeunit!(out, _uppercase_u(ch))
        end
        out += sizeof(T)
    end
end

function _upper(::Type{T}, beg, pnt, fin, len) where {T<:UCS2Strings}
    buf, out = _allocate(UInt16, len)
    unsafe_copyto!(out, beg, len)
    _upper!(out + (pnt - beg), out + (len<<1))
    Str(cse(T), buf)
end

function _upper(::Type{T}, beg, pnt, fin, len) where {T<:UTF32Strings}
    buf, out = _allocate(UInt32, len)
    unsafe_copyto!(out, beg, len)
    _upper!(out + (pnt - beg), out + (len<<2))
    Str(cse(T), buf)
end

function uppercase(str::UCS2Str)
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        (isascii(ch) ? _islower_a(ch) : (islatin(ch) ? _islower_l(ch) : _islower_u(ch))) &&
            return _upper(UCS2Str, beg, pnt, _len(str))
        pnt += 2
    end
    str
end

function uppercase(str::T) where {T<:UTF32Strings}
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        _can_upper_ch(get_codeunit(pnt)) && return _upper(T, beg, pnt, _len(str))
        pnt += 4
    end
    str
end

function lowercase(str::T) where {T<:UTF32Strings}
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        (islatin(ch) ? _isupper_al(ch) : _isupper_u(ch)) &&
            return _lower(T, beg, pnt, _len(str))
        pnt += 4
    end
    str
end

# result must have at least one character > 0xff, so if the only character(s)
# > 0xff became <= 0xff, then the result may need to be narrowed and returned as _LatinStr
function lowercase(str::_UCS2Str)
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        # Todo, return as _LatinStr if necessary
        (islatin(ch) ? (_isupper_a(ch) | _isupper_l(ch)) : _isupper_u(ch)) &&
            return _lower(_UCS2Str, beg, pnt, _len(str))
        pnt += 2
    end
    str
end

# characters 0xb5 and 0xff get treated specially
function uppercase(str::_UCS2Str)
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        _can_upper_ch(get_codeunit(pnt)) && return _upper(_UCS2Str, beg, pnt, _len(str))
        pnt += 2
    end
    str
end

# These are more complex, and maybe belong in a separate UTF16Str.jl package

function _lower(::Type{UTF16Str}, beg, pnt, len)
    buf, out = _allocate(UInt16, len)
    unsafe_copyto!(out, beg, len)
    fin = out + (len<<1)
    out += (pnt - beg)
    while out < fin
        ch = get_codeunit(out)
        if islatin(ch)
            (_isupper_a(ch) | _isupper_l(ch)) && set_codeunit!(out, ch += 0x20)
        elseif is_surrogate_trail(ch)
            # pick up previous code unit (lead surrogate)
            cp = get_supplementary(get_codeunit(out - 2), ch)
            if _check_mask(cp, Uni.LU, Uni.LT)
                cp = _lowercase_u(cp)
                set_codeunit!(out - 2, (0xd7c0 + (cp >>> 10))%UInt16)
                set_codeunit!(out,     (0xdc00 + (cp & 0x3ff))%UInt16)
            end
        elseif _check_mask(ch, Uni.LU, Uni.LT)
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
        (ch > 0xd7ff # May be surrogate pair
         ? _is_upper_u(ch > 0xdfff ? ch%UInt32 : get_supplementary(ch, get_codeunit(pnt += 2)))
         : (isascii(ch) ? _isupper_a(ch) : (islatin(ch) ? _isupper_l(ch) : _isupper_u(ch)))) &&
             return _lower(UTF16Str, beg, pnt, _len(str))
        pnt += 2
    end
    str
end

function _upper(::Type{UTF16Str}, beg, pnt, len)
    buf, out = _allocate(UInt16, len)
    unsafe_copyto!(out, beg, len)
    fin = out + (len<<1)
    out += (pnt - beg)
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
        (ch > 0xd7ff # May be surrogate pair
         ? _is_lower_u(ch > 0xdfff ? ch%UInt32 : get_supplementary(ch, get_codeunit(pnt += 2)))
         : _can_upper_ch(ch)) &&
             return _upper(UTF16Str, beg, pnt, _len(str))
        pnt += 2
    end
    str
end
