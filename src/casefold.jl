#=
Case folding for Unicode Str types

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

_lowercase_l(ch) = ifelse(_isupper_a(ch) | _isupper_l(ch), ch + 0x20, ch)
_uppercase_l(ch) = ifelse(_islower_a(ch) | _islatin_l(ch), ch - 0x20, ch)

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
    dat = _data(str)
    (isempty(dat) || !_islower_a(dat[1])) && return str
    out = copy(dat)
    out[1] -= 32
    ASCIIStr(out)
end

function lcfirst(str::ASCIIStr)
    dat = _data(str)
    (isempty(dat) || !_isupper_a(dat[1])) && return str
    out = copy(dat)
    out[1] += 32
    ASCIIStr(out)
end

function _upper(::Type{ASCIIStr}, dat, i, len)
    out = copy(dat)
    @inbounds for j = i:len
        _islower_a(out[j]) && (out[j] -= 32)
    end
    ASCIIStr(out)
end

function uppercase(str::ASCIIStr)
    println("uppercase(::ASCIIStr)")
    len, dat = _lendata(str)
    @inbounds for i = 1:len
        _islower_a(dat[i]) && return _upper(ASCIIStr, dat, i, len)
    end
    str
end

function _lower(::Type{ASCIIStr}, dat, i, len)
    out = copy(dat)
    @inbounds for j = i:len
        _isupper_a(out[j]) && (out[j] += 32)
    end
    ASCIIStr(out)
end

function lowercase(str::ASCIIStr)
    println("lowercase(::ASCIIStr)")
    len, dat = _lendata(s)
    for i = 1:len
        _isupper_a(dat[i]) && return _lower(ASCIIStr, dat, i, len)
    end
    str
end

function ucfirst(str::LatinStr)
    dat = _data(str)
    isempty(dat) && return str
    ch = get_codeunit(dat, 1)%LatinChr
    if islower(ch%LatinChr)
        out = copy(dat)
        out[1] = ch - 32
        LatinStr(out)
    else
        str
    end
end

# Special handling for characters that can't map into Latin1
function ucfirst(str::_LatinStr)
    dat = _data(str)
    isempty(dat) && return str
    @inbounds ch = dat[1]
    if islower(ch%LatinChr)
        @inbounds out = copy(dat)
        @inbounds out[1] = ch - 32
        T(out)
    elseif (ch == 0xb5) | (ch == 0xff)
        buf, pnt = _allocate(UInt16, len)
        set_codeunit!(pnt, 1, ifelse(ch == 0xb5, 0x39c, 0x178))
        @inbounds for i = 2:len ; set_codeunit!(pnt, i, dat[i]%UInt16) ; end
        _UCS2Str(buf)
    else
        str
    end
end

function lcfirst(str::T) where {T<:LatinStrings}
    dat = _data(str)
    (isempty(dat) || !isupper(dat[1]%LatinChr)) && return str
    @inbounds out = copy(dat)
    @inbounds out[1] += 32
    T(out)
end

lowercase(ch::T) where {T<:LatinChars} = T(_lowercase_l(tobase(ch)))
uppercase(ch::LatinChr) = LatinChr(_uppercase_l(tobase(ch)))

# Special handling for case where this is just an optimization of the first 256 bytes of Unicode,
# and not the 8-bit ISO 8859-1 character set
function uppercase(ch::_LatinChr)
    islower(ch%LatinChr) && return _LatinChr(ch + 0x20)
    ch == 0xb5 ? UCS2Chr(0x39c) : (ch == 0xff ? UCS2Chr(0x178) : ch)
end
titlecase(ch::LatinChars) = uppercase(ch)

function _upper(::Type{LatinStr}, dat, i, len)
    out = copy(dat)
    @inbounds for j = i:len
        islower(out[j]%LatinChr) && (out[j] -= 32)
    end
    LatinStr(out)
end

function _upper(::Type{_LatinStr}, dat, i, len)
    # Need to scan the rest of the string to see if _widenupper needs to be called
    @inbounds begin
        for j = i:len
            ((ch = dat[j]) == 0xb5) | (ch == 0xff) && return _widenupper(dat, i, len)
        end
        out = copy(dat)
        for j = i:len
            islower(out[j]%LatinChr) && (out[j] -= 32)
        end
    end
    _LatinStr(out)
end

function _widenupper(dat, i, len)
    buf, pnt = _allocate(UInt16, len)
    @inbounds for j = 1:i-1
        set_codeunit!(pnt, j, dat[j])
    end
    @inbounds for j = i:len
        ch = dat[j]%UInt8
        if ch == 0xb5
            set_codeunit!(pnt, j, 0x39c)
        elseif ch == 0xff
            set_codeunit!(pnt, j, 0x178)
        else
            set_codeunit!(pnt, j, islower(ch%LatinChr) ? ch - 0x20 : ch)
        end
    end
    _UCS2Str(buf)
end

function uppercase(str::LatinStr)
    println("uppercase(::LatinStr)")
    len, dat = _lendata(str)
    @inbounds for i = 1:len
        islower(dat[1]%LatinChr) && return _upper(LatinStr, dat, i, len)
    end
    str
end

function uppercase(str::_LatinStr)
    println("uppercase(::_LatinStr)")
    len, dat = _lendata(str)
    @inbounds for i = 1:len
        ch = dat[i]
        ((ch == 0xb5) | (ch == 0xff)) && return _widenupper(dat, i, len)
        islower(ch%LatinChr) && return _upper(_LatinStr, dat, i, len)
    end
    str
end

function _lower(::Type{T}, dat, i) where {T<:LatinStrings}
    out = copy(dat)
    @inbounds for j = i:length(out)
        isupper(out[j]%LatinChr) && (out[j] += 32)
    end
    T(out)
end

function lowercase(str::T) where {T<:LatinStrings}
    println("lowercase(::$T)")
    dat = _data(str)
    for i = 1:_len(str)
        isupper(dat[i]%LatinChr) && return _lower(T, dat, i)
    end
    str
end

@inline _islower_lu(ch) =  _islower_l(ch) | (ch == 0xb5) | (ch == 0xff)

function _lower!(out::Ptr{T}, fin::Ptr{T}) where {T}
    while out < fin
        ch = get_codeunit(out)
        if islatin(ch)
            (_isupper_a(ch) | _isupper_l(ch)) && set_codeunit!(out, ch += 0x20)
        elseif _check_mask(ch, Uni.LU, Uni.LT)
            set_codeunit!(out, _lowercase_u(ch))
        end
        out += sizeof(T)
    end
end

function _lower(::Type{T}, beg, pnt, fin, len) where {T<:UCS2Strings}
    buf, out = _allocate(UInt16, len)
    unsafe_copyto!(out, beg, len)
    _lower!(out + (pnt - beg), fin)
    T(buf)
end

function _lower(::Type{T}, beg, pnt, fin, len) where {T<:UTF32Strings}
    buf, out = _allocate(UInt32, len)
    unsafe_copyto!(out, beg, len)
    _lower!(out + (pnt - beg), fin)
    T(buf)
end

# Placeholders until I write some optimal code for these
lowercase(str::UTF8Str) = UTF8Str(lowercase(String(str.data)))
uppercase(str::UTF8Str) = UTF8Str(uppercase(String(str.data)))

function lowercase(str::UCS2Str)
    println("lowercase(::UCS2Str)")
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        (islatin(ch) ? (_isupper_a(ch) || _isupper_l(ch)) : isupper(ch)) &&
            return _lower(UCS2Str, beg, pnt, fin, _len(str))
        pnt += 2
    end
    str
end

function _upper!(out::Ptr{T}, fin::Ptr{T}) where {T}
    while out < fin
        ch = get_codeunit(out)
        if islatin(ch)
            if _islower_a(ch) | _islower_l(ch)
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
    _upper!(out + (pnt - beg), fin)
    T(buf)
end

function _upper(::Type{T}, beg, pnt, fin, len) where {T<:UTF32Strings}
    buf, out = _allocate(UInt32, len)
    unsafe_copyto!(out, beg, len)
    _upper!(out + (pnt - beg), fin)
    T(buf)
end

function uppercase(str::UCS2Str)
    println("uppercase(::UCS2Str)")
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        (isascii(ch) ? _islower_a(ch) : (islatin(ch) ? _islower_lu(ch) : _islower_u(ch))) &&
            return _upper(UCS2Str, beg, pnt, fin, _len(str))
        pnt += 2
    end
    str
end

function uppercase(str::T) where {T<:UTF32Strings}
    println("uppercase(::$T)")
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        (isascii(ch) ? _islower_a(ch) : (islatin(ch) ? _islower_lu(ch) : _islower_u(ch))) &&
            return _upper(T, beg, pnt, fin, _len(str))
        pnt += 4
    end
    str
end

function lowercase(str::T) where {T<:UTF32Strings}
    println("lowercase(::$T)")
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        (islatin(ch) ? (_isupper_a(ch) | _isupper_l(ch)) : _isupper_u(ch)) &&
            return _lower(T, beg, pnt, fin, _len(str))
        pnt += 4
    end
    str
end

# result must have at least one character > 0xff, so if the only character(s)
# > 0xff became <= 0xff, then the result may need to be narrowed and returned as _LatinStr
function lowercase(str::_UCS2Str)
    println("lowercase(::_UCS2Str)")
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        # Todo, return as _LatinStr if necessary
        (islatin(ch) ? (_isupper_a(ch) | _isupper_l(ch)) : _isupper_u(ch)) &&
            return _lower(_UCS2Str, beg, pnt, fin, _len(str))
        pnt += 2
    end
    str
end

# characters 0xb5 and 0xff get treated specially
function uppercase(str::_UCS2Str)
    println("uppercase(::_UCS2Str)")
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        (islatin(ch) ? (_islower_a(ch) | _islower_lu(ch)) : _islower_u(ch)) &&
            return _upper(_UCS2Str, beg, pnt, fin, _len(str))
        pnt += 2
    end
    str
end

# These are more complex, and maybe belong in a separate UTF16Str.jl package

function _lower(::Type{UTF16Str}, beg, pnt, fin, len)
    buf, out = _allocate(UInt16, len)
    unsafe_copyto!(out, beg, len)
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
    UTF16Str(buf)
end

function lowercase(str::UTF16Str)
    println("lowercase(::UTF16Str)")
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        (ch > 0xd7ff # May be surrogate pair
         ? _is_upper_u(ch > 0xdfff ? get_supplementary(ch, get_codeunit(pnt += 2)) : ch%UInt32)
         : (isascii(ch) ? _isupper_a(ch) : (islatin(ch) ? _isupper_l(ch) : _isupper_u(ch)))) &&
             return _lower(UTF16Str, beg, pnt, fin, _len(str))
        pnt += 2
    end
    str
end

function _upper(::Type{UTF16Str}, beg, pnt, fin, len)
    buf, out = _allocate(UInt16, len)
    unsafe_copyto!(out, beg, len)
    out += (pnt - beg)
    while out < fin
        ch = get_codeunit(out)
        if islatin(ch)
            if _islower_a(ch) | _islower_l(ch)
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
    UTF16Str(buf)
end

function uppercase(str::UTF16Str)
    println("uppercase(::UTF16Str)")
    pnt = beg = _pnt(str)
    fin = beg + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)
        (ch > 0xd7ff # May be surrogate pair
         ? _is_lower_u(ch > 0xdfff ? get_supplementary(ch, get_codeunit(pnt += 2)) : ch%UInt32)
         : (isascii(ch) ? _islower_a(ch) : (islatin(ch) ? _islower_lu(ch) : _islower_u(ch)))) &&
             return _upper(UTF16Str, beg, pnt, fin, _len(str))
        pnt += 2
    end
    str
end
