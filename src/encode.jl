#=
Constructors for Str strings

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#
function _str(str::T) where {T<:Union{Vector{UInt8}, BinaryStr, RawByteStr, String}}
    siz, dat = _lendata(str)
    # handle zero length string quickly
    siz == 0 && return empty_ascii
    len, flags, num4byte, num3byte, num2byte, latin1byte = unsafe_checkstring(dat, 1, siz)
    if flags == 0
        buf = _allocate(len)
        if T == Vector{UInt8}
            unsafe_copyto!(buf, 1, dat, 1, len)
        else
            @inbounds for i = 1:len; set_codeunit!(buf, i, get_codeunit(dat, i)); end
        end
        ASCIIStr(buf)
    elseif num4byte != 0
        _UTF32Str(_encode(UInt32, dat, len))
    elseif num3byte + num2byte != 0
        _UCS2Str(_encode(UInt16, dat, len))
    else
        buf = _allocate(len)
        out = pos = 0
        @inbounds while out < len
            ch8 = get_codeunit(dat, pos += 1)
            buf[out += 1] = (ch8 <= 0x7f
                             ? ch8
                             : (((ch8 & 3) << 6) | (get_codeunit(dat, pos += 1) & 0x3f)))
        end
        latin1byte == 0 ? ASCIIStr(buf) : _LatinStr(buf)
    end
end

Str(dat) = _str(dat)
UniStr(dat) = _str(dat)

"""Convert to a UniStr if valid Unicode, otherwise return a RawByteStr"""
function unsafe_str(str::T;
                    accept_long_null  = false,
                    accept_surrogates = false,
                    accept_long_char  = false
                    accept_invalids   = true
                    ) where {T <: Union{Vector{UInt8}, BinaryStr, RawByteStr, String}}
    siz, dat = _lendata(str)
    # handle zero length string quickly
    siz == 0 && return empty_ascii
    len, flags, num4byte, num3byte, num2byte, latin1byte, invalids =
        unsafe_checkstring(dat, 1, siz;
                           accept_long_null  = accept_long_null,
                           accept_surrogates = accept_surrogates,
                           accept_long_char  = accept_long_char,
                           accept_invalids   = accept_invalids)
    if invalids != 0
        RawByteStr(dat)
    elseif flags == 0
        # Don't allow this to be aliased to a mutable Vector{UInt8}
        T == Vector{UInt8} && (dat = unsafe_copyto!(_allocate(siz), 1, dat, 1, siz))
        ASCIIStr(dat)
    elseif num4byte != 0
        _UTF32Str(_encode(UInt32, dat, len))
    elseif num2byte + num3byte != 0
        _UCS2Str(_encode(UInt16, dat, len))
    else
        buf = _allocate(len)
        out = pos = 0
        @inbounds while out < len
            ch8 = get_codeunit(dat, pos += 1)
            buf[out += 1] = ch8 <= 0x7f ? ch8 : (((ch8 & 3) << 6) | (dat[pos += 1] & 0x3f))
        end
        latin1byte == 0 ? ASCIIStr(buf) : _LatinStr(buf)
    end
end

"""Convert to a UniStr if valid Unicode, otherwise return a RawCharStr"""
function unsafe_str(str::T;
                    accept_long_null  = false,
                    accept_surrogates = false,
                    accept_long_char  = false
                    accept_invalids   = true
                    ) where {T<:Union{AbstractString,AbstractVector{<:Union{Char,UInt16,UInt32}}}}
    siz = length(str)
    # handle zero length string quickly
    siz == 0 && return empty_ascii
    len, flags, num4byte, num3byte, num2byte, latin1, invalids =
        unsafe_checkstring(str, 1, siz;
                           accept_long_null  = accept_long_null,
                           accept_surrogates = accept_surrogates,
                           accept_long_char  = accept_long_char,
                           accept_invalids   = accept_invalids)
    if flags == 0
        ASCIIStr(unsafe_copyto!(_allocate(siz), 1, str, 1, siz))
    elseif invalids
        if eltype(T) == Char
            buf, pnt = _allocate(UInt32, siz)
            for (i, ch) in enumerate(str)
                set_codeunit!(pnt, i, UInt32(ch))
            end
            RawCharStr(buf)
        else
            buf, pnt = _allocate(eltype(T), siz)
            for (i, ch) in enumerate(str)
                set_codeunit!(pnt, i, ch)
            end
            T == UInt32 ? RawCharStr(buf) : RawWordStr(buf)
        end
    elseif num4byte != 0
        _UTF32Str(_encode(UInt32, dat, len))
    elseif num2byte + num3byte != 0
        _UCS2Str(_encode(UInt16, dat, len))
    else
        buf = _allocate(len)
        out = pos = 0
        @inbounds while out < len
            ch8 = dat[pos += 1]
            buf[out += 1] = ch8 <= 0x7f ? ch8 : (((ch8 & 3) << 6) | (dat[pos += 1] & 0x3f))
        end
        latin1byte == 0 ? ASCIIStr(buf) : _LatinStr(buf)
    end
end

"""Encode a String as a Str, picking the best representation"""
Str(str::String) = unsafe_str(_data(str))
