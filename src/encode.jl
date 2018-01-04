#=
Constructors for Str strings

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#
function _str(str::T) where {T<:Union{Vector{UInt8}, BinaryStr, Text1Str, String}}
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_ascii
    dat = _data(str)
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

function _str_cpy(::Type{T}, str, len) where {T}
    buf, pnt = _allocate(T, len)
    @inbounds for ch in str
        set_codeunit!(pnt, ch%T)
        pnt += sizeof(T)
    end
    buf
end

"""Encode as a possibly smaller type"""
function _str_encode(str::T, len, flags) where {T}
    if (flags & ~UTF_LATIN1) != 0
        buf = codeunit(T) == UInt8 ? _data(str) : _str_cpy(UInt8, str, len)
        flags == 0 ? ASCIIStr(buf) : _LatinStr(buf)
    elseif (flags & UTF_UNICODE4) == 0
        _UCS2Str(codeunit(T) == UInt16 ? _data(str) : _str_cpy(UInt16, str, len))
    else
        _UTF32Str(codeunit(T) == UInt32 ? _data(str) : _str_cpy(UInt32, str, len))
    end
end

function convert(::Type{Str}, str::AbstractString)
    # handle zero length string quickly
    isempty(str) && return empty_ascii
    len, flags = unsafe_checkstring(str)
    str_encode(str, len, flags)
end
convert(::Type{Str}, str::String) = _str(str)
convert(::Type{Str}, str::T) where {T<:Str} = str

convert(::Type{UniStr}, str::AbstractString) = _str(str)
convert(::Type{UniStr}, str::T) where {T<:Union{ASCIIStr,_LatinStr,_UCS2Str,_UTF32Str}} = str
function convert(::Type{UniStr}, str::T) where {T<:Str}
    # handle zero length string quickly
    isempty(str) && return empty_ascii
    len, flags = count_chars(T, str, _len(str))
    str_encode(str, len, flags)
end

"""Convert to a UniStr if valid Unicode, otherwise return a Text1Str"""
function unsafe_str(str::T;
                    accept_long_null  = false,
                    accept_surrogates = false,
                    accept_long_char  = false,
                    accept_invalids   = true
                    ) where {T <: Union{Vector{UInt8}, BinaryStr, Text1Str, String}}
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_ascii
    dat = _data(str)
    len, flags, num4byte, num3byte, num2byte, latin1byte, invalids =
        unsafe_checkstring(dat, 1, siz;
                           accept_long_null  = accept_long_null,
                           accept_surrogates = accept_surrogates,
                           accept_long_char  = accept_long_char,
                           accept_invalids   = accept_invalids)
    if invalids != 0
        Text1Str(dat)
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

"""Convert to a UniStr if valid Unicode, otherwise return a Text2Str"""
function unsafe_str(str::T;
                    accept_long_null  = false,
                    accept_surrogates = false,
                    accept_long_char  = false,
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
            @inbounds for (i, ch) in enumerate(str)
                set_codeunit!(pnt, i, UInt32(ch))
            end
            Text4Str(buf)
        else
            buf, pnt = _allocate(eltype(T), siz)
            @inbounds for (i, ch) in enumerate(str)
                set_codeunit!(pnt, i, ch)
            end
            T == UInt32 ? Text4Str(buf) : Text2Str(buf)
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

# Fallback constructors for Str types, from any AbstractString
(::Type{T})(str::S) where {T<:Str, S<:AbstractString} = convert(T, str)
(::Type{T})(str::S) where {T<:Str, S<:Str} = convert(T, str)
(::Type{T})(str::T) where {T<:Str} = str
