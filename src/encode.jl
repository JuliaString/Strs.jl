#=
Constructors for Str strings

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

function _encode_ascii_latin(dat, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    pos = 0
    @inbounds while out < fin
        ch8 = dat[pos += 1]
        set_codeunit!(out, (ch8 <= 0x7f ? ch8 : (((ch8 & 3) << 6) | (dat[pos += 1] & 0x3f))))
        out += 1
    end
    buf
end

function _encode_ascii_latin(pnt::Ptr{UInt8}, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    @inbounds while out < fin
        ch8 = get_codeunit(pnt)
        set_codeunit!(out,
                      (ch8 <= 0x7f ? ch8 : (((ch8 & 3) << 6) | (get_codeunit(pnt += 1) & 0x3f))))
        pnt += 1
        out += 1
    end
    buf
end

@inline function safe_copy(::Type{Vector{T}}, ::Type{C}, str) where {T<:CodeUnitTypes,C<:CSE}
    @preserve str buf begin
        len, pnt = _lenpnt(str)
        buf, out = _allocate(T, len)
        _memcpy(out, pnt, len)
        Str(C, buf)
    end
end

@inline safe_copy(::Type{<:Str}, ::Type{C}, str) where {C<:CSE} = Str(C, str.data)
@inline safe_copy(::Type{String}, ::Type{C}, str) where {C<:CSE} = Str(C, _data(str))

function _str(str::T) where {T<:Union{Vector{UInt8}, BinaryStrings, String}}
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_ascii
    @preserve str begin
        pnt = _pnt(str)
        len, flags, num4byte, num3byte, num2byte, latin1byte = unsafe_check_string(pnt, 1, siz)
        if flags == 0
            buf, out = _allocate(UInt8, len)
            _memcpy(out, pnt, len)
            Str(ASCIICSE, buf)
        elseif num4byte != 0
            Str(_UTF32CSE, _encode_utf32(pnt, len))
        elseif num3byte + num2byte != 0
            Str(_UCS2CSE, _encode_utf16(pnt, len))
        else
            Str(latin1byte == 0 ? ASCIICSE : _LatinCSE, _encode_ascii_latin(pnt, len))
        end
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
function _str_encode(str::T, len, flags) where {T<:Str}
    if flags == 0
        Str(ASCIICSE, codeunit(T) == UInt8 ? str.data : _str_cpy(UInt8, str, len))
    elseif (flags & ~UTF_LATIN1) == 0
        Str(_LatinCSE, codeunit(T) == UInt8 ? str.data : _str_cpy(UInt8, str, len))
    elseif (flags & UTF_UNICODE4) == 0
        Str(_UCS2CSE, codeunit(T) == UInt16 ? str.data : _str_cpy(UInt16, str, len))
    else
        Str(_UTF32CSE, codeunit(T) == UInt32 ? str.data : _str_cpy(UInt32, str, len))
    end
end

function convert(::Type{<:Str}, str::AbstractString)
    # handle zero length string quickly
    is_empty(str) && return empty_ascii
    len, flags = unsafe_check_string(str)
    _str_encode(str, len, flags)
end
convert(::Type{<:Str}, str::String) = _str(str)

convert(::Type{UniStr}, str::AbstractString) = _str(str)
convert(::Type{UniStr}, str::String)         = _str(str)
convert(::Type{UniStr}, str::Str{<:Union{ASCIICSE,SubSet_CSEs}}) = str

function convert(::Type{UniStr}, str::T) where {T<:Str}
    # handle zero length string quickly
    is_empty(str) && return empty_ascii
    len, flags = count_chars(T, _pnt(str), _len(str))
    _str_encode(str, len, flags)
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
    pnt = _pnt(str)
    len, flags, num4byte, num3byte, num2byte, latin1byte, invalids =
        unsafe_check_string(pnt, 1, siz;
                            accept_long_null  = accept_long_null,
                            accept_surrogates = accept_surrogates,
                            accept_long_char  = accept_long_char,
                            accept_invalids   = accept_invalids)
    if invalids != 0
        safe_copy(T, Text1CSE, str)
    elseif flags == 0
        # Don't allow this to be aliased to a mutable Vector{UInt8}
        safe_copy(T, ASCIICSE, str)
   elseif num4byte != 0
        Str(_UTF32CSE, _encode_utf32(pnt, len))
    elseif num2byte + num3byte != 0
        Str(_UCS2CSE, _encode_utf16(pnt, len))
    else
        Str(latin1byte == 0 ? ASCIICSE : _LatinCSE, _encode_ascii_latin(_pnt(str), len))
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
        unsafe_check_string(str, 1, siz;
                            accept_long_null  = accept_long_null,
                            accept_surrogates = accept_surrogates,
                            accept_long_char  = accept_long_char,
                            accept_invalids   = accept_invalids)
    if flags == 0
        Str(ASCIICSE, _cvtsize(UInt8, str, siz))
    elseif invalids
        if eltype(T) == Char
            buf, pnt = _allocate(UInt32, siz)
            @inbounds for (i, ch) in enumerate(str)
                set_codeunit!(pnt, i, UInt32(ch))
            end
            Str(Text4CSE, buf)
        else
            buf, pnt = _allocate(eltype(T), siz)
            @inbounds for ch in str
                set_codeunit!(pnt, ch)
                pnt += sizeof(T)
            end
            Str(T == UInt32 ? Text4CSE : Text2CSE, buf)
        end
    elseif num4byte != 0
        Str(_UTF32CSE, _encode_utf32(str, len))
    elseif num2byte + num3byte != 0
        Str(_UCS2CSE, _encode_utf16(str, len))
    else
        Str(latin1byte == 0 ? ASCIICSE : _LatinCSE, _encode_ascii_latin(str, len))
    end
end

function Str(v::Vector{UInt8})
    len = length(v)
    buf = _allocate(siz)
    _memcpy(pointer(buf), pointer(v), siz)
    Str(Text1CSE, buf)
end
function Str(v::Vector{UInt16})
    len = length(v)
    buf, pnt = _allocate(UInt16, siz)
    _memcpy(pnt, pointer(v), siz)
    Str(Text2CSE, buf)
end
function Str(v::Vector{UInt32})
    len = length(v)
    buf, pnt = _allocate(UInt32, siz)
    _memcpy(pnt, pointer(v), len)
    Str(Text4CSE, buf)
end

# Fallback constructors for Str types, from any AbstractString
(::Type{T})(str::S) where {T<:Str, S<:AbstractString} = convert(T, str)
(::Type{T})(str::S) where {T<:Str, S<:Str} = convert(T, str)
(::Type{T})(str::T) where {T<:Str} = str
