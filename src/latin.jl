#=
LatinStr/_LatinStr type (ISO Latin1 8-bit subset of Unicode)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for ASCIIString that used to be in Julia
=#

## overload methods for efficiency ##

is_ascii(str::Str{C}) where {C<:_LatinCSE}    = false
is_latin(str::Str{C}) where {C<:Latin_CSEs}   = true
is_bmp(str::Str{C}) where {C<:Latin_CSEs}     = true
is_unicode(str::Str{C}) where {C<:Latin_CSEs} = true

const _UBS = Str{<:Union{ASCIICSE, Latin_CSEs}}

function string(collection::_UBS...)
    length(collection) == 1 && return collection[1]
    len = 0
    @inbounds for str in collection
        len += ncodeunits(str)
    end
    buf, pnt = _allocate(len)
    @inbounds for str in collection
        len = ncodeunits(str)
        _memcpy(pnt, pointer(str), len)
        pnt += len
    end
    Str(LatinCSE, buf)
end

## transcoding to Latin1 ##

convert(::Type{T}, s::T) where {T<:Str{<:Latin_CSEs}} = s
convert(::Type{T}, s::Str{<:_UBS}) where {T<:Str{<:Latin_CSEs}} = T(s.data)
convert(::Type{T}, s::Str{UTF8CSE}) where {T<:Str{<:Latin_CSEs}} = convert(T, s.data)

# Assumes that has already been checked for validity
function _utf8_to_latin(pnt::Ptr{UInt8}, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    while out < fin
        ch = get_codeunit(pnt)
        # Handle ASCII characters
        ch > 0x7f && (ch = ((ch & 3) << 6) | (get_codeunit(pnt += 1) & 0x3f))
        pnt += 1
        set_codeunit!(out, ch)
        out += 1
    end
    buf
end

function _latin_to_utf8(pnt::Ptr{UInt8}, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    while out < fin
        ch = get_codeunit(pnt)
        pnt += 1
        if ch > 0x7f
            set_codeunit!(out, 0xc0 | (ch >>> 6))
            ch = 0x80 | (ch & 0x3f)
            out += 1
        end
        set_codeunit!(out, ch)
        out += 1
    end
    buf
end

# Fast conversion from LatinStr or _LatinStr to UTF8Str
function convert(::Type{<:Str{UTF8CSE}}, str::Str{<:Latin_CSEs})
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_utf8
    @preserve str begin
        pnt = pointer(str)
        Str(UTF8CSE, (cnt = count_latin(len, pnt)) != 0 ? _latin_to_utf8(pnt, len + cnt) : str)
    end
end

# Fast conversion from LatinStr or _LatinStr to String
function convert(::Type{String}, str::Str{<:Latin_CSEs})
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_string
    @preserve str begin
        pnt = pointer(str)
        (cnt = count_latin(len, pnt)) != 0 ? _latin_to_utf8(pnt, len + cnt) : str.data
    end
end

function convert(::Type{<:Str{LatinCSE}}, str::MS_ByteStr)
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_latin
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, siz)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        Str(LatinCSE, flags == 0 ? _copysub(str) : _utf8_to_latin(pnt, len))
    end
end

_convert(::Type{_LatinCSE}, ch::UInt8) = _convert(ch <= 0x7f ? ASCIICSE : _LatinCSE, ch)
convert(::Type{<:Str{_LatinCSE}}, ch::Unsigned) =
    ch < 0xff ? _convert(_LatinCSE, ch%UInt8) : unierror(UTF_ERR_LATIN1, ch)

function convert(::Type{<:Str{_LatinCSE}}, str::MS_ByteStr)
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_ascii
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, siz)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        Str(latinbyte == 0 ? ASCIICSE : _LatinCSE,
            flags == 0 ? _copysub(str) : _utf8_to_latin(pnt, len))
    end
end

convert(::Type{<:Str{LatinCSE}}, a::Vector{UInt8}) = _convert(LatinStr, a)
convert(::Type{<:Str{_LatinCSE}}, a::Vector{UInt8}) =
    _convert(is_ascii(a) ? ASCIIStr : _LatinStr, a)

function convert(::Type{T}, str::AbstractString) where {T<:Str{Latin_CSEs}}
    # Might want to have invalids_as here
    len, flags = unsafe_check_string(str)
    (flags & ~(UTF_LONG | UTF_LATIN1)) == 0 || unierror(UTF_ERR_INVALID_LATIN1)
    buf, pnt = _allocate(UInt8, len)
    @inbounds for ch in str
        set_codeunit!(pnt, ch%UInt8)
        pnt += 1
    end
    Str(T, buf)
end
