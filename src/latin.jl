#=
LatinStr/_LatinStr type (ISO Latin1 8-bit subset of Unicode)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for ASCIIString that used to be in Julia
=#

## overload methods for efficiency ##

isascii(str::Str{_LatinCSE}) = false
islatin(str::LatinStrings)   = true
isbmp(str::LatinStrings)     = true
isunicode(str::LatinStrings) = true

bytestring(s::LatinStrings) = s

const _UBS = Str{<:Union{ASCIICSE, Latin_CSEs}}

function string(collection::_UBS...)
    length(c) == 1 && return collection[1]
    len = 0
    for str in collection
        len += _len(str)
    end
    buf, pnt = _allocate(len)
    for str in collection
        len = _len(str)
        _memcpy(pnt, _pnt(str), len)
        pnt += len
    end
    Str(LatinCSE, buf)
end

## outputting Latin 1 strings ##

function print(io::IO, str::LatinStrings)
    @preserve str begin
        pnt = _pnt(str)
        fin = pnt + sizeof(str)
        # Skip and write out ASCII sequences together
        while pnt < fin
            # Skip to first non-ASCII sequence
            # Todo: Optimize this to look at chunks at a time to find first non-ASCII
            beg = pnt
            ch = 0x00
            while (ch = get_codeunit(pnt)) < 0x80 && (pnt += 1) < fin ; end
            # Now we have from beg to < pnt that are ASCII
            unsafe_write(io, beg, pnt - beg)
            pnt < fin || break
            # Todo: Optimize sequences of more than one character > 0x7f
            # Write out two bytes of Latin1 character encoded as UTF-8
            _write_utf8_2(io, ch)
            pnt += 1
        end
    end
    nothing
end

_print(io, ch::UInt8) = ch <= 0x7f ? write(io, ch) : _write_utf8_2(io, ch)
print(io::IO, ch::LatinChars) = _print(io, ch%UInt8)

write(io::IO, ch::LatinChars) = write(io, ch%UInt8)

## transcoding to Latin1 ##

convert(::Type{T}, s::T) where {T<:LatinStrings} = s
convert(::Type{T}, s::Str{<:_UBS}) where {T<:LatinStrings} = T(s.data)
convert(::Type{T}, s::Str{UTF8CSE}) where {T<:LatinStrings} = convert(T, s.data)

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

function convert(::Type{LatinStr}, str::String)
    # handle zero length string quickly
    isempty(str) && return empty_latin
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte = unsafe_checkstring(str, 1, sizeof(str))
    num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
    Str(LatinCSE, flags == 0 ? str : _utf8_to_latin(_pnt(str), len))
end

_convert(::Type{_LatinCSE}, ch::UInt8) = _convert(ch <= 0x7f ? ASCIICSE : _LatinCSE, ch)
convert(::Type{<:Str{_LatinCSE}}, ch::Unsigned) =
    ch < 0xff ? _convert(_LatinCSE, ch%UInt8) : unierror(UTF_ERR_LATIN1, ch)

function convert(::Type{_LatinStr}, str::String)
    # handle zero length string quickly
    isempty(str) && return empty_ascii
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte =
        unsafe_checkstring(str, 1, sizeof(str))
    num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
    Str(latinbyte == 0 ? ASCIICSE : _LatinCSE,
        flags == 0 ? str : _utf8_to_latin(_pnt(str), len))
end

convert(::Type{LatinStr}, a::Vector{UInt8}) = _convert(LatinStr, a)
convert(::Type{_LatinStr}, a::Vector{UInt8}) = _convert(isascii(a) ? ASCIIStr : _LatinStr, a)

function convert(::Type{T}, str::AbstractString) where {T<:LatinStrings}
    # Might want to have invalids_as here
    len, flags = unsafe_checkstring(str)
    (flags & ~(UTF_LONG | UTF_LATIN1)) == 0 || unierror(UTF_ERR_INVALID_LATIN1)
    buf, pnt = _allocate(UInt8, len)
    @inbounds for ch in str
        set_codeunit!(pnt, ch%UInt8)
        pnt += 1
    end
    Str(T, buf)
end
