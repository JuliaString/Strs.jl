#=
LatinStr/_LatinStr type (ISO Latin1 8-bit subset of Unicode)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for ASCIIString that used to be in Julia
=#

## overload methods for efficiency ##

isascii(str::_LatinStr)      = false
islatin(str::LatinStrings)   = true
isbmp(str::LatinStrings)     = true
isunicode(str::LatinStrings) = true

bytestring(s::LatinStrings) = s

function search(str::LatinStrings, ch::UInt32, pos::Integer)
    len = _len(str)
    pos == len + 1 && return 0
    1 <= pos <= len && boundserr(str, pos)
    ch <= 0xff ? search(_data(str), ch%UInt8, pos) : 0
end

rsearch(str::LatinStrings, ch::UInt32, pos::Integer) =
    ch <= 0xff ? rsearch(_data(str), ch%UInt8, pos) : 0

function string(c::UnicodeByteStrings...)
    length(c) == 1 && return c[1]
    n = 0
    for s in c
        n += _len(s)
    end
    buf = _allocate(n)
    off = 1
    for str in c
        len, dat = _lendata(str)
        unsafe_copyto!(buf, off, dat, 1, len)
        off += len
    end
    LatinStr(buf)
end

reverse(s::T) where {T<:LatinStrings} = T(reverse(_data(s)))

## outputting Latin 1 strings ##

function write(io::IO, str::LatinStrings)
    len, dat = _lendata(str)
    # Skip and write out ASCII sequences together
    cnt = pos = 0
    while pos < len
        # Skip to first non-ASCII sequence
        i = pos
        ch = 0x0
        while (ch = dat[i += 1]) < 0x80
            if i == len
                # Write out remaining data, from pos+1:i
                write(io, dat[pos+1:i])
                return len + cnt
            end
        end
        # Now we have from pos+1:i-1 that are ASCII
        pos+1 <= i-1 && write(io, dat[pos+1:i-1])
        cnt += 1
        ch = dat[i]
        # Write out two bytes of Latin1 character encoded as UTF-8
        write(io, 0xc0 | (ch >>> 6), 0x80 | (ch & 0x3f))
        pos = i
    end
    len + cnt
end

_write(io::IO, ch::UInt8) =
    ch <= 0x7f ? write(io, ch) : write(io, 0xc0 | (ch >>> 6), 0x80 | (ch & 0x3f))

write(io::IO, ch::LatinChars) = _write(io, tobase(ch))

function convert(::Type{T}, ch::UInt32) where {T<:LatinStrings}
    ch <= 0xff || unierror(UTF_ERR_INVALID_LATIN1, ch)
    buf = _allocate(1)
    buf[1] = ch%UInt8
    T(buf)
end

function convert(::Type{ASCIIStr}, ch::UInt32)
    ch <= 0x7f || unierror(UTF_ERR_INVALID_ASCII, ch)
    buf = _allocate(1)
    buf[1] = ch%UInt8
    ASCIIStr(buf)
end

## transcoding to Latin1 ##

latin1(x) = convert(_LatinStr, x)
convert(::Type{T}, s::T) where {T<:LatinStrings} = s
convert(::Type{T}, s::S) where {T<:LatinStrings,S<:UnicodeByteStrings} = T(_data(s))
convert(::Type{T}, s::UTF8Str) where {T<:LatinStrings} = convert(T, _data(s))

function convert(::Type{LatinStr}, str::String)
    # handle zero length string quickly
    isempty(str) && return empty_latin
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte =
        unsafe_checkstring(str, 1, sizeof(str))
    num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
    LatinStr((flags & (UTF_LATIN1 | UTF_LONG | UTF_SURROGATE)) == 0
             ? _data(str)
             : _transcode(UInt8, _data(str), len + latinbyte))
end

function convert(::Type{_LatinStr}, str::String)
    # handle zero length string quickly
    isempty(str) && return empty_ascii
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte =
        unsafe_checkstring(str, 1, sizeof(str))
    num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
    T = latinbyte == 0 ? ASCIIStr : _LatinStr
    T((flags & (UTF_LONG | UTF_SURROGATE)) == 0
      ? _data(str)
      : _transcode(UInt8, _data(str), len + latinbyte))
end

_convert(T, a::Vector{UInt8}) = (len = sizeof(a); T(copyto!(_allocate(len), a, len)))

convert(::Type{LatinStr}, a::Vector{UInt8}) = _convert(LatinStr, a)
convert(::Type{_LatinStr}, a::Vector{UInt8}) = _convert(isascii(a) ? ASCIIStr : _LatinStr, a)

latin1(p::Ptr{UInt8}) =
    latin1(p, p == C_NULL ? Csize_t(0) : ccall(:strlen, Csize_t, (Ptr{UInt8},), p))
function latin1(p::Ptr{UInt8}, len::Integer)
    p == C_NULL && throw(ArgumentError("cannot convert NULL to string"))
    _LatinStr(ccall(:jl_pchar_to_array, Vector{UInt8}, (Ptr{UInt8}, Csize_t), p, len))
end

function convert(::Type{T}, str::AbstractString) where {T<:LatinStrings}
    # Might want to have invalids_as here
    len, flags = unsafe_checkstring(str, 1, endof(str))
    (flags & ~(UTF_LONG | UTF_LATIN1)) == 0 || unierror(UTF_ERR_INVALID_LATIN1)
    buf = _allocate(len)
    out = 0
    @inbounds for ch in str
        buf[out += 1] = ch%UInt8
    end
    T(buf)
end
