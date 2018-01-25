#=
ASCIIStr type

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for ASCIIString that used to be in Julia
=#

## overload methods for efficiency ##

bytestring(s::ASCIIStr) = s

function search(s::ASCIIStr, c::UInt32, i::Integer)
    len = _len(s)
    i == len + 1 && return 0
    1 <= i <= len || boundserr(s, i)
    c < 0x80 ? search(_data(s), c%UInt8, i) : 0
end

rsearch(s::ASCIIStr, c::UInt32, i::Integer) =
    c < 0x80 ? rsearch(_data(s), c%UInt8, i) : 0

function _string(c)
    n = 0
    for s in c
        n += _len(s)
    end
    v = _allocate(n)
    o = 1
    for s in c
        len = _len(s)
        unsafe_copy!(v, o, _data(s), 1, len)
        o += len
    end
    v
end

string(c::ASCIIStr...) = length(c) == 1 ? c[1] : Str(ASCIICSE, _string(c))

## outputting ASCII strings ##

write(io::IO, s::ASCIIStr) = write(io, _data(s))

write(io::IO, ch::ASCIIChr) = write(io, tobase(ch))

## transcoding to ASCII ##

function convert(::Type{ASCIIStr}, str::AbstractString)
    # Need to fix this to show where the non-ASCII character was found!
    isempty(str) && return empty_ascii
    len = length(str)
    buf, pnt = _allocate(UInt8, len)
    @inbounds for ch in str
        isascii(ch) || unierror(UTF_ERR_INVALID_ASCII, pnt - pointer(buf) + 1, ch)
        set_codeunit!(pnt, ch%UInt8)
        pnt += 1
    end
    Str(ASCIICSE, buf)
end

convert(::Type{ASCIIStr}, str::T) where {T<:Union{LatinStr,UTF8Str}} =
    isascii(str) ? _convert(ASCIIStr, _data(str)) : unierror(UTF_ERR_INVALID_ASCII)

convert(::Type{ASCIIStr}, dat::Vector{UInt8}) =
    isascii(dat) ? _convert(ASCIIStr, dat) : unierror(UTF_ERR_INVALID_ASCII)

function convert(::Type{ASCIIStr}, str::String)
    len, flags = unsafe_checkstring(str, 1, sizeof(str))
    flags == 0 && return Str(ASCIICSE, _data(str))
    (flags & ~UTF_LONG) == 0 || unierror(UTF_ERR_INVALID_ASCII)
    # Handle any long encodings, such as \xc0\x80 for \0 (maybe that should only be for unsafe_str)
    buf, pnt = _allocate(UInt8, len)
    @inbounds for ch in str
        set_codeunit!(pnt, ch%UInt8)
        pnt += 1
    end
    Str(ASCIICSE, buf)
end

# This should really use a keyword argument, and allow for the following possibilities:
# 1) use default substitution character \u{1a} for ASCII/Latin1 (SUB) or \u{fffd} for Unicode
# 2) use a different substitution character
# 3) use a substition string
# 4) use a substition function, which gets passed the location of the start of the invalid sequence
#    and returns a replacement string, and the new location after the invalid sequence.
#    Note: for that to work, would need to use the same function in the checkstring function
# 5) throw an exception, with enough information that one can determine where in the input
#    the invalid character was, and what it was

function _convert_ascii(a, invlen, invdat)
    len = length(a)
    cnt = 0
    @inbounds for i = 1:len ; cnt += (a[i] >= 0x80) ; end
    # Note: this doesn't make a copy if only ascii characters,
    # so that changes later to the vector will affect the string!
    # that is only safe if the vector came from something else immutable
    cnt == 0 && return Str(ASCIICSE, a)
    v = _allocate(len + cnt*invlen)
    out = 1
    @inbounds for i = 1:len
        if (ch = a[i]) <= 0x7f
            v[out] = ch
            out += 1
        else
            unsafe_copy!(v, out, invdat, 1, invlen)
            out += invlen
        end
    end
    Str(ASCIICSE, v)
end

convert(::Type{ASCIIStr}, a::Vector{UInt8}, invalids_as::String) =
    _convert_ascii(a, sizeof(invalids_as), _data(ascii(invalids_as)))

convert(::Type{ASCIIStr}, a::Vector{UInt8}, invalids_as::ASCIIStr) =
    _convert_ascii(a, sizeof(invalids_as), _data(invalids_as))

convert(::Type{ASCIIStr}, a::Vector{UInt8}, invalids_as::AbstractString) =
    convert(ASCIIStr, a, ascii(invalids_as))
