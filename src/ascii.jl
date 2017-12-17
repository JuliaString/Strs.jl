#=
ASCIIStr type

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for ASCIIString that used to be in Julia
=#

## overload methods for efficiency ##

isascii(s::ASCIIStr) = true

bytestring(s::ASCIIStr) = s

function search(s::ASCIIStr, c::UInt32, i::Integer)
    len, dat = _lendata(s)
    i == len + 1 && return 0
    1 <= i <= len || throw(BoundsError(s, i))
    c < 0x80 ? search(dat, c%UInt8, i) : 0
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
        len, dat = _lendata(s)
        unsafe_copy!(v, o, dat, 1, len)
        o += len
    end
    v
end

string(c::ASCIIStr...) = length(c) == 1 ? c[1] : ASCIIStr(_string(c))

@inline _islower(c::UInt8) = 'a'%UInt8 <= c <= 'z'%UInt8
@inline _isupper(c::UInt8) = 'A'%UInt8 <= c <= 'Z'%UInt8

function ucfirst(str::ASCIIStr)
    dat = _data(str)
    (isempty(dat) || !_islower(dat[1])) && return str
    t = copy(dat)
    t[1] -= 32
    ASCIIStr(t)
end

function lcfirst(str::ASCIIStr)
    dat = _data(str)
    (isempty(dat) || !_isupper(dat[1])) && return str
    t = copy(dat)
    t[1] += 32
    ASCIIStr(t)
end

function _upper(::Type{ASCIIStr}, d, i, len)
    td = copy(d)
    @inbounds for j = i:len
        _islower(td[j]) && (td[j] -= 32)
    end
    ASCIIStr(td)
end

function uppercase(str::ASCIIStr)
    len, dat = _lendata(str)
    @inbounds for i = 1:len
        _islower(dat[i]) && return _upper(ASCIIStr, dat, i, len)
    end
    str
end

function _lower(::Type{ASCIIStr}, d, i, len)
    td = copy(d)
    @inbounds for j = i:len
        _isupper(td[j]) && (td[j] += 32)
    end
    ASCIIStr(td)
end

function lowercase(str::ASCIIStr)
    len, dat = _lendata(s)
    for i = 1:len
        _isupper(dat[i]) && return _lower(ASCIIStr, dat, i, len)
    end
    str
end

reverse(s::ASCIIStr) = ASCIIStr(reverse(_data(s)))

## outputing ASCII strings ##

write(io::IO, s::ASCIIStr) = write(io, _data(s))

## transcoding to ASCII ##

ascii(str) = convert(ASCIIStr, str)
function convert(::Type{ASCIIStr}, str::AbstractString)
    # Need to fix this to show where the non-ASCII character was found!
    isascii(str) || throw(ArgumentError("invalid ASCII sequence"))
    len = length(str)
    buf = _allocate(len)
    out = 0
    @inbounds for ch in str
        buf[out += 1] = ch%UInt8
    end
    ASCIIStr(buf)
end
convert(::Type{ASCIIStr}, str::ASCIIStr) = str
function convert(::Type{ASCIIStr}, str::T) where {T<:Union{LatinStr,UTF8Str}}
    isascii(str) || throw(ArgumentError("invalid ASCII sequence"))
    ASCIIStr(_data(str))
end
function convert(::Type{ASCIIStr}, str::String)
    len, flags = unsafe_checkstring(str, 1, endof(str))
    flags == 0 && ASCIIStr(_data(str))
    (flags & ~UTF_LONG) == 0 || throw(UnicodeError(UTF_ERR_INVALID_ASCII))
    # Handle any long encodings, such as \xc0\x80 for \0
    buf = _allocate(len)
    out = 0
    @inbounds for ch in str
        buf[out += 1] = ch%UInt8
    end
    ASCIIStr(buf)
end

ascii(pnt::Ptr{UInt8}) =
    ascii(pnt, pnt == C_NULL ? Csize_t(0) : ccall(:strlen, Csize_t, (Ptr{UInt8},), pnt))
ascii(pnt::Ptr{UInt8}, len::Integer) = begin
    pnt == C_NULL && throw(ArgumentError("cannot convert NULL to string"))
    vec = ccall(:jl_pchar_to_array, Vector{UInt8}, (Ptr{UInt8}, Csize_t), pnt, len)
    isvalid(ASCIIStr, vec) || throw(ArgumentError("invalid ASCII sequence"))
    ASCIIStr(vec)
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
    cnt == 0 && return ASCIIStr(a)
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
    ASCIIStr(v)
end

convert(::Type{ASCIIStr}, a::Vector{UInt8}, invalids_as::String) =
    _convert_ascii(a, sizeof(invalids_as), _data(ascii(invalids_as)))

convert(::Type{ASCIIStr}, a::Vector{UInt8}, invalids_as::ASCIIStr) =
    _convert_ascii(a, sizeof(invalids_as), _data(invalids_as))

convert(::Type{ASCIIStr}, a::Vector{UInt8}, invalids_as::AbstractString) =
    convert(ASCIIStr, a, ascii(invalids_as))
