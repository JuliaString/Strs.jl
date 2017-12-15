#=
UTF8Str type

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for UTF8String that used to be in Julia
=#

## required core functionality ##

function endof(str::UTF8Str)
    len, dat = _lendata(str)
    len == 0 && return len
    while is_valid_continuation(dat[len])
        len -= 1
    end
    len
end

utf_trail(c::UInt8) = (0xe5000000 >>> ((c & 0xf0) >> 3))

#=
_cont(byt, n) = (byt >>> n)%UInt8 != 0x80
cnt += 8
else
val &= 0xe0e0_e0e0_e0e0_e0e0
# Count ones that are not valid continuation
cnt += _cont(val, 0x0) + _cont(val, 0x8) + _cont(val, 0x10) + _cont(val, 0x18) +
_cont(val, 0x20) + _cont(val, 0x28) + _cont(val, 0x30) + _cont(val, 0x38)
end

xor 80 then << 1 then |
00 -> 10 -> 1
01 -> 11 -> 1
10 -> 00 -> 0
11 -> 01 -> 1
=#

const hi_mask = 0x8080_8080_8080_8080 

@inline function _count_cont(v)
    val = xor(v, hi_mask)
    count_ones((val << 1) | val) & hi_mask
end

function length(str::UTF8Str)
    len, pnt = _lenpnt(str)
    cnt = pos = 0
    qpnt = reinterpret(Ptr{UInt64}, pnt)
    while len >= 8
        cnt += _count_cont(unsafe_load(qpnt))
        qpnt += 8
        len -= 8
    end
    len == 0 ? cnt : cnt + _count_cont(unsafe_load(qpnt) & _mask_bytes(len))
end

@inline _mask_bytes(n) = (1%UInt<<(n<<3))-0x1

function isascii(str::T) where {T<:Union{UTF8Str, LatinStr}}
    len, pnt = _lenpnt(str)
    qpnt = reinterpret(Ptr{UInt64}, pnt)
    while len >= 8
        unsafe_load(qpnt) & hi_mask == 0 || return false
        qpnt += 8
        len -= 8
    end
    len == 0 ? true : (unsafe_load(qpnt) & _mask_bytes(len)) & hi_mask == 0
end

# Gets next codepoint (UInt32)
function _next(str::UTF8Str, i::Int)
    len, pnt = _lenpnt(str)
    b1 = get_codeunit(pnt, i)%UInt32
    b1 < 0x80 && return b1, i+1
    ch = b1 << 6 + get_codeunit(pnt, i+1)
    b1 < 0xe0 && return ch - 0x03080, i+2
    ch = ch << 6 + get_codeunit(pnt, i+2)
    b1 < 0xf0 && return ch - 0xe2080, i+3
    ch << 6 + get_codeunit(pnt, i+3) - 0x3c82080, i+4
end

done(str::UTF8Str, i::Int) = done(_data(str), i)
next(str::UTF8Str, i::Int) = begin ch, i = _next(str, i); Char(ch), i ; end
next(it::CodeUnits{UTF8Str}, i::Int) = next(_data(it.xs), i)
next(it::CodePoints{UTF8Str}, i::Int) = _next(it.xs, i)

length(it::CodePoints{String}, i::Int) = length(it.xs)
Base.@propagate_inbounds next(it::CodeUnits{String}, state)  = (codeunit(it.xs, state), state+1)
Base.@propagate_inbounds function next(it::CodePoints{String}, state)
    ch, state = next(it.xs, state)
    UInt32(ch), state
end

function first_utf8_byte(ch::Char)
    c = UInt32(ch)
    c < 0x80    ? c%UInt8 :
    c < 0x800   ? ((c>>6)  | 0xc0)%UInt8 :
    c < 0x10000 ? ((c>>12) | 0xe0)%UInt8 :
                  ((c>>18) | 0xf0)%UInt8
end

function reverseind(s::UTF8Str, i::Integer)
    j = lastidx(s) + 1 - i
    d = _data(s)
    while is_valid_continuation(d[j])
        j -= 1
    end
    j
end

## overload methods for efficiency ##

bytestring(s::UTF8Str) = s

lastidx(s::UTF8Str) = sizeof(s)

function isvalid(s::UTF8Str, i::Integer)
    len, dat = _lendata(s)
    (1 <= i <= len) && !is_valid_continuation(dat[i])
end

function getindex(s::UTF8Str, r::UnitRange{Int})
    isempty(r) && return empty_utf8
    i, j = first(r), last(r)
    len, dat = _lendata(s)
    1 <= i <= len || throw(BoundsError(s, i))
    is_valid_continuation(dat[i]) &&
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, dat[i]))
    j > len || throw(BoundsError(s, j))
    j = nextind(s, j) - 1
    UTF8Str(dat[i:j])
end

function search(s::UTF8Str, c::UInt32, i::Integer)
    len, dat = _lendata(s)
    if 1 <= i <= len
        i == len + 1 && return 0
        throw(BoundsError(s, i))
    end
    is_valid_continuation(dat[i]) &&
        throw(UnicodeError(UTF_ERR_INVALID_INDEX, i, dat[i]))
    c < 0x80 && return search(d, c%UInt8, i)
    while true
        i = search(dat, first_utf8_byte(c), i)
        (i==0 || s[i] == c) && return i
        i = next(s, i)[2]
    end
end
function rsearch(s::UTF8Str, c::UInt32, i::Integer)
    dat = _data(s)
    c < 0x80 && return rsearch(dat, c%UInt8, i)
    b = first_utf8_byte(c)
    while true
        i = rsearch(dat, b, i)
        (i==0 || s[i] == c) && return i
        i = prevind(s, i)
    end
end

search(s::UTF8Str, c::Char, i::Integer) = search(s, UInt32(c), i)
rsearch(s::UTF8Str, c::Char, i::Integer) = rsearch(s, UInt32(c), i)

const _ByteStr = Union{ASCIIStr, UTF8Str, String}

string(c::_ByteStr...) = length(c) == 1 ? c[1]::UTF8Str : UTF8Str(_string(c))
    # ^^ at least one must be UTF-8 or the ASCII-only method would get called

function reverse(s::UTF8Str)
    len, dat = _lendata(s)
    len <= 1 && return s
    buf = _sv(len)
    out = len
    pos = 1
    @inbounds while out > 0
        ch = dat[pos]
        if ch > 0xdf
            if ch < 0xf0
                # (out -= 3) < 0 && throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
                buf[out - 2], buf[out - 1], buf[out] = ch, dat[pos + 1], dat[pos + 2]
                out -= 3
                pos += 3
            else
                # (out -= 4) < 0 && throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
                buf[out - 3], buf[out - 2], buf[out - 1], buf[out] =
                    ch, dat[pos+1], dat[pos+2], dat[pos+3]
                out -= 4 # Assume valid for UTF8Str!
                pos += 4
            end
        elseif ch > 0x7f
            # (out -= 2) < 0 && throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
            buf[out - 1], buf[out] = ch, dat[pos + 1]
            out -= 2
            pos += 2
        else
            buf[out] = ch
            out -= 1
            pos += 1
        end
    end
    UTF8Str(buf)
end

## outputting UTF-8 strings ##

write(io::IO, s::UTF8Str) = write(io, _data(s))

## transcoding to UTF-8 ##

utf8(x) = convert(UTF8Str, x)
convert(::Type{UTF8Str}, s::UTF8Str) = s
convert(::Type{UTF8Str}, s::ASCIIStr) = UTF8Str(_data(s))
convert(::Type{SubString{UTF8Str}}, s::SubString{ASCIIStr}) =
    SubString(utf8(s.string), s.offset + 1, s.endof + s.offset)

function convert(::Type{UTF8Str}, dat::Vector{UInt8})
    # handle zero length string quickly
    isempty(dat) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte = unsafe_checkstring(dat)
    (flags & (UTF_LONG | UTF_SURROGATE)) == 0 &&
        return UTF8Str(copy!(_sv(sizeof(dat)), dat))
    # Copy, but eliminate over-long encodings and surrogate pairs
    len += num2byte + num3byte*2 + num4byte*3
    buf = _sv(len)
    out = 0
    pos = 0
    @inbounds while out < len
        ch::UInt32 = dat[pos += 1]
        # Handle ASCII characters
        if ch <= 0x7f
            buf[out += 1] = ch
        # Handle overlong < 0x100
        elseif ch < 0xc2
            buf[out += 1] = ((ch & 3) << 6) | (dat[pos += 1] & 0x3f)
        # Handle 0x100-0x7ff
        elseif ch < 0xe0
            buf[out += 1] = ch
            buf[out += 1] = dat[pos += 1]
        elseif ch != 0xed
            buf[out += 1] = ch
            buf[out += 1] = dat[pos += 1]
            buf[out += 1] = dat[pos += 1]
            # Copy 4-byte encoded value
            ch >= 0xf0 && (buf[out += 1] = dat[pos += 1])
        # Handle surrogate pairs
        else
            ch = dat[pos += 1]
            if ch < 0xa0 # not surrogate pairs
                buf[out += 1] = 0xed
                buf[out += 1] = ch
                buf[out += 1] = dat[pos += 1]
            else
                # Pick up surrogate pairs (CESU-8 format)
                ch = ((((((ch & 0x3f) << 6) | (dat[pos + 1] & 0x3f)) << 10)
                       + (((dat[pos + 3] & 0x3f)%UInt32 << 6) | (dat[pos + 4] & 0x3f)))
                      - 0x01f0c00)
                pos += 4
                output_utf8_4byte!(buf, out, ch)
                out += 4
            end
        end
    end
    UTF8Str(buf)
end

#=
# This is broken and inefficient
function convert(::Type{UTF8Str}, a::Vector{UInt8}, invalids_as::AbstractString)
    l = length(a)
    idx = 1
    iscopy = false
    while idx <= l
        if !is_valid_continuation(a[idx])
            nextidx = idx + 1 + utf8_trailing[a[idx]+1]
            (nextidx <= (l+1)) && (idx = nextidx; continue)
        end
        !iscopy && (a = copy(a); iscopy = true)
        endn = idx
        while endn <= l
            !is_valid_continuation(a[endn]) && break
            endn += 1
        end
        (endn > idx) && (endn -= 1)
        splice!(a, idx:endn, Vector{UInt8}(invalids_as))
        l = length(a)
    end
    UTF8Str(a)
end
=#
convert(::Type{UTF8Str}, s::AbstractString) = utf8(bytestring(s))

"""
Converts an already validated vector of `UInt16` or `UInt32` to a `UTF8Str`

Input Arguments:

* `dat` Vector of code units (`UInt16` or `UInt32`)
* `len` length of output in bytes

Returns:

* `UTF8Str`
"""
function _encode(::Type{UInt8}, dat::Union{Vector{T}, Ptr{T}}, len) where {T<:Union{UInt16, UInt32}}
    buf = _sv(len)
    out = 0
    pos = 0
    @inbounds while out < len
        ch::UInt32 = get_codeunit(dat, pos += 1)
        # Handle ASCII characters
        if ch <= 0x7f
            buf[out += 1] = ch
        # Handle 0x80-0x7ff
        elseif ch < 0x800
            buf[out += 1] = 0xc0 | (ch >>> 6)
            buf[out += 1] = 0x80 | (ch & 0x3f)
        # Handle 0x10000-0x10ffff (if input is UInt32)
        elseif ch > 0xffff # this is only for T == UInt32, should not be generated for UInt16
            output_utf8_4byte!(buf, out, ch)
            out += 4
        # Handle surrogate pairs
        elseif is_surrogate_codeunit(ch)
            output_utf8_4byte!(buf, out, get_supplementary(ch, get_codeunit(dat, pos += 1)))
            out += 4
        # Handle 0x800-0xd7ff, 0xe000-0xffff UCS-2 characters
        else
            buf[out += 1] = 0xe0 | ((ch >>> 12) & 0x3f)
            buf[out += 1] = 0x80 | ((ch >>> 6) & 0x3f)
            buf[out += 1] = 0x80 | (ch & 0x3f)
        end
    end
    UTF8Str(buf)
end

utf8(p::Ptr{UInt8}) =
    utf8(p, p == C_NULL ? Csize_t(0) : ccall(:strlen, Csize_t, (Ptr{UInt8},), p))

function utf8(p::Ptr{UInt8}, len::Integer)
    p == C_NULL && throw(ArgumentError("cannot convert NULL to string"))
    UTF8Str(ccall(:jl_pchar_to_array, Vector{UInt8}, (Ptr{UInt8}, Csize_t), p, len))
end
