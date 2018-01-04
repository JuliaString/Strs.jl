#=
UTF8Str type

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for UTF8String that used to be in Julia
=#

## required core functionality ##

function endof(str::UTF8Str)
    (len = _len(str)) == 0 && return len
    dat = _data(str)
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
    count_ones(xor(((val << 1) | val), hi_mask) & hi_mask)
end

function _length(::CodeUnitMulti, str::UTF8Str)
    (siz = sizeof(str)) < 2 && return siz
    pnt, fin = _calcpnt(str, siz)
    cnt = siz
    while (pnt += CHUNKSZ) <= fin
        cnt -= _count_cont(unsafe_load(pnt))
    end
    pnt - CHUNKSZ == fin ? cnt : (cnt - _count_cont(unsafe_load(pnt) & _mask_bytes(siz)))
end

function isascii(str::T) where {T<:Union{UTF8Str, LatinStr}}
    (siz = sizeof(str)) == 0 && return true
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) <= fin
        (unsafe_load(pnt) & hi_mask) == 0 || return false
    end
    pnt - CHUNKSZ == fin || ((unsafe_load(pnt) & _mask_bytes(siz)) & hi_mask == 0)
end

# Todo! Here you need to see that 0b11yyyyxx at least 1 y must be set,
# which indicates a non-Latin1 character
all_latin(val) = ((val & (val<<1) & (val<<2 | (val<<3) | (val<<4) | (val<<5))) & hi_mask) == 0

function islatin(str::UTF8Str)
    (siz = sizeof(str)) == 0 && return true
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) <= fin
        all_latin(unsafe_load(pnt)) || return false
    end
    pnt - CHUNKSZ == fin || all_latin(unsafe_load(pnt) & _mask_bytes(siz))
end

# All 4 top bits must be 1 (i.e. 0xfx) for this to be non-BMP
all_bmp(val) = ((val | (val<<1) | (val<<2) | (val<<3)) & hi_mask) == 0

function isbmp(str::UTF8Str)
    (siz = sizeof(str)) == 0 && return true
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) <= fin
        all_bmp(unsafe_load(pnt)) || return false
    end
    pnt - CHUNKSZ == fin && all_bmp(unsafe_load(pnt) & _mask_bytes(siz))
end

isunicode(str::UTF8Str) = true

# Gets next codepoint
@propagate_inbounds function _next(::CodeUnitMulti, T, str::UTF8Str, i::Int)
    len, pnt = _lenpnt(str)
    @inbounds b1 = get_codeunit(pnt, i)%UInt32
    b1 < 0x80 && return T(b1), i+1
    @inbounds ch = (b1 << 6) + get_codeunit(pnt, i+1)
    b1 < 0xe0 && return T(ch - 0x03080), i+2
    @inbounds ch = (ch << 6) + get_codeunit(pnt, i+2)
    b1 < 0xf0 && return T(ch - 0xe2080), i+3
    @inbounds return T((ch << 6) + get_codeunit(pnt, i+3) - 0x3c82080), i+4
end

@propagate_inbounds done(str::UTF8Str, i::Int) = done(_data(str), i)

length(it::CodePoints{String}, i::Int) = length(it.xs)

@propagate_inbounds function next(it::CodePoints{String}, state)
    ch, state = next(it.xs, state)
    UTF32Chr(ch%UInt32), state
end

@inline function first_utf8_byte(ch::UInt32)
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

isvalid(s::UTF8Str, i::Integer) = (1 <= i <= _len(s)) && !is_valid_continuation(_data(s)[i])

function getindex(s::UTF8Str, r::UnitRange{Int})
    isempty(r) && return empty_utf8
    i, j = first(r), last(r)
    len = _len(s)
    1 <= i <= len || boundserr(s, i)
    dat = _data(s)
    ch = dat[i]
    is_valid_continuation(ch) && unierror(UTF_ERR_INVALID_INDEX, i, ch)
    j > len || boundserr(s, j)
    j = nextind(s, j) - 1
    UTF8Str(dat[i:j])
end

function search(s::UTF8Str, c::UInt32, i::Integer)
    len = _len(s)
    if 1 <= i <= len
        i == len + 1 && return 0
        boundserr(s, i)
    end
    dat = _data(s)
    is_valid_continuation(dat[i]) && unierror(UTF_ERR_INVALID_INDEX, i, dat[i])
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

const _ByteStr = Union{ASCIIStr, UTF8Str, String}

string(c::_ByteStr...) = length(c) == 1 ? c[1]::UTF8Str : UTF8Str(_string(c))
    # ^^ at least one must be UTF-8 or the ASCII-only method would get called

function reverse(s::UTF8Str)
    (len = _len(s)) <= 1 && return s
    buf = _allocate(len)
    out = len
    pos = 1
    dat = _data(s)
    @inbounds while out > 0
        ch = dat[pos]
        if ch > 0xdf
            if ch < 0xf0
                # (out -= 3) < 0 && unierror(UTF_ERR_SHORT, pos, ch)
                buf[out - 2], buf[out - 1], buf[out] = ch, dat[pos + 1], dat[pos + 2]
                out -= 3
                pos += 3
            else
                # (out -= 4) < 0 && unierror(UTF_ERR_SHORT, pos, ch)
                buf[out - 3], buf[out - 2], buf[out - 1], buf[out] =
                    ch, dat[pos+1], dat[pos+2], dat[pos+3]
                out -= 4 # Assume valid for UTF8Str!
                pos += 4
            end
        elseif ch > 0x7f
            # (out -= 2) < 0 && unierror(UTF_ERR_SHORT, pos, ch)
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

@inline get_ch(dat, pos, off) = (get_codeunit(dat, pos + off) & 0x3f)%UInt32

## transcoding to UTF-8 ##

function _transcode(::Type{UInt8}, dat::T, len) where {T<:Union{Ptr{UInt8}, Vector{UInt8}}}
    buf, pnt = _allocate(UInt8, len)
    out = 0
    pos = 0
    @inbounds while out < len
        ch = get_codeunit(dat, pos += 1)
        # Handle ASCII characters
        if ch <= 0x7f
            set_codeunit!(pnt, out += 1, ch)
        # Handle overlong < 0x100
        elseif ch < 0xc2
            ch = ((ch & 3) << 6) | (get_codeunit(dat, pos += 1) & 0x3f)
            set_codeunit!(pnt, out += 1, ch)
        # Handle 0x100-0x7ff
        elseif ch < 0xe0
            set_codeunit!(pnt, out += 1, ch)
            set_codeunit!(pnt, out += 1, get_codeunit(dat, pos += 1))
        elseif ch != 0xed
            set_codeunit!(pnt, out += 1, ch)
            set_codeunit!(pnt, out += 1, get_codeunit(dat, pos += 1))
            set_codeunit!(pnt, out += 1, get_codeunit(dat, pos += 1))
            # Copy 4-byte encoded value
            ch >= 0xf0 && set_codeunit!(pnt, out += 1, get_codeunit(dat, pos += 1))
        # Handle surrogate pairs
        else
            ch = get_codeunit(dat, pos += 1)
            if ch < 0xa0 # not surrogate pairs
                set_codeunit!(pnt, out += 1, 0xed)
                set_codeunit!(pnt, out += 1, ch)
                set_codeunit!(pnt, out += 1, get_codeunit(dat, pos += 1))
            else
                # Pick up surrogate pairs (CESU-8 format)
                ch32 = (((ch & 0x3f)%UInt32 << 16) | (get_ch(dat, pos, 1) << 10)) +
                        (get_ch(dat, pos, 3) << 6 | get_ch(dat, pos, 4)) - 0x01f0c00
                pos += 4
                output_utf8_4byte!(buf, out, ch32)
                out += 4
            end
        end
    end
    buf
end

utf8(x) = convert(UTF8Str, x)
convert(::Type{UTF8Str}, s::UTF8Str) = s
convert(::Type{UTF8Str}, s::ASCIIStr) = UTF8Str(_data(s))
convert(::Type{SubString{UTF8Str}}, s::SubString{ASCIIStr}) =
    SubString(utf8(s.string), s.offset + 1, s.endof + s.offset)

function convert(::Type{UTF8Str}, dat::Vector{UInt8})
    # handle zero length string quickly
    isempty(dat) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte = unsafe_checkstring(dat, 1, _len(dat))
    # Copy, but eliminate over-long encodings and surrogate pairs
    UTF8Str((flags & (UTF_LONG | UTF_SURROGATE)) == 0
            ? copyto!(_allocate(sizeof(dat)), dat)
            : _transcode(UInt8, dat, len + latinbyte + num2byte + num3byte*2 + num4byte*3))
end

function convert(::Type{UTF8Str}, str::String)
    # handle zero length string quickly
    isempty(str) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte = unsafe_checkstring(str, 1, sizeof(str))
    # Copy, but eliminate over-long encodings and surrogate pairs
    # Speed this up if no surrogates, long encodings
    UTF8Str((flags & (UTF_LONG | UTF_SURROGATE)) == 0
            ? _data(str)
            : _transcode(UInt8, dat, len + latinbyte + num2byte + num3byte*2 + num4byte*3))
end

function convert(::Type{UTF8Str}, str::AbstractString)
    # handle zero length string quickly
    isempty(str) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte = unsafe_checkstring(str)
    if flags == 0
        # Speed this up if only ASCII, no overlong encodings
        buf, pnt = _allocate(len)
        out = 0
        for ch in str
            set_codepoint!(pnt, out += 1, ch%UInt8)
        end
        UTF8Str(buf)
    else
        # Copy, but eliminate over-long encodings and surrogate pairs
        UTF8Str(_transcode(UInt8, str, len + latinbyte + num2byte + num3byte*2 + num4byte*3))
    end
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

"""
Converts an already validated vector of `UInt16` or `UInt32` to a `UTF8Str`

Input Arguments:

* `dat` Vector of code units (`UInt16` or `UInt32`)
* `len` length of output in bytes

Returns:

* `UTF8Str`
"""
function _encode(::Type{UInt8}, dat::Union{Vector{T}, Ptr{T}},
                 len) where {T<:Union{UInt16, UInt32}}
    buf, pnt = _allocate(UInt8, len)
    out = pos = 0
    @inbounds while out < len
        out = _encode_utf8(pnt, get_codeunit(dat, pos += 1), out)
    end
    buf
end

function _transcode(::Type{UInt8}, dat::Union{Vector{T}, Ptr{T}},
                    len) where {T<:Union{UInt16, UInt32}}
    buf, pnt = _allocate(UInt8, len)
    out = pos = 0
    @inbounds while out < len
        out = _transcode_utf8(pnt, get_codeunit(dat, pos += 1), out)
    end
    buf
end

"""
Converts an already validated vector of `UInt16` or `UInt32` to a `UTF8Str`

Input Arguments:

* `dat` Vector of code units (`UInt16` or `UInt32`)
* `len` length of output in bytes
"""
function _transcode_utf8(pnt, ch::Union{UInt16, UInt32}, out)
    # Handle ASCII characters
    if ch <= 0x7f
        set_codeunit!(pnt, out += 1, ch)
        # Handle 0x80-0x7ff
    elseif ch < 0x800
        set_codeunit!(pnt, out += 1, 0xc0 | (ch >>> 6))
        set_codeunit!(pnt, out += 1, 0x80 | (ch & 0x3f))
        # Handle 0x10000-0x10ffff (if input is UInt32)
    elseif ch > 0xffff # this is only for T == UInt32, should not be generated for UInt16
        output_utf8_4byte!(pnt, out, ch)
        out += 4
        # Handle surrogate pairs
    elseif is_surrogate_codeunit(ch)
        output_utf8_4byte!(pnt, out, get_supplementary(ch, get_codeunit(dat, pos += 1)))
        out += 4
        # Handle 0x800-0xd7ff, 0xe000-0xffff UCS-2 characters
    else
        set_codeunit!(pnt, out += 1, 0xe0 | ((ch >>> 12) & 0x3f))
        set_codeunit!(pnt, out += 1, 0x80 | ((ch >>> 6) & 0x3f))
        set_codeunit!(pnt, out += 1, 0x80 | (ch & 0x3f))
    end
    out
end

@inline function _encode_utf8(buf, ch::Union{UInt16, UInt32}, out)
    # Handle ASCII characters
    if ch <= 0x7f
        set_codeunit!(buf, out += 1, ch)
        out
    # Handle 0x80-0x7ff
    elseif ch < 0x800
        set_codeunit!(buf, out + 1, 0xc0 | (ch >>> 6))
        set_codeunit!(buf, out + 2, 0x80 | (ch & 0x3f))
        out + 2
    # Handle 0x10000-0x10ffff
    elseif ch > 0xffff # this is only for T == UInt32, should not be generated for UInt16
        output_utf8_4byte!(buf, out, ch)
        out + 4
    # Handle 0x800-0xd7ff, 0xe000-0xffff UCS-2 characters
    else
        set_codeunit!(buf, out + 1, 0xe0 | ((ch >>> 12) & 0x3f))
        set_codeunit!(buf, out + 2, 0x80 | ((ch >>> 6) & 0x3f))
        set_codeunit!(buf, out + 3, 0x80 | (ch & 0x3f))
        out + 3
    end
end

utf8(p::Ptr{UInt8}) =
    utf8(p, p == C_NULL ? Csize_t(0) : ccall(:strlen, Csize_t, (Ptr{UInt8},), p))

function utf8(p::Ptr{UInt8}, len::Integer)
    p == C_NULL && nullerr()
    UTF8Str(ccall(:jl_pchar_to_array, Vector{UInt8}, (Ptr{UInt8}, Csize_t), p, len))
end
