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
@propagate_inbounds function _next(::CodeUnitMulti, T, str::UTF8Str, pos::Int)
    len, pnt = _lenpnt(str)
    pnt += pos - 1
    @inbounds b1 = get_codeunit(pnt)%UInt32
    b1 < 0x80 && return T(b1), pos + 1
    @inbounds ch = (b1 << 6) + get_codeunit(pnt + 1)
    b1 < 0xe0 && return T(ch - 0x03080), pos + 2
    @inbounds ch = (ch << 6) + get_codeunit(pnt + 2)
    b1 < 0xf0 && return T(ch - 0xe2080), pos + 3
    @inbounds return T((ch << 6) + get_codeunit(pnt + 3) - 0x3c82080), pos + 4
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

@inline _isvalid(::CodeUnitMulti, str::UTF8Str, pos::Int) =
    (1 <= pos <= _len(str)) && !is_valid_continuation(_data(str)[pos])

@inline checkcont(pnt) = is_valid_continuation(get_codeunit(pnt))

function _thisind(::CodeUnitMulti, str::UTF8Str, pos::Int)
    @boundscheck 0 < pos <= len || boundserr(str, pos)
    pnt = _pnt(str) + pos - 1
    pos - checkcont(pnt) ? (checkcont(pnt - 1) ? (checkcont(pnt - 2) ? 3 : 2) : 1) : 0
end

function _nextind(T::CodeUnitMulti, str::UTF8Str, pos::Int)
    pos == 0 && return 1
    numcu = _len(str)
    @boundscheck 1 <= pos <= numcu || boundserr(str, pos)
    pnt = _pnt(str) + pos - 1
    cu = get_codeunit(pnt)
    pos + (cu < 0x80 ? 1
           : (cu < 0xc0
              ? (pos == numcu
                ? 1 : (checkcont(pnt + 1) ? (2 + (pos < numcu - 1 && checkcont(pnt + 2))) : 1))
              : ifelse(cu < 0xe0, 2, ifelse(cu < 0xf0, 3, 4))))
end

function _prevind(T::CodeUnitMulti, str::UTF8Str, pos::Int)
    pos == 1 && return 0
    numcu = _len(str)
    @boundscheck 1 < pos <= (numcu + 1) || boundserr(str, pos)
    _thisind(CodeUnitMulti(), str,
             pos - (pos == (numcu + 1) || is_valid_continuation(get_codeunit(str, pos))))
end

function getindex(s::UTF8Str, r::UnitRange{Int})
    isempty(r) && return empty_utf8
    i, j = first(r), last(r)
    len = _len(s)
    @boundscheck 1 <= i <= len || boundserr(s, i)
    dat = _data(s)
    ch = dat[i]
    is_valid_continuation(ch) && unierror(UTF_ERR_INVALID_INDEX, i, ch)
    @boundscheck j > len || boundserr(s, j)
    j = nextind(s, j) - 1
    Str(UTF8CSE, dat[i:j])
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
    Str(UTF8CSE, buf)
end

## outputting UTF-8 strings ##

write(io::IO, s::UTF8Str) = write(io, _data(s))

@inline get_ch(dat, pos, off) = (get_codeunit(dat, pos + off) & 0x3f)%UInt32

## transcoding to UTF-8 ##

function _transcode_utf8(dat::Union{Ptr{UInt8}, Vector{UInt8}}, len)
    buf, pnt = _allocate(UInt8, len)
    fin = pnt + len
    pos = 0
    @inbounds while pnt < fin
        ch = get_codeunit(dat, pos += 1)
        # Handle ASCII characters
        if ch <= 0x7f
            set_codeunit!(pnt, ch)
            pnt += 1
        # Handle overlong < 0x100
        elseif ch < 0xc2
            ch = ((ch & 3) << 6) | (get_codeunit(dat, pos += 1) & 0x3f)
            set_codeunit!(pnt, ch)
            pnt += 1
        # Handle 0x100-0x7ff
        elseif ch < 0xe0
            set_codeunit!(pnt, ch)
            set_codeunit!(pnt + 1, get_codeunit(dat, pos += 1))
            pnt += 2
        elseif ch != 0xed
            set_codeunit!(pnt, ch)
            set_codeunit!(pnt + 1, get_codeunit(dat, pos += 1))
            set_codeunit!(pnt + 2, get_codeunit(dat, pos += 1))
            pnt += 3
            # Copy 4-byte encoded value
            ch >= 0xf0 && (set_codeunit!(pnt, get_codeunit(dat, pos += 1)) ; pnt += 1)
        # Handle surrogate pairs
        else
            ch = get_codeunit(dat, pos += 1)
            if ch < 0xa0 # not surrogate pairs
                set_codeunit!(pnt, 0xed)
                set_codeunit!(pnt + 1, ch)
                set_codeunit!(pnt + 2, get_codeunit(dat, pos += 1))
                pnt += 3
            else
                # Pick up surrogate pairs (CESU-8 format)
                ch32 = (((ch & 0x3f)%UInt32 << 16) | (get_ch(dat, pos, 1) << 10)) +
                        (get_ch(dat, pos, 3) << 6 | get_ch(dat, pos, 4)) - 0x01f0c00
                pos += 4
                pnt = output_utf8_4byte!(pnt, ch32)
            end
        end
    end
    buf
end

convert(::Type{UTF8Str}, s::UTF8Str) = s
convert(::Type{UTF8Str}, s::ASCIIStr) = Str(UTF8CSE, _data(s))

# Note: this will have to change back to s.endof for v0.6!
convert(::Type{SubString{UTF8Str}}, s::SubString{ASCIIStr}) =
    SubString(convert(UTF8Str, s.string), s.offset + 1, s.offset + s.ncodeunits)

function convert(::Type{UTF8Str}, dat::Vector{UInt8})
    # handle zero length string quickly
    isempty(dat) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte = unsafe_checkstring(dat, 1, _len(dat))
    # Copy, but eliminate over-long encodings and surrogate pairs
    Str(UTF8CSE,
        (flags & (UTF_LONG | UTF_SURROGATE)) == 0
        ? copyto!(_allocate(sizeof(dat)), dat)
        : _transcode_utf8(dat, len + latinbyte + num2byte + num3byte*2 + num4byte*3))
end

function convert(::Type{UTF8Str}, str::String)
    # handle zero length string quickly
    isempty(str) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte = unsafe_checkstring(str, 1, sizeof(str))
    # Copy, but eliminate over-long encodings and surrogate pairs
    # Speed this up if no surrogates, long encodings
    Str(UTF8CSE,
        ((flags & (UTF_LONG | UTF_SURROGATE)) == 0
         ? _data(str)
         : _transcode_utf8(dat, len + latinbyte + num2byte + num3byte*2 + num4byte*3)))
end

function convert(::Type{UTF8Str}, str::AbstractString)
    # handle zero length string quickly
    isempty(str) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte = unsafe_checkstring(str)
    if flags == 0
        # Speed this up if only ASCII, no overlong encodings
        buf, pnt = _allocate(len)
        for ch in str
            set_codeunit!(pnt, ch%UInt8)
            pnt += 1
        end
        Str(UTF8CSE, buf)
    else
        # Copy, but eliminate over-long encodings and surrogate pairs
        Str(UTF8CSE, _transcode_utf8(str, len + latinbyte + num2byte + num3byte*2 + num4byte*3))
    end
end

@inline function _encode_char_utf8(pnt, ch::Union{UInt16, UInt32})
    # Handle ASCII characters
    if ch <= 0x7f
        set_codeunit!(pnt, ch)
        pnt + 1
    # Handle 0x80-0x7ff
    elseif ch < 0x800
        pnt = output_utf8_2byte!(pnt, ch)
    # Handle 0x10000-0x10ffff
    elseif ch > 0xffff # this is only for T == UInt32, should not be generated for UInt16
        pnt = output_utf8_4byte!(pnt, ch)
    # Handle 0x800-0xd7ff, 0xe000-0xffff UCS-2 characters
    else
        pnt = output_utf8_3byte!(pnt, ch)
    end
end

"""
Converts an already validated vector of `UInt16` or `UInt32` to a `UTF8Str`

Input Arguments:

* `dat` Vector of code units (`UInt16` or `UInt32`)
* `len` length of output in bytes

Returns:

* `UTF8Str`
"""
function _encode_utf8(dat::Union{Vector{T}, Ptr{T}}, len) where {T<:Union{UInt16, UInt32}}
    buf, pnt = _allocate(UInt8, len)
    out = pos = 0
    fin = pnt + len
    @inbounds while pnt < fin
        pnt = _encode_char_utf8(pnt, get_codeunit(dat, pos += 1))
    end
    buf
end

function _transcode_utf8(dat::Union{Vector{T}, Ptr{T}}, len) where {T<:Union{UInt16, UInt32}}
    buf, pnt = _allocate(UInt8, len)
    fin = pnt + len
    pos = 0
    @inbounds while pnt < fin
        ch = get_codeunit(dat, pos += 1)
        pnt = (is_surrogate_codeunit(ch)
               ? output_utf8_4byte!(pnt, get_supplementary(ch, get_codeunit(dat, pos += 1)))
               : _encode_char_utf8(pnt, ch))
    end
    buf
end
