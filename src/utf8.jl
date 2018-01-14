#=
UTF8Str type

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for UTF8String that used to be in Julia
=#

# UTF-8 support functions

# Get rest of character ch from 2-byte UTF-8 sequence in dat
@inline get_utf8_2byte(pnt, ch) =
    @inbounds return (((ch & 0x3f)%UInt16 << 6) | (get_codeunit(pnt) & 0x3f))

# Get rest of character ch from 3-byte UTF-8 sequence in dat
@inline get_utf8_3byte(pnt, ch) =
    @inbounds return (((ch & 0xf)%UInt16 << 12)
                      | ((get_codeunit(pnt - 1)%UInt16 & 0x3f) << 6)
                      | (get_codeunit(pnt) & 0x3f))

# Get rest of character ch from 4-byte UTF-8 sequence in dat
@inline get_utf8_4byte(pnt, ch) =
    @inbounds return (((ch & 0x7)%UInt32 << 18)
                      | ((get_codeunit(pnt - 2)%UInt32 & 0x3f) << 12)
                      | ((get_codeunit(pnt - 1)%UInt32 & 0x3f) << 6)
                      | (get_codeunit(pnt) & 0x3f))

# Output a character as a 2-byte UTF-8 sequence
@inline function output_utf8_2byte!(pnt, ch)
    set_codeunit!(pnt,     0xc0 | (ch >>> 6))
    set_codeunit!(pnt + 1, 0x80 | (ch & 0x3f))
    pnt + 2
end

# Output a character as a 3-byte UTF-8 sequence
@inline function output_utf8_3byte!(pnt, ch)
    set_codeunit!(pnt,     0xe0 | ((ch >>> 12) & 0x3f))
    set_codeunit!(pnt + 1, 0x80 | ((ch >>> 6) & 0x3f))
    set_codeunit!(pnt + 2, 0x80 | (ch & 0x3f))
    pnt + 3
end

# Output a character as a 4-byte UTF-8 sequence
@inline function output_utf8_4byte!(pnt, ch)
    set_codeunit!(pnt,     0xf0 | (ch >>> 18))
    set_codeunit!(pnt + 1, 0x80 | ((ch >>> 12) & 0x3f))
    set_codeunit!(pnt + 2, 0x80 | ((ch >>> 6) & 0x3f))
    set_codeunit!(pnt + 3, 0x80 | (ch & 0x3f))
    pnt + 4
end

@inline _write_utf_2(io, ch) =
    write(io, 0xc0 | (ch >>> 6)%UInt8, 0x80 | (ch & 0x3f)%UInt8)

@inline _write_utf_3(io, ch) =
    write(io, 0xe0 | ((ch >>> 12) & 0x3f)%UInt8,
              0x80 | ((ch >>> 6) & 0x3f)%UInt8,
              0x80 | (ch & 0x3f)%UInt8)

@inline _write_utf_4(io, ch) =
    write(io, 0xf0 | (ch >>> 18)%UInt8,
              0x80 | ((ch >>> 12) & 0x3f)%UInt8,
              0x80 | ((ch >>> 6) & 0x3f)%UInt8,
              0x80 | (ch & 0x3f)%UInt8)

@inline _write_ucs2(io, ch) =
    ch <= 0x7f ? write(io, ch%UInt8) : ch <= 0x7ff ? _write_utf_2(io, ch) : _write_utf_3(io, ch)

@inline _write_utf32(io, ch) = ch <= 0xffff ? _write_ucs2(io, ch) : _write_utf_4(io, ch)

@inline write(io::IO, ch::UCS2Chr) = _write_ucs2(io, tobase(ch))
@inline write(io::IO, ch::UTF32Chr) = _write_utf32(io, tobase(ch))

## required core functionality ##

function endof(str::UTF8Str)
    (len = _len(str)) == 0 && return len
    dat = _data(str)
    while is_valid_continuation(dat[len])
        len -= 1
    end
    len
end

utf_trail(c::UInt8) = (0xe5000000 >>> ((c & 0xf0) >> 3)) & 0x3

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
    len = _len(str)
    @boundscheck 0 < pos <= len || boundserr(str, pos)
    pnt = _pnt(str) + pos - 1
    ch = get_codeunit(pnt)
    if ch < 0x80
        T(ch), pos + 1
    # elseif ch < 0xc0 # This means an incorrect position was passed, next documents won't happen
    elseif ch < 0xe0
        T(get_utf8_2byte(pnt + 1, ch)), pos + 2
    elseif ch < 0xf0
        T(get_utf8_3byte(pnt + 2, ch)), pos + 3
    else
        T(get_utf8_4byte(pnt + 3, ch)), pos + 4
    end
end

@propagate_inbounds done(str::UTF8Str, i::Int) = done(_data(str), i)

length(it::CodePoints{String}, i::Int) = length(it.xs)

@propagate_inbounds function next(it::CodePoints{String}, state)
    ch, state = next(it.xs, state)
    UTF32Chr(ch%UInt32), state
end

@inline first_utf8_byte(ch::UInt32) =
    (ch > 0x7f
     ? (ch > 0x7ff ? (ch > 0xffff ? ((ch>>18) | 0xf0) : ((ch>>12) | 0xe0)) : ((ch>>6) | 0xc0))
     : ch)%UInt8

function _reverseind(::CodeUnitMulti, str::UTF8Str, pos::Int)
    pnt = _pnt(s) + _len(str) + 1 - pos
    pos - (is_valid_continuation(pnt)
           ? (is_valid_continuation(pnt - 1) ? (is_valid_continuation(pnt - 2) ? 3 : 2) : 1) : 0)
end

## overload methods for efficiency ##

bytestring(str::UTF8Str) = str

lastidx(str::UTF8Str) = sizeof(str)

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
    _thisind(T, str,
             pos - (pos == (numcu + 1) || is_valid_continuation(get_codeunit(str, pos))))
end

function getindex(str::UTF8Str, rng::UnitRange{Int})
    isempty(rng) && return SubString(empty_utf8, 1, 0)
    beg = first(rng) 
    len = _len(str)
    @boundscheck 1 <= beg <= len || boundserr(str, beg)
    dat = _data(str)
    @inbounds is_valid_continuation(dat[beg]) && unierror(UTF_ERR_INVALID_INDEX, beg, dat[beg])
    lst = last(rng)
    @boundscheck beg > lst > len || boundserr(str, lst)
    SubString(str, beg, nextind(str, lst) - 1)
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

function _transcode_utf8(pnt::Ptr{UInt8}, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    @inbounds while out < fin
        ch = get_codeunit(pnt)
        # Handle ASCII characters
        if ch <= 0x7f
            set_codeunit!(out, ch)
            out += 1
        # Handle overlong < 0x100
        elseif ch < 0xc2
            ch = ((ch & 3) << 6) | (get_codeunit(pnt += 1) & 0x3f)
            set_codeunit!(out, ch)
            out += 1
        # Handle 0x100-0x7ff
        elseif ch < 0xe0
            set_codeunit!(out, ch)
            set_codeunit!(out + 1, get_codeunit(pnt += 1))
            out += 2
        elseif ch != 0xed
            set_codeunit!(out, ch)
            set_codeunit!(out + 1, get_codeunit(pnt += 1))
            set_codeunit!(out + 2, get_codeunit(pnt += 1))
            out += 3
            # Copy 4-byte encoded value
            ch >= 0xf0 && (set_codeunit!(out, get_codeunit(pnt += 1)) ; out += 1)
        # Handle surrogate pairs
        else
            ch = get_codeunit(pnt += 1)
            if ch < 0xa0 # not surrogate pairs
                set_codeunit!(out, 0xed)
                set_codeunit!(out + 1, ch)
                set_codeunit!(out + 2, get_codeunit(pnt += 1))
                out += 3
            else
                # Pick up surrogate pairs (CESU-8 format)
                ch32 = (((ch & 0x3f)%UInt32 << 16) | (get_ch(pnt + 1) << 10)) +
                    (get_ch(pnt + 3) << 6 | get_ch(pnt + 4)) - 0x01f0c00
                pnt += 4
                out = output_utf8_4byte!(out, ch32)
            end
        end
        pnt += 1
    end
    buf
end
_transcode_utf8(dat::Vector{UInt8}, len) = _transcode_utf8(pointer(dat), len)

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

const WideCodeUnit = Union{UInt16, UInt32}

@inline function _encode_char_utf8(pnt, ch::WideCodeUnit)
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
function _encode_utf8(pnt::Ptr{T}, len) where {T<:WideCodeUnit}
    buf, out = _allocate(UInt8, len)
    fin = out + len
    while out < fin
        out = _encode_char_utf8(out, get_codeunit(pnt))
        pnt += sizeof(T)
    end
    buf
end
_encode_utf8(dat::Vector{<:WideCodeUnit}, len) = _encode_utf8(pointer(dat), len)

function _transcode_utf8(pnt::Ptr{T}, len) where {T<:WideCodeUnit}
    buf, out = _allocate(UInt8, len)
    fin = out + len
    while out < fin
        ch = get_codeunit(pnt)
        out = (is_surrogate_codeunit(ch)
               ? output_utf8_4byte!(out, get_supplementary(ch, get_codeunit(pnt += sizeof(T))))
               : _encode_char_utf8(out, ch))
        pnt += sizeof(T)
    end
    buf
end
_transcode_utf8(dat::Vector{<:WideCodeUnit}, len) = _transcode_utf8(pointer(dat), len)
