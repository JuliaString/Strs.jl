#=
UTF8Str type

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones,
and other contributors to the Julia language

Licensed under MIT License, see LICENSE.md
Based in part on code for UTF8String that used to be in Julia
=#

# UTF-8 support functions

const MS_UTF8 = MaybeSub{<:Str{UTF8CSE}}

# Get rest of character ch from 2-byte UTF-8 sequence at pnt - 1
@inline get_utf8_2byte(pnt, ch) =
    (((ch & 0x3f)%UInt16 << 6) | (get_codeunit(pnt) & 0x3f))

# Get rest of character ch from 3-byte UTF-8 sequence at pnt - 2
@inline get_utf8_3byte(pnt, ch) =
    (((ch & 0xf)%UInt16 << 12)
     | ((get_codeunit(pnt - 1)%UInt16 & 0x3f) << 6)
     | (get_codeunit(pnt) & 0x3f))

# Get rest of character ch from 4-byte UTF-8 sequence at pnt - 3
@inline get_utf8_4byte(pnt, ch) =
    (((ch & 0x7)%UInt32 << 18)
     | ((get_codeunit(pnt - 2)%UInt32 & 0x3f) << 12)
     | ((get_codeunit(pnt - 1)%UInt32 & 0x3f) << 6)
     | (get_codeunit(pnt) & 0x3f))

@inline get_utf8_2(ch) =
    0xc0 | (ch >>> 6)%UInt8, 0x80 | (ch & 0x3f)%UInt8
@inline get_utf8_3(ch) =
    (0xe0 | (ch >>> 12)%UInt8, 0x80 | ((ch >>> 6) & 0x3f)%UInt8, 0x80 | (ch & 0x3f)%UInt8)
@inline get_utf8_4(ch) =
    (0xf0 | (ch >>>  18)%UInt8, 0x80 | ((ch >>> 12) & 0x3f)%UInt8,
     0x80 | ((ch >>>  6) & 0x3f)%UInt8, 0x80 | (ch & 0x3f)%UInt8)

# Output a character as a 2-byte UTF-8 sequence
@inline function output_utf8_2byte!(pnt, ch)
    b1, b2 = get_utf8_2(ch)
    set_codeunit!(pnt,     b1)
    set_codeunit!(pnt + 1, b2)
    pnt + 2
end

# Output a character as a 3-byte UTF-8 sequence
@inline function output_utf8_3byte!(pnt, ch)
    b1, b2, b3 = get_utf8_3(ch)
    set_codeunit!(pnt,     b1)
    set_codeunit!(pnt + 1, b2)
    set_codeunit!(pnt + 2, b3)
    pnt + 3
end

# Output a character as a 4-byte UTF-8 sequence
@inline function output_utf8_4byte!(pnt, ch)
    b1, b2, b3, b4 = get_utf8_4(ch)
    set_codeunit!(pnt,     b1)
    set_codeunit!(pnt + 1, b2)
    set_codeunit!(pnt + 2, b3)
    set_codeunit!(pnt + 3, b4)
    pnt + 4
end

@inline eq_bytes(pnt, b1)         = get_codeunit(pnt) == b1
@inline eq_bytes(pnt, b1, b2)     = get_codeunit(pnt+1) == b2 && eq_bytes(pnt, b1)
@inline eq_bytes(pnt, b1, b2, b3) = get_codeunit(pnt+2) == b3 && eq_bytes(pnt, b1, b2)

## required core functionality ##

utf_trail(c::UInt8) = (0xe5000000 >>> ((c & 0xf0) >> 3)) & 0x3

function lastindex(str::MS_UTF8)
    (len = _len(str)) == 0 && return len
    @preserve str begin
        beg = _pnt(str)
        pnt = beg + len
        while is_valid_continuation(get_codeunit(pnt -= 1)) ; end
        Int(pnt + 1 - beg)
    end
end

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

function _length(::CodeUnitMulti, ::Type{UTF8CSE}, str::Str)
    (siz = sizeof(str)) < 2 && return siz
    @preserve str begin
        pnt, fin = _calcpnt(str, siz)
        cnt = siz
        while (pnt += CHUNKSZ) <= fin
            cnt -= _count_cont(unsafe_load(pnt))
        end
        siz & (CHUNKSZ-1) == 0 ? cnt : (cnt - _count_cont(unsafe_load(pnt) & _mask_bytes(siz)))
    end
end

function _length(::CodeUnitMulti, ::Type{UTF8CSE}, str, i::Int, j::Int)
    # Count number of valid codepoints within the range of the codeunits at i:j
    @preserve str begin
        cnt = j - i + 1
        beg = _pnt(str)
        align = reinterpret(UInt, beg)
        pnt = reinterpret(Ptr{UInt64}, align & ~(CHUNKSZ-1))
        align &= (CHUNKSZ-1)
        siz = cnt - align
        if align != 0
            v = unsafe_load(pnt) >>> (align*8)
            # v has 1-7 bytes, (8-align)
            # May be split into more than one word
            if cnt < (16 - align)
                cnt < (8 - align) || (cnt -= _count_cont(v) ; v = unsafe_load(pnt + CHUNKSZ))
                return cnt - _count_cont(v & _mask_bytes(siz))
            end
        else
            v = unsafe_load(pnt)
        end
        fin = pnt + cnt
        while (pnt += CHUNKSZ) < fin
            cnt -= _count_cont(v)
            v = unsafe_load(pnt)
        end
        cnt -= _count_cont(siz & (CHUNKSZ-1) == 0 ? v : (v & _mask_bytes(siz)))
    end
end

function is_ascii(str::Str{<:Union{UTF8CSE, LatinCSE, Binary_CSEs}})
    (siz = sizeof(str)) == 0 && return true
    @preserve str begin
        pnt, fin = _calcpnt(str, siz)
        while (pnt += CHUNKSZ) <= fin
            (unsafe_load(pnt) & hi_mask) == 0 || return false
        end
        pnt - CHUNKSZ == fin || ((unsafe_load(pnt) & _mask_bytes(siz)) & hi_mask == 0)
    end
end

_ascii_mask(::Type{UInt8})  = hi_mask
_ascii_mask(::Type{UInt16}) = 0xff80_ff80_ff80_ff80
_ascii_mask(::Type{UInt32}) = 0xffffff80_ffffff80

_latin_mask(::Type{UInt16}) = 0xff00_ff00_ff00_ff00
_latin_mask(::Type{UInt32}) = 0xffffff00_ffffff00

function is_ascii(vec::Vector{T}) where {T<:CodeUnitTypes}
    (siz = sizeof(vec)) == 0 && return true
    @preserve vec begin
        pnt = pointer(vec)
        fin = pnt + siz
        # Check first code units
        while (reinterpret(UInt, pnt) & (CHUNKSZ-1)) != 0
            unsafe_load(pnt) > 0x7f && return false
            (pnt += sizeof(T)) < fin || return true
        end
        if (fin -= CHUNKSZ) - pnt >= 0
            qpnt = reinterpret(Ptr{UInt64}, pnt)
            while true
                (unsafe_load(qpnt) & _ascii_mask(T)) == 0 || return false
                (qpnt += CHUNKSZ) <= fin || break
            end
            qpnt - CHUNKSZ == fin && return true
            pnt = reinterpret(Ptr{T}, qpnt)
        end
        while pnt < fin
            unsafe_load(pnt) > 0x7f && return false
            pnt += sizeof(T)
        end
    end
    true
end

# Todo! Here you need to see that 0b11yyyyxx at least 1 y must be set,
# which indicates a non-Latin1 character
_all_latin(val) = ((val & (val<<1) & (val<<2 | (val<<3) | (val<<4) | (val<<5))) & hi_mask) == 0

function is_latin(str::Str{<:Union{UTF8CSE, Binary_CSEs}})
    (siz = sizeof(str)) == 0 && return true
    @preserve str begin
        pnt, fin = _calcpnt(str, siz)
        while (pnt += CHUNKSZ) <= fin
            _all_latin(unsafe_load(pnt)) || return false
        end
        pnt - CHUNKSZ == fin || _all_latin(unsafe_load(pnt) & _mask_bytes(siz))
    end
end

# All 4 top bits must be 1 (i.e. 0xfx) for this to be non-BMP
_all_bmp(val) = ((val | (val<<1) | (val<<2) | (val<<3)) & hi_mask) == 0

function is_bmp(str::Str{UTF8CSE})
    (siz = sizeof(str)) == 0 && return true
    @preserve str begin
        pnt, fin = _calcpnt(str, siz)
        while (pnt += CHUNKSZ) <= fin
            _all_bmp(unsafe_load(pnt)) || return false
        end
        pnt - CHUNKSZ == fin && _all_bmp(unsafe_load(pnt) & _mask_bytes(siz))
    end
end

is_unicode(str::Str{UTF8CSE}) = true

function _nextcpfun(::CodeUnitMulti, ::Type{UTF8CSE}, pnt)
    ch = get_codeunit(pnt)
    if ch < 0x80
        ch%UInt32, pnt + 1
    elseif ch < 0xe0
        get_utf8_2byte(pnt + 1, ch)%UInt32, pnt + 2
    elseif ch < 0xf0
        get_utf8_3byte(pnt + 2, ch)%UInt32, pnt + 3
    else
        get_utf8_4byte(pnt + 3, ch), pnt + 4
    end
end

# Gets next codepoint
@propagate_inbounds function _next(::CodeUnitMulti, ::Type{T},
                                   str::MS_UTF8, pos::Int) where {T<:CodePoint}
    len = _len(str)
    @boundscheck 0 < pos <= len || boundserr(str, pos)
    @preserve str begin
        pnt = _pnt(str) + pos - 1
        ch = get_codeunit(pnt)
        if ch < 0x80
            T(ch), pos + 1
        elseif ch < 0xe0
            T(get_utf8_2byte(pnt + 1, ch)), pos + 2
        elseif ch < 0xf0
            T(get_utf8_3byte(pnt + 2, ch)), pos + 3
        else
            T(get_utf8_4byte(pnt + 3, ch)), pos + 4
        end
    end
end

done(str::Str{UTF8CSE}, i::Int) = i > sizeof(str.data)

length(it::CodePoints{<:AbstractString}, i::Int) = length(it.xs)

@propagate_inbounds function next(it::CodePoints{<:AbstractString}, state)
    ch, state = next(it.xs, state)
    UTF32Chr(ch%UInt32), state
end

@inline first_utf8_byte(ch::UInt32) =
    (ch > 0x7f
     ? (ch > 0x7ff ? (ch > 0xffff ? ((ch>>18) | 0xf0) : ((ch>>12) | 0xe0)) : ((ch>>6) | 0xc0))
     : ch)%UInt8

@inline checkcont(pnt) = is_valid_continuation(get_codeunit(pnt))

function _reverseind(::CodeUnitMulti, str::MS_UTF8, pos::Integer)
    @preserve str begin
        pnt = _pnt(str) + _len(str) + 1 - pos
        pos - (checkcont(pnt) ? (checkcont(pnt - 1) ? checkcont(pnt - 2) + 2 : 1) : 0)
    end
end

## overload methods for efficiency ##

@inline _isvalid_char_pos(::CodeUnitMulti, ::Type{UTF8CSE}, str, pos::Integer) =
    !is_valid_continuation(get_codeunit(_pnt(str) + pos - 1))

@inline function _thisind_utf8(str, len, pnt, pos::Int)
    @boundscheck 0 < pos <= len || boundserr(str, pos)
    pnt += pos - 1
    pos - (checkcont(pnt) ? (checkcont(pnt - 1) ? checkcont(pnt - 2) + 2 : 1) : 0)
end

@propagate_inbounds _thisind(::CodeUnitMulti, str::MS_UTF8, len, pnt, pos::Integer) =
    _thisind_utf8(str, len, pnt, Int(pos))
@propagate_inbounds _thisind(::CodeUnitMulti, str::String, len, pnt, pos::Integer) =
    _thisind_utf8(str, len, pnt, Int(pos))

@propagate_inbounds function _nextind(::CodeUnitMulti, str::MS_UTF8, pos::Integer)
    pos == 0 && return 1
    numcu = _len(str)
    @boundscheck 1 <= pos <= numcu || boundserr(str, pos)
    @preserve str begin
        pnt = _pnt(str) + pos - 1
        cu = get_codeunit(pnt)
        pos + (cu < 0x80 ? 1
               : (cu < 0xc0
                  ? (pos == numcu ? 1
                     : (checkcont(pnt + 1) ? (2 + (pos < numcu - 1 && checkcont(pnt + 2))) : 1))
                  : ifelse(cu < 0xe0, 2, ifelse(cu < 0xf0, 3, 4))))
    end
end

@propagate_inbounds function _prevind(::CodeUnitMulti, str::MS_UTF8, pos::Integer)
    (pos -= 1) == 0 && return 0
    @preserve str begin
        numcu, pnt = _lenpnt(str)
        pnt += pos
        pos == numcu &&
            return pos - (checkcont(pnt-1) ? (checkcont(pnt-2) ? checkcont(pnt-3) + 2 : 1) : 0)
        @boundscheck 0 < pos < numcu || boundserr(str, pos+1)
        if checkcont(pnt)
            pos - (checkcont(pnt - 1) ? checkcont(pnt - 2) + 1 : 0)
        elseif pos != 1
            pos - (checkcont(pnt - 1) ? (checkcont(pnt - 2) ? checkcont(pnt - 3) + 2 : 1) : 0)
        else
            1
        end
    end
end

@propagate_inbounds function _nextind(::CodeUnitMulti, str::MS_UTF8, pos::Int, nchar::Int)
    nchar < 0 && ncharerr(nchar)
    siz = ncodeunits(str)
    @boundscheck 0 <= pos <= siz || boundserr(str, pos)
    siz == 0 && return ifelse(nchar == 0, 0, 1)
    pos == 0 && (pos = 1)
    @preserve str begin
        beg = _pnt(str)
        pnt = beg + pos - 1
        fin = beg + siz
        nchar == 0 && (checkcont(pnt) ? index_error(str, pos) : return pos)
        cu = get_codeunit(pnt)
        pnt += (cu < 0x80 ? 1
                : (cu < 0xc0
                   ? (pos == siz ? 1
                      : (checkcont(pnt + 1) ? (2 + (pos < siz - 1 && checkcont(pnt + 2))) : 1))
                   : ifelse(cu < 0xe0, 2, ifelse(cu < 0xf0, 3, 4))))
        # pnt should now point to a valid start of a character
        # This could be sped up, by looking at chunks, and if all ASCII (common case),
        # simply move forward 8
        while (nchar -= 1) > 0 && pnt < fin
            pnt += utf_trail(get_codeunit(pnt)) + 1
        end
        Int(pnt - beg)
    end
end

@propagate_inbounds function _prevind(::CodeUnitMulti, str::MS_UTF8, pos::Int, nchar::Int)
    nchar < 0 && ncharerr(nchar)
    @boundscheck 0 < pos <= ncodeunits(str)+1 || boundserr(str, pos)
    @preserve str begin
        beg = _pnt(str)
        pnt = beg + pos
        nchar == 0 && (checkcont(pnt-1) ? index_error(str, pos) : return pos)
        # This could be sped up, by looking at chunks, and if all ASCII (common case),
        # simply move back 8
        while (pnt -= 1) >= beg && (nchar -= !checkcont(pnt)) > 0 ; end
        Int(pnt - beg + 1)
    end
end

@propagate_inbounds function getindex(str::MS_UTF8, rng::UnitRange{Int})
    isempty(rng) && return SubString(empty_utf8, 1, 0)
    beg = first(rng)
    len = _len(str)
    @boundscheck 1 <= beg <= len || boundserr(str, beg)
    @preserve str begin
        pnt = _pnt(str)
        ch = get_codeunit(pnt, beg)
        is_valid_continuation(ch) && unierror(UTF_ERR_INVALID_INDEX, beg, ch)
        lst = last(rng)
        @boundscheck lst > len && boundserr(str, lst)
        if lst != len
            ch = get_codeunit(pnt, lst)
            is_valid_continuation(ch) && unierror(UTF_ERR_INVALID_INDEX, lst, ch)
        end
    end
    SubString(str, beg, lst)
end

const _ByteStr = Union{Str{ASCIICSE}, SubString{<:Str{ASCIICSE}},
                       Str{UTF8CSE},  SubString{<:Str{UTF8CSE}},
                       String}

string(c::_ByteStr...) = length(c) == 1 ? c[1]::UTF8Str : UTF8Str(_string(c))
    # ^^ at least one must be UTF-8 or the ASCII-only method would get called

function _reverse(::CodeUnitMulti, ::Type{UTF8CSE}, len, pnt::Ptr{T}) where {T<:CodeUnitTypes}
    buf, beg = _allocate(T, len)
    out = beg + len
    while out >= beg
        ch = get_codeunit(pnt)
        if ch > 0xdf
            if ch < 0xf0
                set_codeunit!(out -= 2, get_codeunit(pnt += 1))
                set_codeunit!(out + 1, get_codeunit(pnt += 1))
            else
                set_codeunit!(out -= 3, get_codeunit(pnt += 1))
                set_codeunit!(out + 1,  get_codeunit(pnt += 1))
                set_codeunit!(out + 2,  get_codeunit(pnt += 1))
            end
        elseif ch > 0x7f
            set_codeunit!(out -= 1, get_codeunit(pnt += 1))
        end
        set_codeunit!(out -= 1, ch)
        pnt += 1
    end
    Str(UTF8CSE, buf)
end

@inline get_ch(dat, pos, off) = (get_codeunit(dat, pos + off) & 0x3f)%UInt32

## transcoding to UTF-8 ##

@propagate_inbounds function _transcode_utf8(pnt::Ptr{UInt8}, len)
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

# Single character conversion
function convert(::Type{<:Str{UTF8CSE}}, ch::Unsigned)
    is_unicode(ch) || unierror(UTF_ERR_INVALID, 0, ch)
    len = ch <= 0x7f ? 1 : (ch < 0x800 ? 2 : (ch > 0xffff ? 4 : 3))
    buf = _allocate(len)
    _encode_char_utf8(buf, ch, 0)
    Str(UTF8CSE, buf)
end

convert(::Type{UTF8Str}, s::Str{UTF8CSE}) = s
convert(::Type{UTF8Str}, s::ASCIIStr) = Str(UTF8CSE, s.data)

# Note: this will have to change back to s.endof for v0.6!
convert(::Type{SubString{UTF8Str}}, s::SubString{ASCIIStr}) =
    SubString(convert(UTF8Str, s.string), s.offset + 1, s.offset + s.ncodeunits)

function convert(::Type{UTF8Str}, dat::Vector{UInt8})
    # handle zero length string quickly
    isempty(dat) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte = unsafe_check_string(dat, 1, _len(dat))
    # Copy, but eliminate over-long encodings and surrogate pairs
    if flags & (UTF_LONG | UTF_SURROGATE) == 0
        siz = sizeof(dat)
        buf = _allocate(siz)
        unsafe_copyto!(pointer(buf), pointer(dat), siz)
        Str(UTF8CSE, buf)
    else
        Str(UTF8CSE, _transcode_utf8(dat, len + latinbyte + num2byte + num3byte*2 + num4byte*3))
    end
end

function convert(::Type{UTF8Str}, str::String)
    # handle zero length string quickly
    isempty(str) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte = unsafe_check_string(str, 1, sizeof(str))
    # Copy, but eliminate over-long encodings and surrogate pairs
    # Speed this up if no surrogates, long encodings
    Str(UTF8CSE,
        (flags & (UTF_LONG | UTF_SURROGATE) == 0
         ? str
         : _transcode_utf8(_pnt(str), len + latinbyte + num2byte + num3byte*2 + num4byte*3)))
end

function convert(::Type{UTF8Str}, str::AbstractString)
    # handle zero length string quickly
    isempty(str) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte = unsafe_check_string(str)
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
        output_utf8_2byte!(pnt, ch)
    # Handle 0x10000-0x10ffff
    elseif ch > 0xffff # this is only for T == UInt32, should not be generated for UInt16
        output_utf8_4byte!(pnt, ch)
    # Handle 0x800-0xd7ff, 0xe000-0xffff UCS-2 characters
    else
        output_utf8_3byte!(pnt, ch)
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
