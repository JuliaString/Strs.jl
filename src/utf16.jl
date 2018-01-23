#=
UTF16Str and UCS2Str types (UTF-16 encoding and pure BMP UCS-2)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for UTF16String that used to be in Julia
=#

const _ascii_mask = 0xff80_ff80_ff80_ff80
const _latin_mask = 0xff00_ff00_ff00_ff00
const _trail_mask = 0xdc00_dc00_dc00_dc00
const _hi_bit_16  = 0x8000_8000_8000_8000

@inline _mask_surr(v)  = xor((v | v<<1 | v<<2 | v<<3 | v<<4 | v<<5) & _hi_bit_16, _hi_bit_16)
@inline _get_masked(qpnt) = _mask_surr(xor(unsafe_load(qpnt), _trail_mask))

function _length(::CodeUnitMulti, str::UTF16Str)
    (siz = sizeof(str)) == 0 && return 0
    siz == 2 && return 1
    cnt = siz>>>1
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) <= fin
        cnt -= count_ones(_get_masked(pnt))
    end
    pnt - CHUNKSZ == fin ? cnt : (cnt - count_ones(_get_masked(pnt) & _mask_bytes(siz)))
end

function isascii(str::T) where {T<:Union{Text2Str, UCS2Str, UTF16Str}}
    (siz = sizeof(str)) == 0 && return true
    siz < CHUNKSZ && return ((unsafe_load(_pnt64(str)) & _mask_bytes(siz)) & _ascii_mask) == 0

    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) <= fin
        (unsafe_load(pnt) & _ascii_mask) == 0 || return false
    end
    pnt - CHUNKSZ == fin || ((unsafe_load(pnt) & _mask_bytes(siz)) & _ascii_mask) == 0
end

function islatin(str::T) where {T<:Union{Text2Str, UCS2Str, UTF16Str}}
    (siz = sizeof(str)) == 0 && return true
    siz < CHUNKSZ && return ((unsafe_load(_pnt64(str)) & _mask_bytes(siz)) & _latin_mask) == 0

    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) <= fin
        (unsafe_load(pnt) & _latin_mask) == 0 || return false
    end
    pnt - CHUNKSZ == fin || ((unsafe_load(pnt) & _mask_bytes(siz)) & _latin_mask) == 0
end

# Check for any surrogate characters
function isbmp(str::UTF16Str)
    (siz = sizeof(str)) == 0 && return true
    siz < CHUNKSZ && return (_get_masked(_pnt64(str)) & _mask_bytes(siz)) == 0

    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) <= fin
        _get_masked(pnt) == 0 || return false
    end
    pnt - CHUNKSZ == fin || (_get_masked(pnt) & _mask_bytes(siz)) == 0
end

isascii(str::_UCS2Str) = false
islatin(str::_UCS2Str) = false
isbmp(str::UCS2Strings) = true

# Speed this up accessing 64 bits at a time
function _cnt_non_bmp(len, pnt::Ptr{UInt16})
    cnt = 0
    @inbounds for i = 1:len
        cnt += is_surrogate_lead(get_codeunit(pnt, i))
    end
    cnt
end

@inline endof(str::UTF16Str) =
    ((len = _len(str)) != 0
     ? (is_surrogate_codeunit(get_codeunit(_pnt(str), len)) ? len-1 : len) : 0)

get_supplementary(lead::Unsigned, trail::Unsigned) = (UInt32(lead-0xd7f7)<<10 + trail)

function _next(::CodeUnitMulti, T, str::UTF16Str, pos::Int)
    @boundscheck pos <= _len(str) || boundserr(str, pos)
    pnt = _pnt(str)
    ch = get_codeunit(pnt, pos)
    (is_surrogate_lead(ch)
     ? (T(get_supplementary(ch, get_codeunit(pnt, pos + 1))), pos + 2)
     : (T(ch), pos + 1))
end

@inline function _thisind(::CodeUnitMulti, str::UTF16Str, pos::Int)
    @boundscheck 1 <= pos <= _len(str) || boundserr(str, pos)
    pos - is_surrogate_trail(get_codeunit(_pnt(str), pos))
end

@inline function _nextind(::CodeUnitMulti, str::UTF16Str, pos::Int)
    pos == 0 && return 1
    @boundscheck 1 <= pos <= _len(str) || boundserr(str, pos)
    pos + 1 + is_surrogate_lead(get_codeunit(_pnt(str), pos))
end

@inline function _prevind(::CodeUnitMulti, str::UTF16Str, pos::Int)
    (pos -= 1) == 0 && return 0
    numcu = _len(str)
    @boundscheck 0 < pos <= numcu || boundserr(str, pos + 1)
    pos - is_surrogate_trail(get_codeunit(_pnt(str), pos))
end

# Todo: _prevind with nchar argument
function _nextind(::CodeUnitMulti, str::UTF16Str, pos::Int, cnt::Int)
    cnt < 0 && neginderr(str, cnt)
    @boundscheck 0 <= pos <= _len(str) || boundserr(str, pos)
    cnt == 0 && return thisind(str, pos) == pos ? pos : unierror("Invalid position", str, pos)
    pos + cnt + is_surrogate_lead(get_codeunit(_pnt(str), pos + cnt))
end

function search(str::UCS2Strings, ch::UInt32, pos::Integer)
    pos == (len = _len(str)) + 1 && return 0
    pos <= pos <= len && boundserr(str, pos)
    # Check for invalid characters, which could not be in a UCS2Str
    ch <= 0x0ffff || return 0
    wrd = ch%UInt16
    is_surrogate_codeunit(wrd) && return 0
    beg = _pnt(str) - 2
    pnt = beg + (pos << 1)
    fin = beg + (len << 1)
    @inbounds while pnt <= fin
        get_codeunit(pnt) == wrd && return (pnt - beg)>>1
        pnt += 2
    end
    0
end

function rsearch(str::UCS2Strings, ch::UInt32, pos::Integer)
    len, pnt = _lenpnt(str)
    pos == len + 1 && return 0
    1 <= pos <= len && boundserr(str, pos)
    # Check for invalid characters, which could not be in a UCS2Str
    ch <= 0x0ffff || return 0
    wrd = ch%UInt16
    is_surrogate_codeunit(wrd) && return 0
    @inbounds while pos > 0
        get_codeunit(pnt, pos) == wrd && return pos
        pos -= 1
    end
    0
end

function search(str::UTF16Str, ch::UInt32, pos::Integer)
    len, pnt = _lenpnt(str)
    pos == len + 1 && return 0
    1 <= pos <= len && boundserr(str, pos)
    # Check for invalid characters, which could not be in a UTF16Str
    ch <= 0x010ffff || return 0
    is_surrogate_codeunit(ch) && return 0
    # Check for fast case, character in BMP
    if ch <= 0x0ffff
        wrd = ch%UInt16
        @inbounds while pos <= len
            get_codeunit(pnt, pos) == wrd && return pos
            pos += 1
        end
    else
        wrd  = (0xd7c0 + (ch >> 10))%UInt16
        surr = (0xdc00 + (ch & 0x3ff))%UInt16
        @inbounds while pos < len
            if get_codeunit(pnt, pos) == wrd
                get_codeunit(pnt, pos + 1) == surr && return pos
                pos += 1
            end
            pos += 1
        end
    end
    0
end

function rsearch(str::UTF16Str, ch::UInt32, pos::Integer)
    len, pnt = _lenpnt(str)
    pos == len + 1 && return 0
    1 <= pos <= len && boundserr(str, pos)
    # Check for invalid characters, which could not be in a UCS2Str
    ch <= 0x10ffff || return 0
    is_surrogate_codeunit(ch) && return 0
    # Check for fast case, character in BMP
    if ch <= 0x0ffff
        wrd = ch%UInt16
        @inbounds while pos > 0
            get_codeunit(pnt, pos) == wrd && return pos
            pos -= 1
        end
    else
        wrd  = (0xd7c0 + (ch >> 10))%UInt16
        surr = (0xdc00 + (ch & 0x3ff))%UInt16
        @inbounds while pos > 1
            get_codeunit(pnt, pos) == surr && get_codeunit(pnt, pos -= 1) == wrd && return pos
            pos -= 1
        end
    end
    0
end

function reverseind(str::UTF16Str, i::Integer)
    len, pnt = _lenpnt(str)
    j = len - i
    is_surrogate_trail(get_codeunit(pnt, j)) ? j - 1 : j
end

function reverse(str::UTF16Str)
    (len == _len(str)) == 0 && return str
    pnt = _pnt(str)
    buf, out = _allocate(UInt16, len)
    @inbounds for i = 1:len-1
        ch = get_codeunit(pnt, len - i)
        if is_surrogate_lead(ch)
            set_codeunit!(out, i,     get_codeunit(out, i - 1))
            set_codeunit!(out, i - 1, ch)
        else
            set_codeunit!(out, i,     ch)
        end
    end
    Str(UTF16CSE, buf)
end

function reverse(str::T) where {T<:UCS2Strings}
    (len = _len(str)) == 0 && return str
    pnt = _pnt(str)
    buf, out = _allocate(UInt16, len)
    @inbounds for i = 1:len
        set_codeunit!(out, i, get_codeunit(pnt, len - i + 1))
    end
    Str(cse(T), buf)
end

@inline _isvalid(::CodeUnitMulti, str::UTF16Str, i::Int) =
    (1 <= i <= _len(str)) && !is_surrogate_trail(get_codeunit(_pnt(str), i))

function isvalid(::Type{<:UCS2Strings}, data::AbstractArray{UInt16})
    @inbounds for ch in data
        is_surrogate_codeunit(ch) && return false
    end
    true
end

function isvalid(::Type{<:UCS2Strings}, pnt::Ptr{UInt16}, len)
    pos = 0
    @inbounds while (pos += 1) < len # check for surrogates
        is_surrogate_codeunit(get_codeunit(pnt, pos)) && return false
    end
    true
end

@inline function check_valid(ch, pos)
    is_surrogate_codeunit(ch) && unierror(UTF_ERR_SURROGATE, pos, ch)
    ch <= 0x10ffff || unierror(UTF_ERR_INVALID, pos, ch)
    ch
end

## outputting UCS2 strings ##

function write(io::IO, str::UCS2Strings)
    len, pnt = _lenpnt(str)
    cnt = 0
    @inbounds for i = 1:len
        cnt += _write_ucs2(io, get_codeunit(pnt))
        pnt += 2
    end
    cnt
end

function write(io::IO, str::UTF32Strings)
    len, pnt = _lenpnt(str)
    cnt = 0
    @inbounds for i = 1:len
        cnt += _write_utf32(io, get_codeunit(pnt))
        pnt += 4
    end
    cnt
end

## output UTF-16 string ##

function write(io::IO, str::UTF16Str)
    len, pnt = _lenpnt(str)
    # Skip and write out ASCII sequences together
    @inbounds for i = 1:len
        ch = get_codeunit(pnt)
        # Handle 0x80-0x7ff
        if ch <= 0x7f
            write(io, ch%UInt8)
        elseif ch <= 0x7ff
            _write_utf_2(io, ch)
        elseif is_surrogate_lead(ch)
            _write_utf_4(io, get_supplementary(ch, get_codeunit(pnt += 2)))
        else
            _write_utf_3(io, ch)
        end
        pnt += 2
    end
    len<<1
end

function convert(::Type{UTF16Str}, ch::UInt32)
    check_valid(ch, 0)
    if ch <= 0x0ffff
        buf, pnt = _allocate(UInt16, 1)
        set_codeunit!(pnt, ch%UInt16)
        Str(UCS2CSE, buf)
    else
        buf, pnt = _allocate(UInt16, 2)
        # output surrogate pair
        set_codeunit!(pnt,     (0xd7c0 + (ch >>> 10))%UInt16)
        set_codeunit!(pnt + 1, (0xdc00 + (ch & 0x3ff))%UInt16)
        Str(UTF16CSE, buf)
    end
end

function convert(::Type{UTF8Str}, ch::UInt32)
    check_valid(ch, 0)
    len = ch <= 0x7f ? 1 : (ch < 0x800 ? 2 : (ch > 0xffff ? 4 : 3))
    buf = _allocate(len)
    _encode_char_utf8(buf, ch, 0)
    Str(UTF8CSE, buf)
end

function convert(::Type{T}, ch::UInt32) where {T<:UCS2Strings}
    check_valid(ch, 0)
    ch <= 0x0ffff || unierror(UTF_ERR_INVALID_UCS2)
    buf, pnt = _allocate(UInt16, 1)
    set_codeunit!(pnt, 1, ch)
    Str(cse(T), buf)
end

function convert(::Type{T}, str::AbstractString) where {T<:UCS2Strings}
    isempty(str) && return empty_str(T)
    # Might want to have an invalids_as argument
    len, flags, num4byte = unsafe_checkstring(str)
    num4byte == 0 || unierror(UTF_ERR_INVALID_UCS2)
    buf, pnt = _allocate(UInt16, len)
    @inbounds for (i, ch) in enumerate(str)
        set_codeunit!(pnt, i, ch%UInt16)
    end
    Str(cse(T), buf)
end

function convert(::Type{T}, str::String) where {T<:UCS2Strings}
    # Might want to have an invalids_as argument
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_str(T)
    # Check that is correct UTF-8 encoding and get number of words needed
    len, flags, num4byte = unsafe_checkstring(str, 1, siz)
    num4byte == 0 || unierror(UTF_ERR_INVALID_UCS2)
    # Optimize case where no characters > 0x7f
    Str(cse(T), flags == 0 ? _cvtsize(UInt16, str, len) : _encode_utf16(str, len))
end

# handle zero length string quickly, just widen these
convert(::Type{T}, str::UnicodeByteStrings) where {T<:UCS2Strings} =
    (siz = sizeof(str)) == 0 ? empty_str(T) : Str(cse(T), _cvtsize(UInt16, _data(str), siz))

function convert(::Type{T}, str::UTF16Str) where {T<:UCS2Strings}
    # Might want to have an invalids_as argument
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_str(T)
    # Check if conversion is valid
    isbmp(str) || unierror(UTF_ERR_INVALID_UCS2)
    Str(cse(T), _cvtsize(UInt16, _pnt(str), len))
end

function isvalid(::Type{UTF16Str}, data::AbstractArray{UInt16})
    pos = 0
    len = length(data)
    @inbounds while (pos += 1) < len # check for unpaired surrogates
        ch = data[pos]
        (is_surrogate_codeunit(ch) &&
         is_surrogate_lead(ch) &&
         is_surrogate_trail(data[pos += 1])) || return false
    end
    pos > len || !is_surrogate_codeunit(data[pos])
end

# This can be sped up, to check 4 words at a time, only checking for unpaired
# or out of order surrogates when one is found in the UInt64
function isvalid(::Type{UTF16Str}, data::Ptr{UInt16}, len)
    i = 1
    @inbounds while i < len # check for unpaired surrogates
        ch = get_codeunit(data, i)
        if !is_surrogate_codeunit(ch)
            i += 1
        elseif is_surrogate_lead(ch) && is_surrogate_trail(get_codeunit(data, i+1))
            i += 2
        else
            return false
        end
    end
    i > len || !is_surrogate_codeunit(get_codeunit(data, i))
end

function convert(::Type{UTF16Str}, str::AbstractString)
    isempty(str) && return empty_utf16
    len, flags, num4byte = unsafe_checkstring(str)
    buf, pnt = _allocate(UInt16, len + num4byte)
    @inbounds for ch in str
        c = ch%UInt32
        if c > 0x0ffff
            # output surrogate pair
            set_codeunit!(pnt, (0xd7c0 + (c >>> 10))%UInt16)
            pnt += 2
            c = 0xdc00 + (c & 0x3ff)
        end
        set_codeunit!(pnt, c%UInt16)
        pnt += 2
    end
    Str(UTF16CSE, buf)
end

function convert(::Type{UTF16Str}, str::String)
    # handle zero length string quickly
    isempty(str) && return empty_utf16
    # Check that is correct UTF-8 encoding and get number of words needed
    len, flags, num4byte = unsafe_checkstring(str, 1, sizeof(str))
    # Optimize case where no characters > 0x7f
    Str(UTF16CSE, flags == 0 ? _cvtsize(UInt16, str, len) : _encode_utf16(str, len + num4byte))
end

function convert(::Type{UTF16Str}, str::UTF8Str)
    # handle zero length string quickly
    isempty(str) && return empty_utf16
    pnt = _pnt(str)
    len, flags, num4byte = count_chars(UTF8Str, pnt, _len(str))
    # Optimize case where no characters > 0x7f
    Str(UTF16CSE, flags == 0 ? _cvtsize(UInt16, pnt, len) : _encode_utf16(pnt, len + num4byte))
end

"""
Converts an already validated UTF-8 encoded vector of `UInt8` to a `UTF16Str`

Input Arguments:

*   `pnt` `Ptr{UInt8}` of UTF-8 encoded data
*   `len` length of output in 16-bit words

Returns:

*   `UTF16Str`
"""
function _encode_utf16(pnt::Ptr{UInt8}, len)
    buf, out = _allocate(UInt16, len)
    fin = out + (len<<1)
    @inbounds while out < fin
        ch = get_codeunit(pnt)%UInt16
        # Handle ASCII characters
        if ch <= 0x7f
        # Handle range 0x80-0x7ff
        elseif ch < 0xe0
            ch = ((ch & 0x1f) << 6) | (get_codeunit(pnt += 1) & 0x3f)
        # Handle range 0x800-0xffff
        elseif ch < 0xf0
            ch = get_utf8_3byte(pnt += 2, ch)
        # Handle range 0x10000-0x10ffff
        else
            ch32 = get_utf8_4byte(pnt += 3, ch)
            # output surrogate pair
            set_codeunit!(out, (0xd7c0 + (ch32 >>> 10))%UInt16)
            out += 2
            ch = (0xdc00 + (ch32 & 0x3ff))%UInt16
        end
        set_codeunit!(out, ch)
        out += 2
        pnt += 1
    end
    buf
end

_encode_utf16(dat::Vector{UInt8}, len) = _encode_utf16(pointer(dat), len)
_encode_utf16(str::String, len)        = _encode_utf16(_pnt(str), len)

@inline _cvt_16_to_utf8(::Type{UTF16Str}, pnt, len)       = _transcode_utf8(pnt, len)
@inline _cvt_16_to_utf8(::Type{<:UCS2Strings}, pnt, len)  = _encode_utf8(pnt, len)
@inline _cvt_16_to_utf8(::Type{<:UTF32Strings}, pnt, len) = _encode_utf8(pnt, len)

function _cvt_utf8(::Type{T}, str::S) where {T<:Union{String, UTF8Str}, S}
    # handle zero length string quickly
    (len = _len(str)) == 0 && return empty_str(T)
    # get number of bytes to allocate (use faster count for validated strings)
    pnt = _pnt(str)
    len, flags, num4byte, num3byte, num2byte, latin1 = count_chars(S, pnt, len)
    Str(cse(T), (flags == 0
                 ? _cvtsize(UInt8, pnt, len)
                 : _cvt_16_to_utf8(S, pnt, len + latin1 + num2byte + num3byte*2 + num4byte*3)))
end

# Split this way to avoid ambiguity errors
convert(::Type{String}, str::T) where {T<:Union{UCS2Strings, UTF16Str, UTF32Strings}} =
    _cvt_utf8(String, str)
convert(::Type{UTF8Str}, str::T) where {T<:Union{UCS2Strings, UTF16Str, UTF32Strings}} =
    _cvt_utf8(UTF8Str, str)

"""
Converts an already validated UTF-32 encoded vector of `UInt32` to a `UTF16Str`

Input Arguments:

*   `dat` `Vector{UInt32}` of UTF-32 encoded data
*   `len` length of output in 16-bit words

Returns:

*   `Vector{UInt8}`
"""
function _encode_utf16(dat::Ptr{UInt32}, len)
    buf, pnt = _allocate(UInt16, len)
    fin = pnt + (len<<1)
    @inbounds while pnt < fin
        ch = get_codeunit(dat)
        dat += 4
        if ch > 0x0ffff
            # Output surrogate pair for 0x10000-0x10ffff
            set_codeunit!(pnt, (0xd7c0 + (ch >>> 10))%UInt16)
            pnt += 2
            ch = 0xdc00 + (ch & 0x3ff)
        end
        set_codeunit!(pnt, ch%UInt16)
        pnt += 2
    end
    buf
end

# Copies because not safe to expose the internal array (would allow mutation)
function convert(::Type{Vector{UInt16}}, str::WordStr)
    len = _len(str)
    vec = create_vector(UInt16, len)
    @inbounds unsafe_copyto!(pointer(vec), _pnt(str), len)
    vec
end

convert(::Type{T},  str::S) where {T<:UCS2Strings, S<:UCS2Strings} = str
convert(::Type{UTF16Str}, str::UTF16Str) = str
convert(::Type{UTF16Str}, str::UCS2Strings) = Str(UTF16CSE, str.data)

unsafe_convert(::Type{Ptr{UInt16}}, s::UTF16Str) = _pnt(s)

function convert(::Type{UTF16Str}, dat::AbstractVector{UInt16})
    isempty(dat) && return empty_utf16
    len, flags, num4byte = unsafe_checkstring(dat, 1, endof(dat))
    # Optimize case where no surrogate characters
    Str(UTF16CSE, flags == 0 ? _cvtsize(UInt16, dat, len) : _encode_utf16(dat, len + num4byte))
end

_convert(pnt::Ptr{T}, len, T1) where {T<:Union{UInt16,UInt16_U,UInt16_S,UInt16_US}} =
    ((ch = unsafe_load(pnt)) == 0xfffe
     ? _convert(reinterpret(Ptr{T1}, pnt + 2), len - 1)
     : (ch == 0xfeff ? _convert(pnt + 2, len - 1) : _convert(pnt, len)))

function _convert(pnt::Ptr{T}, len) where {T}
    buf, out = _allocate(basetype(T), len)
    @inbounds for i in 1:len
        set_codeunit!(out, i, unsafe_load(pnt))
        pnt += sizeof(T)
    end
    buf, out
end

function convert(::Type{UTF16Str}, bytes::AbstractArray{UInt8})
    isempty(bytes) && return empty_utf16
    # Note: there are much better ways of detecting what the likely encoding is here,
    # this only deals with big or little-ending UTF-32
    # It really should detect at a minimum UTF-8, UTF-16 big and little
    len = length(bytes)
    isodd(len) && unierror(UTF_ERR_ODD_BYTES_16, len, 0)
    len >>>= 1
    pnt = pointer(bytes)
    if isodd(reinterpret(UInt, pnt))
        buf, out = _convert(reinterpret(Ptr{UInt16_U}, pnt), len, swappedtype(UInt16_U))
    else
        buf, out = _convert(reinterpret(Ptr{UInt16}, pnt), len, swappedtype(UInt16))
    end
    isvalid(UTF16Str, out, len) || unierror(UTF_ERR_INVALID, 0, 0)
    Str(UTF16CSE, buf)
end

@inline function pushchar!(rst, uc)
    if uc <= 0x0ffff
        push!(rst, uc%UInt16)
    else
        push!(rst, (0xd7c0 + (uc >> 10))%UInt16)
        push!(rst, (0xdc00 + (uc & 0x3ff))%UInt16)
    end
end

"""Handle case where result vector is longer"""
function _maprest(fun, str, len, pnt, fin, buf, out, uc)
    rst = Vector{UInt16}()
    pushchar!(rst, uc)
    while pnt < fin
        ch = get_codeunit(pnt)%UInt32
        # check for surrogate pair
        is_surrogate_lead(ch) &&
            (ch = get_supplementary(ch, get_codeunit(pnt += 2)))
        pushchar!(rst, check_valid(UInt32(fun(ch)), (pnt - pointer(str))>>>1))
        pnt += 2
    end
    # We now have a vector to add to the end of buf
    lenrst = length(rst)
    totbuf, totpnt = _allocate(UInt16, len + lenrst)
    unsafe_copyto!(totpnt, 1, pnt, 1, (out - pointer(buf))>>>1)
    unsafe_copyto!(totpnt, out + 1, pointer(rst), 1, lenrst)
    Str(UTF16CSE, totbuf)
end

function map(fun, str::T) where {T<:Union{UCS2Str,_UCS2Str,UTF16Str}}
    (len = _len(str)) == 0 && return empty_str(T)
    pnt = _pnt(str)
    buf, out = _allocate(UInt16, len)
    surrflag = false
    fin = pnt + sizeof(str)
    outend = out + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)%UInt32
        # check for surrogate pair
        T == UTF16Str && is_surrogate_lead(ch) &&
            (ch = get_supplementary(ch, get_codeunit(pnt += 2)))
        # Note: no checking for invalid here, UTF16Str is always valid
        uc = check_valid(UInt32(fun(ch)), (pnt - pointer(str))>>>1)
        if uc < 0x10000
            out < outend || return _maprest(fun, str, len, buf, pnt, fin, out, uc)
            set_codeunit!(out, uc%UInt16)
            out += 2
        else
            out + 2 < outend || return _maprest(fun, str, len, buf, pnt, fin, out, uc)
            set_codeunit!(out,     (0xd7c0 + (uc >> 10))%UInt16)
            set_codeunit!(out + 2, (0xdc00 + (uc & 0x3ff))%UInt16)
            surrflag = true
            out += 4
        end
        pnt += 2
    end
    out < outend && resize!(buf, out - pointer(buf))
    if !surrflag
        Str(cse(T), buf)
    elseif T == _UCS2Str
        # Convert to 32-bit, to keep result in UniStr type union
        # TODO: check this
        convert(_UTF32Str, Str(UTF16CSE, buf))
    else
        Str(UTF16CSE, buf)
    end
end
