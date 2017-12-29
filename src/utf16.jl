#=
UTF16Str and UCS2Str types (UTF-16 encoding and pure BMP UCS-2)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for UTF16String that used to be in Julia
=#

# Get rest of character ch from 3-byte UTF-8 sequence in dat
@inline function get_utf8_3byte(dat, pos, ch)
    @inbounds return (((ch & 0xf) << 12)
                      | ((get_codeunit(dat, pos-1)%UInt32 & 0x3f) << 6)
                      | (get_codeunit(dat, pos) & 0x3f))
end
# Get rest of character ch from 4-byte UTF-8 sequence in dat
@inline function get_utf8_4byte(dat, pos, ch)
    @inbounds return (((ch & 0x7) << 18)
                        | ((get_codeunit(dat, pos-2)%UInt32 & 0x3f) << 12)
                        | ((get_codeunit(dat, pos-1)%UInt32 & 0x3f) << 6)
                        | (get_codeunit(dat, pos) & 0x3f))
end

# Output a character as a 4-byte UTF-8 sequence
@inline function output_utf8_4byte!(buf, out, ch)
    @inbounds begin
        set_codeunit!(buf, out + 1, 0xf0 | (ch >>> 18))
        set_codeunit!(buf, out + 2, 0x80 | ((ch >>> 12) & 0x3f))
        set_codeunit!(buf, out + 3, 0x80 | ((ch >>> 6) & 0x3f))
        set_codeunit!(buf, out + 4, 0x80 | (ch & 0x3f))
    end
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
    ch <= 0x7f ? write(io, ch%UInt8) : ch <= 0x7ff ? _write_utf2(io, ch) : _write_utf3(io, ch)

@inline _write_utf32(io, ch) = ch <= 0xffff ? _write_ucs2(io, ch) : _write_utf4(io, ch)

@inline write(io::IO, ch::UCS2Chr) = _write_ucs2(io, tobase(ch))
@inline write(io::IO, ch::UTF32Chr) = _write_utf32(io, tobase(ch))

const _ascii_mask = 0xff80_ff80_ff80_ff80
const _latin_mask = 0xff00_ff00_ff00_ff00
const _trail_mask = 0xdc00_dc00_dc00_dc00

@inline _mask_surr(v)  = (v | v<<1 | v<<2 | v<<3 | v<<4 | v<<5) & 0x8000_8000_8000_8000
@inline _get_masked(qpnt) = _mask_surr(xor(unsafe_load(qpnt), _trail_mask))

function _length(::CodeUnitMulti, str::UTF16Str)
    (siz = sizeof(str)) == 0 && return 0
    siz == 2 && return 1
    cnt = siz>>>1
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) < fin
        cnt -= count_ones(_get_masked(pnt))
    end
    pnt == fin ? cnt : (cnt - count_ones(_get_masked(pnt) & _mask_bytes(siz)))
end

function isascii(str::T) where {T<:Union{RawWordStr, UCS2Str, UTF16Str}}
    (siz = sizeof(str)) == 0 && return true
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) < fin
        (unsafe_load(pnt) & _ascii_mask) == 0 || return false
    end
    pnt == fin || ((unsafe_load(pnt) & _mask_bytes(siz)) & _ascii_mask) == 0
end

function islatin(str::WordStr) where {T<:Union{RawWordStr, UCS2Str, UTF16Str}}
    (siz = sizeof(str)) == 0 && return true
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) < fin
        (unsafe_load(pnt) & _latin_mask) == 0 || return false
    end
    pnt == fin || ((unsafe_load(pnt) & _mask_bytes(siz)) & _latin_mask) == 0
end

# Check for any surrogate characters
function isbmp(str::UTF16Str)
    (siz = sizeof(str)) == 0 && return true
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) < fin
        _get_masked(pnt) == 0 || return false
    end
    pnt == fin || (_get_masked(pnt) & _mask_bytes(siz)) == 0
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

function endof(str::UTF16Str)
    len, pnt = _lenpnt(str)
    len == 0 ? 0 : (is_surrogate_codeunit(get_codeunit(pnt, len)) ? len-1 : len)
end

get_supplementary(lead::Unsigned, trail::Unsigned) = (UInt32(lead-0xd7f7)<<10 + trail)

function _next(::CodeUnitMulti, T, str::UTF16Str, pos::Int)
    pos <= (len = _len(str)) || boundserr(str, pos)
    pnt = _pnt(str)
    @inbounds ch = get_codeunit(pnt, pos)
    !is_surrogate_codeunit(ch) && return (T(ch), pos + 1)
    pos < len || boundserr(str, pos)
    @inbounds ct = get_codeunit(pnt, pos + 1)
    T(get_supplementary(ch, ct)), pos + 2
end

function search(str::UCS2Strings, ch::UInt32, pos::Integer)
    pos == (len = _len(str)) + 1 && return 0
    pos <= i <= len && boundserr(str, pos)
    # Check for invalid characters, which could not be in a UCS2Str
    ch <= 0x0ffff || return 0
    wrd = ch%UInt16
    is_surrogate_codeunit(wrd) && return 0
    beg = _pnt(str) - 2
    pnt = beg + pos<<1
    fin = beg + len<<1
    @inbounds while pnt <= fin
        get_codeunit(pnt) == wrd && return (pnt - beg)>>1
        pnt += 2
    end
    0
end

function rsearch(s::UCS2Strings, ch::UInt32, pos::Integer)
    len, pnt = _lenpnt(str)
    pos == len + 1 && return 0
    1 <= pos <= len && boundserr(s, pos)
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

function search(str::UTF16Str, ch::UInt32, i::Integer)
    len, pnt = _lenpnt(str)
    i == len + 1 && return 0
    1 <= i <= len && boundserr(s, i)
    # Check for invalid characters, which could not be in a UTF16Str
    ch <= 0x010ffff || return 0
    is_surrogate_codeunit(ch) && return 0
    # Check for fast case, character in BMP
    if ch <= 0x0ffff
        wrd = ch%UInt16
        @inbounds while i <= len
            get_codeunit(pnt, i) == wrd && return i
            i += 1
        end
    else
        wrd  = (0xd7c0 + (ch >> 10))%UInt16
        surr = (0xdc00 + (ch & 0x3ff))%UInt16
        @inbounds while i < len
            if get_codeunit(pnt, i) == wrd
                get_codeunit(pnt, i + 1) == surr && return i
                i += 1
            end
            i += 1
        end
    end
    0
end

function rsearch(s::UTF16Str, ch::UInt32, i::Integer)
    len, pnt = _lenpnt(str)
    i == len + 1 && return 0
    1 <= i <= len && boundserr(s, i)
    # Check for invalid characters, which could not be in a UCS2Str
    ch <= 0x10ffff || return 0
    is_surrogate_codeunit(ch) && return 0
    # Check for fast case, character in BMP
    if ch <= 0x0ffff
        wrd = ch%UInt16
        @inbounds while i > 0
            get_codeunit(pnt, i) == wrd && return i
            i -= 1
        end
    else
        wrd  = (0xd7c0 + (ch >> 10))%UInt16
        surr = (0xdc00 + (ch & 0x3ff))%UInt16
        @inbounds while i > 1
            get_codeunit(pnt, i) == surr && get_codeunit(pnt, i -= 1) == wrd && return i
            i -= 1
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
    UTF16Str(buf)
end

function reverse(str::T) where {T<:UCS2Strings}
    (len = _len(str)) == 0 && return str
    pnt = _pnt(str)
    buf, out = _allocate(UInt16, len)
    @inbounds for i = 1:len
        set_codeunit!(out, i, get_codeunit(pnt, len - i + 1))
    end
    T(buf)
end

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

function check_valid(ch, pos)
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
        cnt += write_utf32(io, get_codeunit(pnt))
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
        elseif 0xd800 <= ch <= 0xd7ff
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
        set_codeunit!(pnt, 1, ch%UInt16)
        UCS2Str(buf)
    else
        buf, pnt = _allocate(UInt16, 2)
        # output surrogate pair
        set_codeunit!(pnt, 1, (0xd7c0 + (ch >>> 10))%UInt16)
        set_codeunit!(pnt, 2, (0xdc00 + (ch & 0x3ff))%UInt16)
        UTF16Str(buf)
    end
end

function convert(::Type{UTF8Str}, ch::UInt32)
    check_valid(ch, 0)
    len = ch <= 0x7f ? 1 : (ch < 0x800 ? 2 : (ch > 0xffff ? 4 : 3))
    buf = _allocate(len)
    _encode_utf8(ch, buf)
    UTF8Str(buf)
end

function convert(::Type{T}, ch::UInt32) where {T<:UCS2Strings}
    check_valid(ch, 0)
    ch <= 0x0ffff || unierror(UTF_ERR_INVALID_UCS2)
    buf, pnt = _allocate(UInt16, 1)
    set_codeunit!(pnt, 1, ch)
    T(buf)
end

function convert(::Type{T}, str::AbstractString) where {T<:UCS2Strings}
    # Might want to have an invalids_as argument
    len, flags, num4byte = unsafe_checkstring(str, 1, endof(str))
    num4byte == 0 || unierror(UTF_ERR_INVALID_UCS2)
    buf, pnt = _allocate(UInt16, len)
    @inbounds for (i, ch) in enumerate(str)
        set_codeunit!(pnt, i, ch%UInt16)
    end
    T(buf)
end

function convert(::Type{T}, str::String) where {T<:UCS2Strings}
    # Might want to have an invalids_as argument
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_str(T)
    # Check that is correct UTF-8 encoding and get number of words needed
    pnt = _pnt(str)
    len, flags, num4byte = unsafe_checkstring(pnt, 1, siz)
    num4byte == 0 || unierror(UTF_ERR_INVALID_UCS2)
    # Optimize case where no characters > 0x7f
    T(flags == 0 ? _cvtsize(UInt16, pnt, len) : _encode(UInt16, pnt, len))
end

function convert(::Type{T}, str::UnicodeByteStrings) where {T<:WideStr}
    # Might want to have an invalids_as argument
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_str(T)
    T(_cvtsize(UInt16, _data(str), siz))
end

function convert(::Type{T}, str::UTF16Str) where {T<:UCS2Strings}
    # Might want to have an invalids_as argument
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_str(T)
    # Check if conversion is valid
    _all_bmp(str) || unierror(UTF_ERR_INVALID_UCS2)
    T(_cvtsize(UInt16, _pnt(str), len))
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
    len, flags, num4byte = unsafe_checkstring(str, 1, endof(str))
    buf, pnt = _allocate(UInt16, len + num4byte)
    out = 0
    @inbounds for ch in str
        c = UInt32(ch)
        if c > 0x0ffff
            # output surrogate pair
            set_codeunit!(pnt, out += 1, (0xd7c0 + (c >>> 10))%UInt16)
            c = 0xdc00 + (c & 0x3ff)
        end
        set_codeunit!(pnt, out += 1, c%UInt16)
    end
    UTF16Str(buf)
end

function convert(::Type{UTF16Str}, str::String)
    len, dat = _lendata(str)
    # handle zero length string quickly
    len == 0 && return empty_utf16
    # Check that is correct UTF-8 encoding and get number of words needed
    len, flags, num4byte = unsafe_checkstring(dat, 1, len)
    # Optimize case where no characters > 0x7f
    UTF16Str(flags == 0 ? _cvtsize(UInt16, dat, len) : _encode(UInt16, dat, len + num4byte))
end

function convert(::Type{UTF16Str}, str::UTF8Str)
    len, pnt = _lenpnt(str)
    # handle zero length string quickly
    len == 0 && return empty_utf16
    len, flags, num4byte = count_chars(UTF8Str, pnt, len)
    # Optimize case where no characters > 0x7f
    UTF16Str(flags == 0 ? _cvtsize(UInt16, pnt, len) : _encode(UInt16, pnt, len + num4byte))
end

"""
Converts an already validated UTF-8 encoded vector of `UInt8` to a `UTF16Str`

Input Arguments:

*   `dat` `Vector{UInt8}` of UTF-8 encoded data
*   `len` length of output in 16-bit words

Returns:

*   `UTF16Str`
"""
function _encode(::Type{UInt16}, dat::Union{Ptr{UInt8}, Vector{UInt8}}, len)
    out = pos = 0
    buf, pnt = _allocate(UInt16, len)
    @inbounds while out < len
        ch = get_codeunit(dat, pos += 1)%UInt16
        # Handle ASCII characters
        if ch <= 0x7f
        # Handle range 0x80-0x7ff
        elseif ch < 0xe0
            ch = ((ch & 0x1f) << 6) | (get_codeunit(dat, pos += 1) & 0x3f)
        # Handle range 0x800-0xffff
        elseif ch < 0xf0
            ch = get_utf8_3byte(dat, pos += 2, ch)
        # Handle range 0x10000-0x10ffff
        else
            ch32 = get_utf8_4byte(dat, pos += 3, ch)
            # output surrogate pair
            set_codeunit!(pnt, out += 1, (0xd7c0 + (ch32 >>> 10))%UInt16)
            ch = (0xdc00 + (ch32 & 0x3ff))%UInt16
        end
        set_codeunit!(pnt, out += 1, ch)
    end
    buf
end

function convert(::Type{T}, str::S) where {T<:Union{String, UTF8Str},
                                           S<:Union{UCS2Strings, UTF16Str, UTF32Strings}}
    len, pnt = _lenpnt(str)
    # handle zero length string quickly
    len == 0 && return empty_str(T)
    # get number of bytes to allocate (use faster count for validated UTF-8 strings)
    len, flags, num4byte, num3byte, num2byte, latin1 = count_chars(S, pnt, len)
    T(flags == 0
      ? _cvtsize(UInt8, pnt, len)
      : _encode(UInt8, pnt, len + latin1 + num2byte + num3byte*2 + num4byte*3))
end

"""
Converts an already validated UTF-32 encoded vector of `UInt32` to a `UTF16Str`

Input Arguments:

*   `dat` `Vector{UInt32}` of UTF-32 encoded data
*   `len` length of output in 16-bit words

Returns:

*   `Vector{UInt8}`
"""
function _encode(::Type{UInt16}, dat::Ptr{UInt32}, len)
    buf, pnt = _allocate(UInt16, len)
    out = pos = 0
    @inbounds while out < len
        ch = get_codeunit(dat, pos += 1)
        if ch > 0x0ffff
            # Output surrogate pair for 0x10000-0x10ffff
            set_codeunit!(pnt, out += 1, (0xd7c0 + (ch >>> 10))%UInt16)
            ch = 0xdc00 + (ch & 0x3ff)
        end
        set_codeunit!(pnt, out += 1, ch%UInt16)
    end
    buf
end

function convert(::Type{T}, str::ASCIIStr) where {T<:WordStr}
    len, dat = _lendata(str)
    T(_cvtsize(UInt16, dat, len))
end

# Copies because not safe to expose the internal array (would allow mutation)
function convert(::Type{T}, str::WordStr) where {T<:Union{Vector{UInt16},Array{UInt16}}}
    len = _len(str)
    res = similar(T, len)
    unsafe_copy!(pointer(res), _pnt(str), len<<1)
    res
end

convert(::Type{T},  str::S) where {T<:UCS2Strings, S<:UCS2Strings} = str
convert(::Type{UTF16Str}, str::UTF16Str) = str
convert(::Type{UTF16Str}, str::UCS2Strings) = UTF16Str(str.data)

unsafe_convert(::Type{Ptr{UInt16}}, s::UTF16Str) = _pnt(s)

function convert(::Type{UTF16Str}, dat::AbstractVector{UInt16})
    len, flags, num4byte = unsafe_checkstring(dat, 1, endof(dat))
    # Optimize case where no surrogate characters
    UTF16Str(flags == 0 ? _cvtsize(UInt16, dat, len) : _encode(UInt16, dat, len + num4byte))
end

_convert(pnt::Ptr{T}, len, T1) where {T<:Union{UInt16,UInt16_U,UInt16_S,UInt16_US}} =
    ((ch = unsafe_load(pnt)) == 0xfffe
     ? _convert(reinterpret(Ptr{T1}, pnt + 2), len - 1)
     : (ch == 0xfeff ? _convert(pnt + 2, len - 1) : _convert(pnt, len)))

function _convert(pnt::Ptr{T}, len) where {T}
    buf, out = allocate(basetype{T}, len)
    @inbounds for i in 1:len
        set_codeunit!(out, i, unsafe_load(pnt))
        pnt += sizeof(T)
    end
    buf, out
end

function convert(::Type{UTF16Str}, bytes::AbstractArray{UInt8})
    isempty(bytes) && return _empty_utf16
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
    UTF16Str(buf)
end

utf16(x) = convert(UTF16Str, x)
utf16(p::Ptr{Int16}) = utf16(reinterpret(Ptr{UInt16}, p))
utf16(p::Ptr{Int16}, len::Integer) = utf16(reinterpret(Ptr{UInt16}, p), len)

function utf16(pnt::Ptr{UInt16})
    len = 0
    while unsafe_load(pnt, len + 1) != 0
        len += 1
    end
    utf16(pnt, len)
end

"""Handle case where result vector is longer"""
function _maprest(fun, len, dat, buf, pnt, out, pos, uc)
    rst = Vector{UInt16}()
    if uc <= 0x0ffff
        push!(rst, uc%UInt16)
    else
        push!(rst, (0xd7c0 + (uc >> 10))%UInt16)
        push!(rst, (0xdc00 + (uc & 0x3ff))%UInt16)
    end
    while pos <= len
        ch = get_codeunit(dat, pos)%UInt32
        # check for surrogate pair
        is_surrogate_lead(ch) &&
            (ch = get_supplementary(ch, get_codeunit(pnt, pos += 1)))
        uc = check_valid(UInt32(fun(ch)), pos)
        if uc < 0x10000
            push!(rst, uc%UInt16)
        else
            push!(rst, (0xd7c0 + (uc >> 10))%UInt16)
            push!(rst, (0xdc00 + (uc & 0x3ff))%UInt16)
        end
    end
    # We now have a vector to add to the end of buf
    lenrst = length(rst)
    totbuf, totpnt = _allocate(UInt16, len + lenrst)
    unsafe_copyto!(totpnt, 1, pnt, 1, out)
    unsafe_copyto!(totpnt, out + 1, pointer(rst), 1, lenrst)
    UTF16Str(totbuf)
end

function map(fun, str::T) where {T<:Union{UCS2Str,_UCS2Str,UTF16Str}}
    len, dat = _lenpnt(str)
    buf, pnt = _allocate(UInt16, len)
    out = pos = 0
    surrflag = false
    while pos <= len
        ch = get_codeunit(dat, pos)%UInt32
        # check for surrogate pair
        T == UTF16Str && is_surrogate_lead(ch) &&
            (ch = get_supplementary(ch, get_codeunit(pnt, pos += 1)))
        # Note: no checking for invalid here, UTF16Str is always valid
        uc = check_valid(UInt32(fun(ch)), pos)
        if uc < 0x10000
            out < len || return _maprest(fun, len, dat, buf, pnt, out, pos, uc)
        else
            out + 2 <= len || return _maprest(fun, len, dat, buf, pnt, out, pos, uc)
            set_codeunit!(pnt, out += 1, (0xd7c0 + (uc >> 10))%UInt16)
            uc = 0xdc00 + (uc & 0x3ff)
            surrflag = true
        end
        set_codeunit!(pnt, out += 1, uc%UInt16)
    end
    out < len && resize!(buf, out<<1)
    if !surrflag
        T(buf)
    elseif T == _UCS2Str
        # Convert to 32-bit, to keep result in UniStr type union
    else
        UTF16Str(buf)
    end
end

"""
    utf16(s)

Create a UTF-16 string from a byte array, array of `UInt16`, or any other string type. (Data
must be valid UTF-16. Conversions of byte arrays check for a byte-order marker in the first
two bytes, and do not include it in the resulting string.)
"""
utf16(s)

"""
    utf16(::Union{Ptr{UInt16}, Ptr{Int16}} [, length])

Create a string from the address of a NUL-terminated UTF-16 string. A copy is made; the
pointer can be safely freed. If `length` is specified, the string does not have to be
NUL-terminated.
"""
utf16(::Union{Ptr{UInt16}, Ptr{Int16}}, length=length)
