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
        buf[out + 1] = 0xf0 | (ch >>> 18)
        buf[out + 2] = 0x80 | ((ch >>> 12) & 0x3f)
        buf[out + 3] = 0x80 | ((ch >>> 6) & 0x3f)
        buf[out + 4] = 0x80 | (ch & 0x3f)
    end
end

const _ascii_mask = 0xff80_ff80_ff80_ff80
const _surr_mask  = 0xd800_d800_d800_d800

@inline _msk(v,n)      = (v<<n) & 0x8000_8000_8000_8000
@inline _mask_surr(v)  = _msk(v | _msk(v,1) | _msk(v,2) | _msk(v,3) | _msk(v,4), 0)
@inline _get_masked(qpnt) = _mask_surr(xor(unsafe_load(qpnt), _surr_mask))

function length(str::UTF16Str)
    (len = sizeof(str)) == 0 && return 0
    cnt = 0
    qpnt = reinterpret(Ptr{UInt64}, pointer(str.data))
    while len >= 8
        cnt += count_ones(_get_masked(qpnt))
        qpnt += 8
        len -= 8
    end
    len == 0 ? cnt : cnt + count_ones(_get_masked(qpnt) & _mask_bytes(len))
end

function isascii(str::WordStr)
    len, pnt = _lenpnt(str)
    qpnt = reinterpret(Ptr{UInt64}, pnt)
    while len >= 8
        unsafe_load(qpnt) & _ascii_mask == 0 || return false
        qpnt += 8
        len -= 8
    end
    len == 0 ? true : (unsafe_load(qpnt) & _mask_bytes(len)) & _ascii_mask == 0
end
function endof(str::UTF16Str)
    len, pnt = _lenpnt(str)
    len == 0 ? 0 : (is_surrogate_codeunit(get_codeunit(pnt, len)) ? len-1 : len)
end

get_supplementary(lead::Unsigned, trail::Unsigned) = (UInt32(lead-0xd7f7)<<10 + trail)

function _next(str::UTF16Str, i::Int)
    len, pnt = _lenpnt(str)
    i <= len || throw(BoundsError(s, i))
    ch = get_codeunit(pnt, i)
    !is_surrogate_codeunit(ch) && return (ch, i + 1)
    # check length (i > len would have gotten bounds error)
    i == len && throw(UnicodeError(UTF_ERR_MISSING_SURROGATE, i, ch))
    !is_surrogate_lead(ch) && throw(UnicodeError(UTF_ERR_NOT_LEAD, i, ch))
    @inbounds ct = get_codeunit(pnt, i + 1)
    !is_surrogate_trail(ct) && throw(UnicodeError(UTF_ERR_NOT_TRAIL, i, ch))
    get_supplementary(ch, ct), i + 2
end
Base.@propagate_inbounds function next(str::UTF16Str, i::Int)
    ch, i = _next(str, i)
    Char(ch), i
end
Base.@propagate_inbounds function next(it::CodeUnits{UTF16Str}, i::Int)
    len, pnt = _lenpnt(it.xs)
    i <= len || throw(BoundsError(s, i))
    get_codeunit(pnt, i), i + 1
end
next(it::CodePoints{UTF16Str}, i::Int) = _next(it.xs, i)

function reverseind(str::UTF16Str, i::Integer)
    len, pnt = _lenpnt(str)
    j = len - i
    is_surrogate_trail(get_codeunit(pnt, j)) ? j - 1 : j
end

function reverse(str::UTF16Str)
    len, pnt = _lenpnt(str)
    len == 0 && return str
    buf, out = _allocate(UInt16, len)
    @inbounds for i = 1:len-1
        ch = get_codeunit(pnt, len - i)
        if is_surrogate_lead(ch)
            set_codeunit!(out, get_codeunit(out, i - 1), i)
            set_codeunit!(out, ch, i - 1)
        else
            set_codeunit!(out, ch, i)
        end
    end
    UTF16Str(buf)
end

function reverse(str::UCS2Str)
    len, pnt = _lenpnt(str)
    len == 0 && return str
    buf, out = _allocate(UInt16, len)
    @inbounds for i = 1:len
        set_codeunit!(out, get_codeunit(pnt, len - i + 1), i)
    end
    UCS2Str(buf)
end

function isvalid(::Type{UCS2Str}, data::AbstractArray{UInt16})
    @inbounds for ch in data
        is_surrogate_codeunit(ch) && return false
    end
    true
end

function isvalid(::Type{UCS2Str}, pnt::Ptr{UInt16}, len)
    pos = 0
    @inbounds while (pos += 1) < len # check for surrogates
        is_surrogate_codeunit(get_codeunit(pnt, pos)) && return false
    end
    true
end

function convert(::Type{UCS2Str}, str::AbstractString)
    # Might want to have an invalids_as argument
    len, flags, num4byte = unsafe_checkstring(str, 1, endof(str))
    num4byte == 0 || throw(UnicodeError(UTF_ERR_INVALID_UCS2))
    buf, pnt = _allocate(UInt16, len)
    @inbounds for (i, ch) in enumerate(str)
        set_codeunit!(pnt, ch%UInt16, i)
    end
    UCS2Str(buf)
end

function convert(::Type{UCS2Str}, str::UTF8Strings)
    # Might want to have an invalids_as argument
    len, dat = _lendata(str)
    # handle zero length string quickly
    len == 0 && return empty_ucs2
    # Check that is correct UTF-8 encoding and get number of words needed
    len, flags, num4byte = unsafe_checkstring(dat, 1, len)
    num4byte == 0 || throw(UnicodeError(UTF_ERR_INVALID_UCS2))
    # Optimize case where no characters > 0x7f
    UCS2Str(flags == 0 ? _cvtsize(UInt16, dat, len) : _encode(UInt16, dat, len))
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
        if c < 0x10000
            set_codeunit!(pnt, c%UInt16, out += 1)
        else
            # output surrogate pair
            set_codeunit!(pnt, (0xd7c0 + (c >>> 10))%UInt16, out += 1)
            set_codeunit!(pnt, (0xdc00 + (c & 0x3ff))%UInt16, out += 1)
        end
    end
    UTF16Str(buf)
end

function convert(::Type{UTF16Str}, str::UTF8Strings)
    len, dat = _lendata(str)
    # handle zero length string quickly
    len == 0 && return empty_utf16
    # Check that is correct UTF-8 encoding and get number of words needed
    len, flags, num4byte = unsafe_checkstring(dat, 1, len)
    # Optimize case where no characters > 0x7f
    UTF16Str(flags == 0 ? _cvtsize(UInt16, dat, len) : _encode(UInt16, dat, len + num4byte))
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
            set_codeunit!(pnt, ch, out += 1)
        # Handle range 0x80-0x7ff
        elseif ch < 0xe0
            set_codeunit!(pnt, ((ch & 0x1f) << 6) | (get_codeunit(dat, pos += 1) & 0x3f), out += 1)
        # Handle range 0x800-0xffff
        elseif ch < 0xf0
            set_codeunit!(pnt, get_utf8_3byte(dat, pos += 2, ch), out += 1)
        # Handle range 0x10000-0x10ffff
        else
            ch32 = get_utf8_4byte(dat, pos += 3, ch)
            # output surrogate pair
            set_codeunit!(pnt, (0xd7c0 + (ch32 >>> 10))%UInt16, out += 1)
            set_codeunit!(pnt, (0xdc00 + (ch32 & 0x3ff))%UInt16, out += 1)
        end
    end
    buf
end

function convert(::Type{T}, str::Union{WordStr, UTF32Str}) where {T <: UTF8Strings}
    len, pnt = _lenpnt(str)
    # handle zero length string quickly
    len == 0 && return empty_str(T)
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte = unsafe_checkstring(pnt, 1, len)
    T(flags == 0
      ? _cvtsize(UInt8, pnt, len)
      : _encode(UInt8, pnt, len + num2byte + num3byte*2 + num4byte*3))
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
            set_codeunit!(pnt, (0xd7c0 + (ch >>> 10))%UInt16, out += 1)
            set_codeunit!(pnt, (0xdc00 + (ch & 0x3ff))%UInt16, out += 1)
        else
            set_codeunit!(pnt, ch%UInt16, out += 1)
        end
    end
    buf
end

function convert(::Type{T}, str::ASCIIStr) where {T<:WordStr}
    len, dat = _lendata(str)
    T(_cvtsize(UInt16, dat, len))
end

# Should this be copying? Not safe to expose the internal array if it does not!
convert(::Type{<:Union{Vector{UInt16},Array{UInt16}}}, str::WordStr) = _data(str)

convert(::Type{UCS2Str},  str::UCS2Str)  = str
convert(::Type{UTF16Str}, str::UTF16Str) = str
convert(::Type{UTF16Str}, str::UCS2Str)  = UTF16Str(str.data)

function convert(::Type{UCS2Str},  str::UTF16Str)
    # Might want to have invalids_as argument
    isvalid(UCS2Str, str) || throw(UnicodeError(UTF_ERR_INVALID_UCS2))
    UCS2Str(str.data)
end

unsafe_convert(::Type{Ptr{UInt16}}, s::UTF16Str) = _pnt(s)

function convert(::Type{UTF16Str}, dat::AbstractVector{UInt16})
    len, flags, num4byte = unsafe_checkstring(dat, 1, endof(dat))
    # Optimize case where no surrogate characters
    UTF16Str(flags == 0 ? _cvtsize(UInt16, dat, len) : _encode(UInt16, dat, len + num4byte))
end

function convert(::Type{UTF16Str}, bytes::AbstractArray{UInt8})
    isempty(bytes) && return _empty_utf16
    # Note: there are much better ways of detecting what the likely encoding is here,
    # this only deals with big or little-ending UTF-32
    # It really should detect at a minimum UTF-8, UTF-16 big and little
    len = length(bytes)
    isodd(len) && throw(UnicodeError(UTF_ERR_ODD_BYTES_16, nb, 0))
    len >>>= 1
    pnt = pointer(bytes)
    ch = get_unaligned16(pnt)
    if ch == 0xfffe
        len -= 1
        pnt += 2
        buf, out = allocate(UInt16, len)
        @inbounds for i in 1:len
            set_codeunit!(out, get_swapped16(pnt), i)
            pnt += 2
        end
    else
        if ch == 0xfeff
            len -= 1
            pnt += 2
        end
        buf, out = allocate(UInt16, len)
        @inbounds for i in 1:len
            set_codeunit!(out, get_unaligned16(pnt), i)
            pnt += 2
        end
    end
    isvalid(UTF16Str, out, len) || throw(UnicodeError(UTF_ERR_INVALID, 0, 0))
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
function _maprest(fun, len, dat, buf, out, pos, uc)
    rst = Vector{UInt16}()
    if uc < 0x10000
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
        c2 = fun(Char(ch))
        isa(c2, Char) ||
            throw(UnicodeError(UTF_ERR_MAP_CHAR, 0, 0))
        uc = UInt32(c2)
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
    unsafe_copy!(totpnt, pnt, out)
    unsafe_copy!(totpnt, out + 1, pointer(rst), 1, lenrst)
    UTF16Str(totbuf)
end

function map(fun, str::T) where {T<:WordStr}
    len, dat = _lenpnt(str)
    buf, pnt = _allocate(UInt16, len)
    out = pos = 0
    surrflag = false
    while pos <= len
        ch = get_codeunit(dat, pos)%UInt32
        # check for surrogate pair
        T == UTF16Str && is_surrogate_lead(ch) &&
            (ch = get_supplementary(ch, get_codeunit(pnt, pos += 1)))
        c2 = fun(Char(ch))
        isa(c2, Char) ||
            throw(UnicodeError(UTF_ERR_MAP_CHAR, 0, 0))
        # Note: no checking for invalid here, UTF16Str is always valid
        uc = UInt32(c2)
        if uc < 0x10000
            out < len || return _maprest(len, dat, buf, pnt, out, pos, uc)
            set_codeunit!(pnt, uc%UInt16, out += 1)
        else
            out + 2 <= len || return _maprest(len, dat, buf, out, pos, uc)
            set_codeunit!(pnt, (0xd7c0 + (uc >> 10))%UInt16, out += 1)
            set_codeunit!(pnt, (0xdc00 + (uc & 0x3ff))%UInt16, out += 1)
            surrflag = true
        end
    end
    out < len && resize!(buf, out<<1)
    surrflag ? UTF16Str(buf) : UCS2Str(buf)
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
