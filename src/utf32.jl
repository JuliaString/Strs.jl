#=
UTF32Str type (UTF-32 encoding)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for UTF32String that used to be in Julia
=#

# UTF-32 basic functions
next(s::UTF32Str, i::Int) = (Char(_data(s)[i]), i+1)

const _ascii_mask_32 = 0xffffff80_ffffff80

function isascii(str::UTF32Str)
    len, pnt = _lenpnt(str)
    qpnt = reinterpret(Ptr{UInt64}, pnt)
    while len >= 8
        unsafe_load(qpnt) & _ascii_mask_32 == 0 || return false
        qpnt += 8
        len -= 8
    end
    len == 0 || unsafe_load(reinterpret(Ptr{UInt32}, pnt)) < 0x80
end

function check_valid(ch, pos)
    is_surrogate_codeunit(ch) && throw(UnicodeError(UTF_ERR_SURROGATE, pos, ch))
    ch <= 0x10ffff || throw(UnicodeError(UTF_ERR_INVALID, pos, ch))
    ch
end

function reverse(str::UTF32Str)
    len, pnt = _lenpnt(str)
    len == 0 && return str
    buf, out = _allocate(UInt32, len)
    @inbounds for i = 1:len
        set_codeunit!(out, get_codeunit(pnt, len - i + 1), i)
    end
    UTF32Str(buf)
end

function convert(::Type{UTF32Str}, ch::Char)
    cu = UInt32(ch) # This might be doing weird conversions in the future!
    check_valid(cu, 0)
    buf, pnt = _allocate(UInt32, 1)
    set_codeunit!(pnt, cu, 1)
    UTF32Str(buf)
end

convert(::Type{UTF32Str}, s::UTF32Str) = s

function convert(::Type{UTF32Str}, str::AbstractString)
    len, flags = unsafe_checkstring(str, 1, endof(str))
    buf, pnt = _allocate(UInt32, len)
    out = 0
    @inbounds for ch in str ; set_codeunit!(pnt, UInt32(ch), out += 1) ; end
    UTF32Str(buf)
end

# This needs to handle the fact that the String type can contain invalid data!
function convert(::Type{UTF32Str}, str::String)
    len, dat = _lendata(str)
    # handle zero length string quickly
    len == 0 && return empty_utf32
    # Validate UTF-8 encoding, and get number of words to create
    len, flags = unsafe_checkstring(dat, 1, len)
    # Optimize case where no characters > 0x7f, no invalid
    flags == 0 && return UTF32Str(_cvtsize(UInt32, dat, len))
    buf, pnt = _allocate(UInt32, len)
    # has multi-byte UTF-8 sequences
    local ch::UInt32, surr::UInt32
    out = pos = 0
    @inbounds while out < len
        ch = dat[pos += 1]
        # Handle ASCII characters
        if ch <= 0x7f
            set_codeunit!(pnt, ch, out += 1)
        # Handle range 0x80-0x7ff
        elseif ch < 0xe0
            set_codeunit!(pnt, ((ch & 0x1f) << 6) | (dat[pos += 1] & 0x3f), out += 1)
        # Handle range 0x800-0xffff
        elseif ch < 0xf0
            ch = get_utf8_3byte(dat, pos += 2, ch)
            # Handle surrogate pairs (should have been encoded in 4 bytes)
            if is_surrogate_lead(ch)
                # Build up 32-bit character from ch and trailing surrogate in next 3 bytes
                pos += 3
                surr = (((dat[pos-2] & 0xf) << 12)%UInt32
                        | ((dat[pos-1] & 0x3f) << 6)%UInt32
                        | (dat[pos] & 0x3f))
                ch = get_supplementary(ch, surr)
            end
            set_codeunit!(pnt, ch, out += 1)
        # Handle range 0x10000-0x10ffff
        else
            set_codeunit!(pnt, get_utf8_4byte(dat, pos += 3, ch), out += 1)
        end
    end
    UTF32Str(buf)
end

# This can rely on the fact that a UTF8Str is always valid
function convert(::Type{UTF32Str}, str::UTF8Str)
    len, dat = _lendata(str)
    # handle zero length string quickly
    len == 0 && return empty_utf32
    cnt = 0
    # Get number of characters to create
    for i = 1:len
        @inbounds cnt += !is_valid_continuation(dat[i])
    end
    # Optimize case where no characters > 0x7f
    cnt == len && return UTF32Str(_cvtsize(UInt32, dat, len))
    len = cnt
    buf, pnt = _allocate(UInt32, len)
    # has multi-byte UTF-8 sequences
    local ch::UInt32
    out = pos = 0
    @inbounds while out < len
        ch = dat[pos += 1]
        # Handle ASCII characters
        if ch <= 0x7f
            set_codeunit!(pnt, ch, out += 1)
        # Handle range 0x80-0x7ff
        elseif ch < 0xe0
            set_codeunit!(pnt, ((ch & 0x1f) << 6) | (dat[pos += 1] & 0x3f), out += 1)
        # Handle range 0x800-0xffff
        elseif ch < 0xf0
            set_codeunit!(pnt, get_utf8_3byte(dat, pos += 2, ch), out += 1)
        # Handle range 0x10000-0x10ffff
        else
            set_codeunit!(pnt, get_utf8_4byte(dat, pos += 3, ch), out += 1)
        end
    end
    UTF32Str(buf)
end

# This can rely on the fact that an ASCIIStr, LatinStr, or UCS2Str is always valid
function convert(::Type{UTF32Str}, str::Union{ASCIIStr, LatinStr, UCS2Str})
    len, pnt = _lenpnt(str)
    len == 0 ? empty_utf32 : UTF32Str(_cvtsize(UInt32, pnt, len))
end

# This can rely on the fact that a UTF16Str is always valid
function convert(::Type{UTF32Str}, str::UTF16Str)
    len, dat = _lenpnt(str)
    # handle zero length string quickly
    len == 0 && return empty_utf32
    # Get number of characters to create
    cnt = 0
    for i = 1:len
        @inbounds cnt += !is_surrogate_trail(get_codeunit(dat, i))
    end
    # No surrogate pairs, do optimized copy
    len == cnt && return UTF32Str(_cvtsize(UInt32, dat, len))
    len = cnt
    buf, pnt = _allocate(UInt32, len)
    out = pos = 0
    @inbounds while out < len
        ch = get_codeunit(dat, pos += 1)%UInt32
        # check for surrogate pair
        is_surrogate_lead(ch) &&
            (ch = get_supplementary(ch, get_codeunit(dat, pos += 1)))
        set_codeunit!(pnt, ch, out += 1)
    end
    UTF32Str(buf)
end

function convert(::Type{Uni16Str}, str::UTF32Str)
    len, pnt = _lenpnt(str)
    # handle zero length string quickly
    len == 0 && return empty_ucs2
    # get number of words to allocate
    len, flags, num4byte = unsafe_checkstring(pnt, 1, len)
    len += num4byte
    # optimized path, no surrogates
    num4byte == 0 ? UCS2Str(_cvtsize(UInt16, pnt, len)) : UTF16Str(_encode(UInt16, pnt, len))
end

function convert(::Type{UCS2Str}, str::UTF32Str)
    len, pnt = _lenpnt(str)
    # handle zero length string quickly
    len == 0 && return empty_ucs2
    # get number of words to allocate
    len, flags, num4byte = unsafe_checkstring(pnt, 1, len)
    num4byte == 0 || throw(UnicodeError(UTF_ERR_INVALID_UCS2))
    UCS2Str(_cvtsize(UInt16, pnt, len))
end

const UniRawChar = Union{UInt32, Int32, Char}

function convert(::Type{UTF32Str}, dat::AbstractVector{<:UniRawChar})
    len = length(dat)
    buf, pnt = _allocate(UInt32, len)
    @inbounds while out < len
        ch = get_codeunit(dat, pos += 1)%UInt32
        set_codeunit!(pnt, check_valid(ch, pos), pos)
    end
    UTF32Str(buf)
end

convert(::Type{T}, v::AbstractVector{<:UniRawChar}) where {T<:AbstractString} =
    convert(T, utf32(v))

#=
# Note: removed them because these are unsafe, they allow the supposed immutable string to be
# aliased and modified
convert(::Type{Vector{UInt32}}, str::UTF32Str) = _data(str)
convert(::Type{Array{UInt32}},  str::UTF32Str) = _data(str)
=#

unsafe_convert(::Type{Ptr{T}}, s::UTF32Str) where {T<:UniRawChar} = convert(Ptr{T}, _pnt(s))

# This needs to ensure that the created string is valid!
function convert(::Type{UTF32Str}, bytes::AbstractArray{UInt8})
    isempty(bytes) && return empty_utf32
    # Note: there are much better ways of detecting what the likely encoding is here,
    # this only deals with big or little-ending UTF-32
    # It really should detect at a minimum UTF-8, UTF-16 big and little
    len = length(bytes)
    len & 3 != 0 && throw(UnicodeError(UTF_ERR_ODD_BYTES_32, 0, 0))
    len >>>= 2
    pnt = pointer(bytes)
    ch = get_unaligned32(pnt)
    if ch == 0x0feff
        len -= 1
        pnt += 4
        swap = false
    elseif ch == 0xfffe0000
        len -= 1
        pnt += 4
        swap = true
    else
        swap = false
    end
    buf, out = _allocate(UInt32, len)
    if swap
        @inbounds for i in 1:len
            set_codeunit!(out, check_valid(get_swapped32(pnt), i), i)
            pnt += 4
        end
    else
        @inbounds for i in 1:len
            set_codeunit!(out, check_valid(get_unaligned32(pnt), i), i)
            pnt += 4
        end
    end
    UTF32Str(buf)
end

function isvalid(::Type{UTF32Str}, str::Vector{<:UniRawChar})
    @inbounds for c in str
        ch = UInt32(c)
        (!is_surrogate_codeunit(ch) && ch <= 0x10ffff) || return false
    end
    true
end

isvalid(str::Vector{Char}) = isvalid(UTF32Str, str)
utf32(x) = convert(UTF32Str, x)

utf32(p::Union{Ptr{Char}, Ptr{Int32}}, len::Integer) = utf32(reinterpret(Ptr{UInt32}, p), len)
utf32(p::Union{Ptr{Char}, Ptr{Int32}}) = utf32(reinterpret(Ptr{UInt32}, p))

function utf32(p::Ptr{UInt32})
    len = 0
    while (ch = unsafe_load(pnt, len += 1)) != 0
        check_valid(ch, len)
    end
    buf, out = _allocate(UInt32, len)
    unsafe_copy!(out, pnt, len)
    UTF32Str(buf)
end

function map(fun, str::UTF32Str)
    len, dat = _lendata(str)
    buf, pnt = _allocate(UInt32, len)
    @inbounds for i = 1:len
        ch = fun(Char(dat[i]))
        isa(ch, Char) || throw(UnicodeError(UTF_ERR_MAP_CHAR, 0, 0))
        set_codeunit!(pnt, check_valid(UInt32(ch), i), i)
    end
    UTF32Str(buf)
end

# Definitions for C compatible strings, that don't allow embedded
# '\0', and which are terminated by a '\0'
containsnul(s::ByteStr) = containsnul(unsafe_convert(Ptr{Cchar}, s), sizeof(s))
function containsnul(s::WideStr)
    findfirst(_data(s), 0) != length(_data(s))
end

if sizeof(Cwchar_t) == 2
    const WString = UTF16Str
    const wstring = utf16
elseif sizeof(Cwchar_t) == 4
    const WString = UTF32Str
    const wstring = utf32
end
wstring(s::Cwstring) = wstring(convert(Ptr{Cwchar_t}, s))

# Cwstring is defined in c.jl, but conversion needs to be defined here
# to have WString
function unsafe_convert(::Type{Cwstring}, s::WString)
    containsnul(s) &&
        throw(ArgumentError("embedded NUL chars are not allowed in C strings: $(repr(s))"))
    Cwstring(unsafe_convert(Ptr{Cwchar_t}, s))
end

# pointer conversions of ASCII/UTF8/UTF16/UTF32 strings:
pointer(s::Union{ByteStr, WideStr}) = _pnt(s)
pointer(s::ByteStr, i::Integer) = _pnt(s) + i - 1
pointer(s::WideStr, i::Integer) = _pnt(s) + (i - 1)*codeunit_size(s)

# pointer conversions of SubString of ASCII/UTF8/UTF16/UTF32:
pointer(x::SubString{<:ByteStr}) =
    _pnt(x.string) + x.offset
pointer(x::SubString{<:ByteStr}, i::Integer) =
    _pnt(x.string) + x.offset + (i-1)
pointer(x::SubString{<:WideStr}) =
    _pnt(x.string) + x.offset*codeunit_size(x.string)
pointer(x::SubString{<:WideStr}, i::Integer) =
    _pnt(x.string) + (x.offset + (i-1))*codeunit_size(x.string)

"""
    utf32(s)

Create a UTF-32 string from a byte array, array of `Char` or `UInt32`, or any other string
type. (Conversions of byte arrays check for a byte-order marker in the first four bytes, and
do not include it in the resulting string.)
"""
utf32(s)

"""
    utf32(::Union{Ptr{Char}, Ptr{UInt32}, Ptr{Int32}} [, length])

Create a string from the address of a NUL-terminated UTF-32 string. A copy is made; the
pointer can be safely freed. If `length` is specified, the string does not have to be
NUL-terminated.
"""
utf32(::Union{Ptr{Char}, Ptr{UInt32}, Ptr{Int32}}, length=length)
