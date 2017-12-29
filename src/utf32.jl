#=
UTF32Str type (UTF-32 encoding)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for UTF32String that used to be in Julia
=#

# UTF-32 basic functions

const _ascii_mask_32 = 0xffffff80_ffffff80
const _latin_mask_32 = 0xffffff00_ffffff00
const _bmp_mask_32   = 0xffff0000_ffff0000

function isascii(str::UTF32Str)
    (siz = sizeof(str)) == 0 && return true
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) < fin
        (unsafe_load(pnt) & _ascii_mask_32) == 0 || return false
    end
    pnt == fin || unsafe_load(reinterpret(Ptr{UInt32}, pnt)) <= 0x7f
end

function islatin(str::UTF32Str)
    (siz = sizeof(str)) == 0 && return true
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) < fin
        (unsafe_load(pnt) & _latin_mask_32) == 0 || return false
    end
    pnt == fin || unsafe_load(reinterpret(Ptr{UInt32}, pnt)) <= 0xff
end

function isbmp(str::UTF32Str)
    (siz = sizeof(str)) == 0 && return true
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) < fin
        (unsafe_load(pnt) & _bmp_mask_32) == 0 || return false
    end
    pnt == fin || unsafe_load(reinterpret(Ptr{UInt32}, pnt)) <= 0xffff
end

isunicode(str::UTF32Str)  = true

isascii(str::_UTF32Str)   = false
islatin(str::_UTF32Str)   = false
isbmp(str::_UTF32Str)     = false
isunicode(str::_UTF32Str) = true


# Speed this up by accessing 64 bits or more at a time
function _cnt_non_bmp(len, pnt::Ptr{UInt32})
    cnt = 0
    @inbounds for i = 1:len
        cnt += get_codeunit(pnt, i) > 0x0ffff
    end
    cnt
end

function search(str::UTF32Strings, ch::UInt32, i::Integer)
    len, pnt = _lenpnt(str)
    i == len + 1 && return 0
    1 <= i <= len && boundserr(s, i)
    (ch <= 0x10ffff && !is_surrogate_codeunit(ch)) || return 0
    @inbounds while i <= len
        get_codeunit(pnt, i) == ch && return i
        i += 1
    end
    0
end

function rsearch(s::UTF32Strings, ch::UInt32, i::Integer)
    len, pnt = _lenpnt(str)
    i == len + 1 && return 0
    1 <= i <= len && boundserr(s, i)
    (ch <= 0x10ffff && !is_surrogate_codeunit(ch)) || return 0
    @inbounds while i > 0
        get_codeunit(pnt, i) == ch && return i
        i -= 1
    end
    0
end

function reverse(str::T) where {T<:UTF32Strings}
    len, pnt = _lenpnt(str)
    len == 0 && return str
    buf, out = _allocate(UInt32, len)
    @inbounds for i = 1:len
        set_codeunit!(out, i, get_codeunit(pnt, len - i + 1))
    end
    T(buf)
end

function convert(::Type{UTF32Str}, ch::UInt32)
    check_valid(cu, 0)
    buf, pnt = _allocate(UInt32, 1)
    set_codeunit!(pnt, 1, cu)
    UTF32Str(buf)
end

# Type _UTF32Str must have at least 1 character > 0xffff, so use other types as well
function convert(::Type{_UTF32Str}, ch::UInt32)
    check_valid(cu, 0)
    if cu <= 0xff
        buf1, pnt1 = _allocate(UInt8, 1)
        set_codeunit!(pnt1, 1, cu)
        cu <= 0x7f ? ASCIIStr(buf1) : _LatinStr(buf1)
    elseif cu <= 0xffff
        buf2, pnt2 = _allocate(UInt16, 1)
        set_codeunit!(pnt2, 1, cu)
        _UCS2Str(buf2)
    else
        buf4, pnt4 = _allocate(UInt32, 1)
        set_codeunit!(pnt4, 1, cu)
        _UTF32Str(buf4)
    end
end

# Is this even necessary anymore?  should have a convert(::Type{T}, s::T) where {T<:Str} = s
#convert(::Type{UTF32Str}, s::UTF32Str) = s

function convert(::Type{UTF32Str}, str::AbstractString)
    len, flags = unsafe_checkstring(str, 1, endof(str))
    buf, pnt = _allocate(UInt32, len)
    out = 0
    @inbounds for ch in str ; set_codeunit!(pnt, out += 1, UInt32(ch)) ; end
    UTF32Str(buf)
end
convert(::Type{_UTF32Str}, str::AbstractString) = Str(str)

# This needs to handle the fact that the String type can contain invalid data!
function convert(::Type{T}, str::String) where {T<:UTF32Strings}
    len, dat = _lendata(str)
    # handle zero length string quickly
    len == 0 && return empty_str(T)
    # Validate UTF-8 encoding, and get number of words to create
    len, flags = unsafe_checkstring(dat, 1, len)
    # Optimize case where no characters > 0x7f, no invalid
    T(flags == 0 ? _cvtsize(UInt32, dat, len) : _encode(UInt32, dat, len))
end

@inline function get_cp(dat, pos)
    ch = get_codeunit(dat, pos += 1)%UInt32
    # Handle ASCII characters
    if ch <= 0x7f
        # Handle range 0x80-0x7ff
    elseif ch < 0xe0
        ch = ((ch & 0x1f) << 6) | (get_codeunit(dat, pos += 1) & 0x3f)
        # Handle range 0x800-0xffff
    elseif ch < 0xf0
        ch = get_utf8_3byte(dat, pos += 2, ch)
        # Handle surrogate pairs (should have been encoded in 4 bytes)
        if is_surrogate_lead(ch)
            # Build up 32-bit character from ch and trailing surrogate in next 3 bytes
            pos += 3
            surr = (((get_codeunit(dat, pos - 2) & 0xf) << 12)%UInt32
                    | ((get_codeunit(dat, pos - 1) & 0x3f) << 6)%UInt32
                    | (get_codeunit(dat, pos) & 0x3f))
            ch = get_supplementary(ch, surr)
        end
        # Handle range 0x10000-0x10ffff
    else
        ch = get_utf8_4byte(dat, pos += 3, ch)
    end
    ch, pos
end

function _encode(::Type{UInt32}, dat, len)
    buf, pnt = _allocate(UInt32, len)
    # has multi-byte UTF-8 sequences
    out = pos = 0
    @inbounds while out < len
        ch, pos = get_cp(dat, pos)
        set_codeunit!(pnt, out += 1, ch)
    end
    buf
end

# transcode to vector of UInt32 from validated UTF8
function _transcode(::Type{T}, ::Type{UTF8Str}, pnt, dat, len) where {T<:CodePoint}
    out = pos = 0
    @inbounds while out < len
        ch = get_codeunit(dat, pos += 1)%UInt32
        # Handle ASCII characters
        if ch <= 0x7f
            # Do nothing
        # Handle range 0x80-0x7ff
        elseif ch < 0xe0
            ch = ((ch & 0x1f) << 6) | (get_codeunit(dat, pos += 1) & 0x3f)
        # Handle range 0x800-0xffff
        elseif ch < 0xf0
            ch = get_utf8_3byte(dat, pos += 2, ch)
        # Handle range 0x10000-0x10ffff
        else
            ch = get_utf8_4byte(dat, pos += 3, ch)
        end
        set_codeunit!(pnt, out += 1, T(ch))
    end
end

# transcode to vector of UInt32 from validated UTF16
function _transcode(::Type{T}, ::Type{UTF16Str}, pnt, dat, len) where {T<:CodePoint}
    out = pos = 0
    @inbounds while out < len
        ch = get_codeunit(dat, pos += 1)%UInt32
        # check for surrogate pair
        is_surrogate_lead(ch) &&
            (ch = get_supplementary(ch, get_codeunit(dat, pos += 1)))
        set_codeunit!(pnt, out += 1, T(ch))
    end
end

# This can rely on the fact that a UTF8Str is always valid
function convert(::Type{UTF32Str}, str::UTF8Str)
    len, dat = _lendata(str)
    # handle zero length string quickly
    len == 0 && return empty_utf32
    cnt = _length(CodeUnitMulti(), str)
    # Optimize case where no characters > 0x7f
    cnt == len && return UTF32Str(_cvtsize(UInt32, dat, len))
    len = cnt
    buf, pnt = _allocate(UInt32, len)
    # has multi-byte UTF-8 sequences
    _transcode(UInt32, UTF8Str, pnt, dat, len)
    UTF32Str(buf)
end

# This can rely on the fact that an ASCIIStr, LatinStr, UCS2Str is always valid
function convert(::Type{UTF32Str}, str::Union{ASCIIStr, LatinStr, _LatinStr, UCS2Str, _UCS2Str})
    len, pnt = _lenpnt(str)
    len == 0 ? empty_utf32 : UTF32Str(_cvtsize(UInt32, pnt, len))
end

# This can rely on the fact that a UTF16Str is always valid
function convert(::Type{UTF32Str}, str::UTF16Str)
    len, dat = _lenpnt(str)
    # handle zero length string quickly
    len == 0 && return empty_utf32
    # Get number of characters to create
    cnt = _length(CodeUnitMulti(), str)
    # No surrogate pairs, do optimized copy
    len == cnt && return UTF32Str(_cvtsize(UInt32, dat, cnt))
    buf, pnt = _allocate(UInt32, cnt)
    _transcode(UInt32, UTF16Str, pnt, dat, cnt)
    UTF32Str(buf)
end

function convert(::Type{UTF16Str}, str::T) where {T<:UTF32Strings}
    # handle zero length string quickly
    (len = _len(str)) == 0 && return empty_utf16
    # get number of words to allocate
    # This can be faster just be checking how many > 0xffff
    pnt = _pnt(str)
    nonbmp = _cnt_non_bmp(len, pnt)
    # optimized path, no surrogates
    UTF16Str(nonbmp == 0 ? _cvtsize(UInt16, pnt, len) : _encode(UInt16, pnt, len + nonbmp))
end

function convert(::Type{S}, str::T) where {S<:UCS2Strings,T<:UTF32Strings}
    # Might want to have an invalids_as argument
    len, pnt = _lenpnt(str)
    # handle zero length string quickly
    len == 0 && return empty_str(S)
    # Check if conversion is valid
    _all_bmp(str) || unierror(UTF_ERR_INVALID_UCS2)
    S(_cvtsize(UInt16, _pnt(str), len))
end

const UniRawChar = Union{UInt32, Int32, Char}

function convert(::Type{T}, dat::AbstractVector{<:UniRawChar}) where {T<:UTF32Strings}
    len = length(dat)
    buf, pnt = _allocate(UInt32, len)
    @inbounds while out < len
        ch = get_codeunit(dat, pos += 1)%UInt32
        set_codeunit!(pnt, out += 1, check_valid(ch, pos))
    end
    T(buf)
end

convert(::Type{T}, v::AbstractVector{<:UniRawChar}) where {T<:AbstractString} =
    convert(T, utf32(v))

function convert(::Type{Vector{UInt32}}, str::UTF32Strings)
    len, pnt = _lenpnt(str)
    vec = Vector{UInt32}(uninitialized, len)
    @inbounds unsafe_copyto!(pointer(vec), pnt, len)
    vec
end

# Is this supposed to allow creating strings sliced up in different ways?
#convert(::Type{Array{UInt32}},  str::UTF32Str) = _data(str)

unsafe_convert(::Type{Ptr{T}}, s::UTF32Str) where {T<:UniRawChar} = convert(Ptr{T}, _pnt(s))

# Should check for 0xxxxxfeff and 0xfffexxxx as well, might be 16-bit encoded
_convert(pnt::Ptr{T}, len, T1) where {T<:Union{UInt32,UInt32_U,UInt32_S,UInt32_US}} =
    ((ch = unsafe_load(pnt)) == 0xfffe0000
     ? _convert(reinterpret(Ptr{T1}, pnt + 4), len - 1)
     : (ch == 0x0feff ? _convert(pnt + 4, len - 1) : _convert(pnt, len)))

function convert(::Type{T}, bytes::AbstractArray{UInt8}) where {T<:UTF32Strings}
    isempty(bytes) && return _empty_utf32
    # Note: there are much better ways of detecting what the likely encoding is here,
    # this only deals with big or little-ending UTF-32
    # It really should detect at a minimum UTF-8, UTF-16 big and little
    len = length(bytes)
    len & 3 == 0 || unierror(UTF_ERR_ODD_BYTES_32, len, 0)
    len >>>= 2
    pnt = pointer(bytes)
    if reinterpret(UInt, pnt) & 3 == 0
        buf, out = _convert(reinterpret(Ptr{UInt32}, pnt), len, swappedtype(UInt32))
    else
        buf, out = _convert(reinterpret(Ptr{UInt32_U}, pnt), len, swappedtype(UInt32_U))
    end
    # Todo, this needs better handling
    isvalid(T, out, len) || unierror(UTF_ERR_INVALID, 0, 0)
    T(buf)
end

function isvalid(::Type{<:UTF32Strings}, str::Vector{<:UniRawChar})
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
    unsafe_copyto!(out, 1, pnt, 1, len)
    UTF32Str(buf)
end

function map(fun, str::T) where {T<:UTF32Strings}
    len, dat = _lendata(str)
    buf, pnt = _allocate(UInt32, len)
    @inbounds for i = 1:len
        set_codeunit!(pnt, i, check_valid(UInt32(fun(dat[i]))))
    end
    T(buf)
end

# Definitions for C compatible strings, that don't allow embedded
# '\0', and which are terminated by a '\0'
containsnul(s::ByteStr) = containsnul(unsafe_convert(Ptr{Cchar}, s), sizeof(s))

function containsnul(s::WideStr)
    # SPJ!!! Fix this!
    findfirst(_data(s), 0) != (sizeof(s)>>1)
end

#=
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
=#

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
