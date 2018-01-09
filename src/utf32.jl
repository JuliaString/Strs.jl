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
    while (pnt += CHUNKSZ) <= fin
        (unsafe_load(pnt) & _ascii_mask_32) == 0 || return false
    end
    pnt - CHUNKSZ == fin || unsafe_load(reinterpret(Ptr{UInt32}, pnt)) <= 0x7f
end

function islatin(str::UTF32Str)
    (siz = sizeof(str)) == 0 && return true
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) <= fin
        (unsafe_load(pnt) & _latin_mask_32) == 0 || return false
    end
    pnt - CHUNKSZ == fin || unsafe_load(reinterpret(Ptr{UInt32}, pnt)) <= 0xff
end

function isbmp(str::UTF32Str)
    (siz = sizeof(str)) == 0 && return true
    pnt, fin = _calcpnt(str, siz)
    while (pnt += CHUNKSZ) <= fin
        (unsafe_load(pnt) & _bmp_mask_32) == 0 || return false
    end
    pnt- CHUNKSZ  == fin || unsafe_load(reinterpret(Ptr{UInt32}, pnt)) <= 0xffff
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
    Str(cse(T), buf)
end

function convert(::Type{UTF32Str}, ch::UInt32)
    check_valid(cu, 0)
    buf, pnt = _allocate(UInt32, 1)
    set_codeunit!(pnt, 1, cu)
    Str(UTF32CSE, buf)
end

# Type _UTF32Str must have at least 1 character > 0xffff, so use other types as well
function convert(::Type{_UTF32Str}, ch::UInt32)
    check_valid(cu, 0)
    if cu <= 0xff
        buf1, pnt1 = _allocate(UInt8, 1)
        set_codeunit!(pnt1, 1, cu)
        Str(cu <= 0x7f ? ASCIICSE : _LatinCSE, buf1)
    elseif cu <= 0xffff
        buf2, pnt2 = _allocate(UInt16, 1)
        set_codeunit!(pnt2, 1, cu)
        Str(_UCS2CSE, buf2)
    else
        buf4, pnt4 = _allocate(UInt32, 1)
        set_codeunit!(pnt4, 1, cu)
        Str(_UTF32CSE, buf4)
    end
end

function convert(::Type{UTF32Str}, str::AbstractString)
    isempty(str) && return empty_utf32
    len, flags = unsafe_checkstring(str)
    buf, pnt = _allocate(UInt32, len)
    out = 0
    @inbounds for ch in str ; set_codeunit!(pnt, out += 1, UInt32(ch)) ; end
    Str(UTF32CSE, buf)
end
convert(::Type{_UTF32Str}, str::AbstractString) = Str(str)

# This needs to handle the fact that the String type can contain invalid data!
function _convert_string_utf32(::Type{T}, str::String) where {T<:UTF32Strings}
    # handle zero length string quickly
    (len = _len(str)) == 0 && return empty_str(T)
    # Validate UTF-8 encoding, and get number of words to create
    len, flags = unsafe_checkstring(str, 1, len)
    # Optimize case where no characters > 0x7f, no invalid
    Str(cse(T), flags == 0 ? _cvtsize(UInt32, str, len) : _encode_utf32(_pnt(str), len))
end

# Avert problems with ambiguous method
convert(::Type{UTF32Str}, str::String)  = _convert_string_utf32(UTF32Str, str)
convert(::Type{_UTF32Str}, str::String) = _convert_string_utf32(_UTF32Str, str)

@inline function get_cp(pnt)
    ch = get_codeunit(pnt)%UInt32
    # Handle ASCII characters
    if ch <= 0x7f
        ch, pnt + 1
    elseif ch < 0xe0 # Handle range 0x80-0x7ff
        ((ch & 0x1f) << 6) | (get_codeunit(pnt + 1) & 0x3f), pnt + 2
    elseif ch < 0xf0 # Handle range 0x800-0xffff
        ch = get_utf8_3byte(pnt += 2, ch)%UInt32
        # Handle surrogate pairs (should have been encoded in 4 bytes)
        (is_surrogate_lead(ch)
            # Build up 32-bit character from ch and trailing surrogate in next 3 bytes
         ? (get_supplementary(ch, get_utf8_3byte(pnt + 3, get_codeunit(pnt + 1)%UInt32)), pnt + 4)
         : (ch, pnt + 1))
    else # Handle range 0x10000-0x10ffff
        get_utf8_4byte(pnt + 3, ch), pnt + 4
    end
end

function _encode_utf32(pnt, len)
    buf, out = _allocate(UInt32, len)
    # has multi-byte UTF-8 sequences
    fin = out + (len<<2)
    @inbounds while out < fin
        ch, pnt = get_cp(pnt)
        set_codeunit!(out, ch)
        out += 4
    end
    buf
end

# transcode to vector of UInt32 from validated UTF8
function _transcode_utf32(::Type{UTF8Str}, pnt, len)
    buf, out = _allocate(UInt32, len)
    fin = out + (len<<2)
    @inbounds while out < fin
        ch = get_codeunit(pnt)%UInt32
        # Handle ASCII characters
        if ch <= 0x7f
            set_codeunit!(out, ch)
        # Handle range 0x80-0x7ff
        elseif ch < 0xe0
            set_codeunit!(out, ((ch & 0x1f) << 6) | (get_codeunit(pnt += 1) & 0x3f))
        elseif ch < 0xf0
            # Handle range 0x800-0xffff
            set_codeunit!(out, get_utf8_3byte(pnt += 2, ch)%UInt32)
        else
            # Handle range 0x10000-0x10ffff
            set_codeunit!(out, get_utf8_4byte(pnt += 3, ch))
        end
        pnt += 1
        out += 4
    end
    buf
end

# transcode to vector of UInt32 from validated UTF16
function _transcode_utf32(::Type{UTF16Str}, pnt, len)
    buf, out = _allocate(UInt32, len)
    fin = out + (len<<2)
    @inbounds while out < fin
        ch = get_codeunit(pnt)
        is_surrogate_lead(ch) && (ch = get_supplementary(ch, get_codeunit(pnt += 2)))
        set_codeunit!(out, ch)
        pnt += 2
        out += 4
    end
    buf
end

# This can rely on the fact that a UTF8Str is always valid
function convert(::Type{UTF32Str}, str::UTF8Str)
    # handle zero length string quickly
    (len = _len(str)) == 0 && return empty_utf32
    cnt = _length(CodeUnitMulti(), str)
    # Optimize case where no characters > 0x7f, otherwise has multi-byte UTF-8 sequences
    Str(UTF32CSE,
        (cnt == len
         ? _cvtsize(UInt32, _data(str), cnt)
         : _transcode_utf32(UTF8Str, _pnt(str), cnt)))
end

_cvt_utf32(T, str) =
    (siz = sizeof(str)) == 0 ? empty_str(T) : Str(cse(T), _cvtsize(UInt32, _data(str), siz))

const ShortStr = Union{UnicodeByteStrings,UCS2Strings}
# This can rely on the fact that an ASCIIStr, LatinStr, UCS2Str is always valid
convert(::Type{Text4Str}, str::ShortStr) = _cvt_utf32(Text4Str, str)
convert(::Type{UTF32Str}, str::ShortStr) = _cvt_utf32(UTF32Str, str)
convert(::Type{_UTF32Str}, str::ShortStr) = _cvt_utf32(_UTF32Str, str)

# This can rely on the fact that a UTF16Str is always valid
function convert(::Type{UTF32Str}, str::UTF16Str)
    # handle zero length string quickly
    (len = _len(str)) == 0 && return empty_utf32
    # Get number of characters to create
    cnt = _length(CodeUnitMulti(), str)
    # No surrogate pairs, do optimized copy
    Str(UTF32CSE, cnt == len
        ? _cvtsize(UInt32, _pnt(str), cnt)
        : _transcode_utf32(UTF16Str, _pnt(str), cnt))
end

function convert(::Type{UTF16Str}, str::T) where {T<:UTF32Strings}
    # handle zero length string quickly
    (len = _len(str)) == 0 && return empty_utf16
    # get number of words to allocate
    # This can be faster just be checking how many > 0xffff
    pnt = _pnt(str)
    nonbmp = _cnt_non_bmp(len, pnt)
    # optimized path, no surrogates
    Str(UTF16CSE, nonbmp == 0 ? _cvtsize(UInt16, pnt, len) : _encode_utf16(pnt, len + nonbmp))
end

function convert(::Type{S}, str::T) where {S<:UCS2Strings,T<:UTF32Strings}
    # Might want to have an invalids_as argument
    # handle zero length string quickly
    (len = _len(str)) == 0 && return empty_str(S)
    # Check if conversion is valid
    isbmp(str) || unierror(UTF_ERR_INVALID_UCS2)
    Str(cse(S), _cvtsize(UInt16, _pnt(str), len))
end

const UniRawChar = Union{UInt32, Int32, Text4Chr, Char}

function convert(::Type{T}, dat::AbstractVector{<:UniRawChar}) where {T<:UTF32Strings}
    (len = length(dat)) == 0 && empty_str(T)
    buf, pnt = _allocate(UInt32, len)
    fin = pnt + (len<<2)
    pos = 0
    @inbounds while pnt < fin
        ch = dat[pos += 1]%UInt32
        set_codeunit!(pnt, check_valid(ch, pos))
        pnt += 4
    end
    Str(cse(T), buf)
end

# Not sure this is valid anymore, want to avoid type piracy
convert(::Type{T}, v::AbstractVector{<:UniRawChar}) where {T<:AbstractString} =
    convert(T, convert(UTF32Str, v))

function convert(::Type{Vector{UInt32}}, str::QuadStr)
    len = _len(str)
    vec = Vector{UInt32}(uninitialized, len)
    @inbounds unsafe_copyto!(pointer(vec), _pnt(str), len)
    vec
end

# I don't think this will work for Char anymore, broken by #24999
unsafe_convert(::Type{Ptr{T}}, s::UTF32Str) where {T<:UniRawChar} = convert(Ptr{T}, _pnt(s))

# Should check for 0xxxxxfeff and 0xfffexxxx as well, might be 16-bit encoded
_convert(pnt::Ptr{T}, len, T1) where {T<:Union{UInt32,UInt32_U,UInt32_S,UInt32_US}} =
    ((ch = unsafe_load(pnt)) == 0xfffe0000
     ? _convert(reinterpret(Ptr{T1}, pnt + 4), len - 1)
     : (ch == 0x0feff ? _convert(pnt + 4, len - 1) : _convert(pnt, len)))

function convert(::Type{T}, bytes::AbstractArray{UInt8}) where {T<:UTF32Strings}
    isempty(bytes) && return empty_utf32
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
    Str(cse(T), buf)
end

function isvalid(::Type{<:UTF32Strings}, str::Vector{<:UniRawChar})
    @inbounds for c in str
        ch = c%UInt32
        (!is_surrogate_codeunit(ch) && ch <= 0x10ffff) || return false
    end
    true
end

isvalid(str::Vector{Char}) = isvalid(UTF32Str, str)

function map(fun, str::T) where {T<:UTF32Strings}
    len = _len(str)
    buf, pnt = _allocate(UInt32, len)
    @inbounds for i = 1:len
        set_codeunit!(pnt, i, check_valid(UInt32(fun(dat[i]))))
    end
    Str(cse(T), buf)
end
