#=
UTF32Str type (UTF-32 encoding)

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones,
and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for UTF32String that used to be in Julia
=#

# UTF-32 basic functions

is_unicode(str::MS_UTF32)  = true

is_ascii(str::MS_SubUTF32)   = false
is_latin(str::MS_SubUTF32)   = false
is_bmp(str::MS_SubUTF32)     = false
is_unicode(str::MS_SubUTF32) = true

# Speed this up by accessing 64 bits or more at a time
function _cnt_non_bmp(len, pnt::Ptr{UInt32})
    cnt = 0
    @inbounds for i = 1:len
        cnt += get_codeunit(pnt, i) > 0x0ffff
    end
    cnt
end

# Type _UTF32Str must have at least 1 character > 0xffff, so use other types as well
convert(::Type{<:Str{_UTF32CSE}}, ch::Unsigned) =
    (ch > 0xff
     ? (is_unicode(ch)
        ? (ch <= 0xffff ? _convert(_UCS2CSE, ch%UInt16) : _convert(_UTF32CSE, ch%UInt32))
        : unierror(UTF_ERR_INVALID, 0, ch))
     : _convert(_LatinCSE, ch%UInt8))

function convert(::Type{<:Str{UTF32CSE}}, str::AbstractString)
    is_empty(str) && return empty_utf32
    len, flags = unsafe_check_string(str)
    buf, pnt = _allocate(UInt32, len)
    @inbounds for ch in str
        set_codeunit!(pnt, UInt32(ch))
        pnt += sizeof(UInt32)
    end
    Str(UTF32CSE, buf)
end
convert(::Type{_UTF32Str}, str::AbstractString) = Str(str)

# This needs to handle the fact that the String type can contain invalid data
function convert(::Type{<:Str{UTF32CSE}}, str::MS_RawUTF8)
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_str(UTF32CSE)
    @preserve str begin
        pnt = pointer(str)
        # Validate UTF-8 encoding, and get number of words to create
        len, flags = fast_check_string(pnt, siz)
        # Optimize case where no characters > 0x7f, no invalid
        Str(UTF32CSE, flags == 0 ? _cvtsize(UInt32, pnt, len) : _encode_utf32(pnt, len))
    end
end

function convert(::Type{<:Str{UTF32CSE}}, str::MaybeSub{<:Str{Text2CSE}})
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_utf32
    @preserve str begin
        pnt = pointer(str)
        # Validate and get number of characters to create
        cnt, flags = fast_check_string(pnt, len)
        Str(UTF32CSE,
            cnt == len ? _cvtsize(UInt32, pnt, cnt) : _transcode_utf16_to_utf32(pnt, cnt))
    end
end

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
    fin = bytoff(out, len)
    @inbounds while out < fin
        ch, pnt = get_cp(pnt)
        set_codeunit!(out, ch)
        out += sizeof(UInt32)
    end
    buf
end

# transcode to vector of UInt32 from validated UTF8
function _transcode_utf8_to_utf32(pnt, len)
    buf, out = _allocate(UInt32, len)
    fin = bytoff(out, len)
    while out < fin
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
        out += sizeof(UInt32)
    end
    buf
end

# transcode to vector of UInt32 from validated UTF16
function _transcode_utf16_to_utf32(pnt, len)
    buf, out = _allocate(UInt32, len)
    fin = bytoff(out, len)
    @inbounds while out < fin
        ch = get_codeunit(pnt)%UInt32
        pnt += sizeof(UInt16)
        is_surrogate_lead(ch) &&
            (ch = get_supplementary(ch, get_codeunit(pnt)) ; pnt += sizeof(UInt16))
        set_codeunit!(out, ch)
        out += sizeof(UInt32)
    end
    buf
end

# This can rely on the fact that a UTF8Str is always valid
function convert(::Type{<:Str{UTF32CSE}}, str::MS_UTF8)
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_str(UTF32CSE)
    cnt = _length(MCU, str)
    # Optimize case where no characters > 0x7f, otherwise has multi-byte UTF-8 sequences
    Str(UTF32CSE, @preserve str (cnt == len
                                 ? _cvtsize(UInt32, pointer(str), cnt)
                                 : _transcode_utf8_to_utf32(pointer(str), cnt)))
end

# This can rely on the fact that an ASCIIStr, LatinStr, UCS2Str is always valid
convert(::Type{<:Str{UTF32CSE}},
        str::MaybeSub{Str{C}}) where {C<:Union{Binary_CSEs,ASCIICSE,Latin_CSEs,UCS2_CSEs}} =
    ((len = ncodeunits(str)) == 0
     ? empty_str(UTF32CSE)
     : @preserve str Str(UTF32CSE, _cvtsize(UInt32, pointer(str), len)))

# This can rely on the fact that a UTF16Str is always valid
function convert(::Type{<:Str{UTF32CSE}}, str::MS_UTF16)
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_utf32
    # Get number of characters to create
    cnt = _length(MCU, str)
    # No surrogate pairs, do optimized copy
    Str(UTF32CSE, @preserve str (cnt == len
                                 ? _cvtsize(UInt32, pointer(str), cnt)
                                 : _transcode_utf16_to_utf32(pointer(str), cnt)))
end

function convert(::Type{<:Str{UTF16CSE}}, str::MaybeSub{Str{<:UTF32_CSEs}})
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_utf16
    # get number of words to allocate
    # This can be faster just be checking how many > 0xffff
    @preserve str begin
        pnt = pointer(str)
        nonbmp = _cnt_non_bmp(len, pnt)
        # optimized path, no surrogates
        Str(UTF16CSE, nonbmp == 0 ? _cvtsize(UInt16, pnt, len) : _encode_utf16(pnt, len + nonbmp))
    end
end

function convert(::Type{<:Str{C}}, str::MaybeSub{Str{<:UTF32_CSEs}}) where {C<:UCS2_CSEs}
    # Might want to have an invalids_as argument
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_str(C === UCS2CSE ? C : ASCIICSE)
    # Check if conversion is valid
    is_bmp(str) || unierror(UTF_ERR_INVALID_UCS2)
    @preserve str Str(UCS2CSE, _cvtsize(UInt16, pointer(str), len))
end

const UniRawChar = Union{UInt32, Int32, Text4Chr, Char}

function convert(::Type{<:Str{C}}, dat::AbstractArray{<:UniRawChar}) where {C<:UTF32_CSEs}
    (len = length(dat)) == 0 && return empty_str(C === UTF32CSE ? C : ASCIICSE)
    buf, pnt = _allocate(UInt32, len)
    C === _UTF32CSE && (msk = 0%UInt32)
    @preserve buf begin
        fin = bytoff(pnt, len)
        pos = 0
        while pnt < fin
            @inbounds ch = dat[pos += 1]%UInt32
            C === _UTF32CSE && (msk |= ch)
            set_codeunit!(pnt, check_valid(ch, pos))
            pnt += sizeof(UInt32)
        end
        (C === UTF32CSE || msk > 0xffff) && return Str(C, buf)
        msk > 0xff && return Str(_UCS2CSE, _cvtsize(UInt16, pnt, len))
        Str(msk > 0x7f ? _LatinCSE : ASCIICSE, _cvtsize(UInt8, pnt, len))
    end
end

function convert(::Type{<:Str{UTF32CSE}}, dat::Vector{<:Union{UInt32,Int32,Text4Chr}})
    is_valid(UTF32Str, dat) || unierror(UTF_ERR_INVALID)
    @preserve dat Str(UTF32CSE, _copysub(pointer(dat), length(dat)))
end

# Not sure this is valid anymore, want to avoid type piracy
convert(::Type{T}, v::AbstractArray{<:UniRawChar}) where {T<:AbstractString} =
    convert(T, convert(UTF32Str, v))

# To do, make this more generic, add a function that can create a vector & fill it from an Str
function convert(::Type{Vector{UInt32}}, str::MaybeSub{<:Str{<:Quad_CSEs}})
    len = ncodeunits(str)
    vec = create_vector(UInt32, len)
    @preserve str _memcpy(pointer(vec), pointer(str), len)
    vec
end

convert(::Type{<:Str{Text4CSE}}, str::MaybeSub{<:Str{C}}
        ) where {C<:Union{ASCIICSE,Text1CSE,BinaryCSE,Latin_CSEs, UCS2_CSEs}} =
            @preserve str Str(Text4CSE, _cvtsize(UInt32, pointer(str), ncodeunits(str)))

convert(::Type{<:Str{Text4CSE}}, str::MaybeSub{<:Str{<:UTF32_CSEs}}) = Str(Text4CSE, _copysub(str))

# Should check for 0xxxxxfeff and 0xfffexxxx as well, might be 16-bit encoded
_convert(pnt::Ptr{T}, len, T1) where {T<:Union{UInt32,UInt32_U,UInt32_S,UInt32_US}} =
    ((ch = unsafe_load(pnt)) == 0xfffe0000
     ? _convert(reinterpret(Ptr{T1}, pnt + 4), len - 1)
     : (ch == 0x0feff ? _convert(pnt + 4, len - 1) : _convert(pnt, len)))

function convert(::Type{T}, bytes::AbstractArray{UInt8}) where {C<:UTF32_CSEs,T<:Str{C}}
    is_empty(bytes) && return empty_utf32
    # Note: there are much better ways of detecting what the likely encoding is here,
    # this only deals with big or little-ending UTF-32
    # It really should detect at a minimum UTF-8, UTF-16 big and little
    len = length(bytes)
    len & 3 == 0 || unierror(UTF_ERR_ODD_BYTES_32, len, 0)
    len >>>= 2
    @preserve bytes begin
        pnt = pointer(bytes)
        if reinterpret(UInt, pnt) & 3 == 0
            buf, out = _convert(reinterpret(Ptr{UInt32}, pnt), len, swappedtype(UInt32))
        else
            buf, out = _convert(reinterpret(Ptr{UInt32_U}, pnt), len, swappedtype(UInt32_U))
        end
        # Todo, this needs better handling
        is_valid(T, out, len) || unierror(UTF_ERR_INVALID, 0, 0)
        Str(C, buf)
    end
end

function is_valid(::Type{<:Str{UTF32CSE}}, str::AbstractArray{<:UniRawChar})
    @inbounds for c in str
        ch = c%UInt32
        (!is_surrogate_codeunit(ch) && ch <= 0x10ffff) || return false
    end
    true
end
function is_valid(::Type{<:Str{_UTF32CSE}}, str::AbstractArray{<:UniRawChar})
    msk = 0%UInt32
    @inbounds for c in str
        ch = c%UInt32
        msk |= ch
        (!is_surrogate_codeunit(ch) && ch <= 0x10ffff) || return false
    end
    msk > 0xffff
end

is_valid(vec::AbstractArray{Char}) = is_unicode(vec)

function map(fun, str::MS_UTF32)
    @preserve str buf begin
        len = ncodeunits(str)
        buf, out = _allocate(UInt32, len)
        pnt = pointer(str)
        fin = bytoff(pnt, len)
        while pnt < fin
            set_codeunit!(out, check_valid(fun(get_codeunit(pnt))))
            pnt += sizeof(UInt32)
            out += sizeof(UInt32)
        end
        Str(UTF32CSE, buf)
    end
end

# Make sure the result actually still has at least one character > 0xffff
function map(fun, str::MS_SubUTF32)
    @preserve str buf begin
        msk = 0%UInt32
        len = ncodeunits(str)
        buf, out = _allocate(UInt32, len)
        pnt = pointer(str)
        fin = bytoff(pnt, len)
        while pnt < fin
            ch = check_valid(fun(get_codeunit(pnt)))
            msk |= ch
            set_codeunit!(out, ch)
            pnt += sizeof(UInt32)
            out += sizeof(UInt32)
        end
        Str(ifelse(msk < 0xffff, UTF32CSE, _UTF32CSE), buf)
    end
end
