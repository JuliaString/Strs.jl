#=
LatinStr/_LatinStr type (ISO Latin1 8-bit subset of Unicode)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for ASCIIString that used to be in Julia
=#

## overload methods for efficiency ##

is_ascii(str::MaybeSub{<:Str{C}}) where {C<:_LatinCSE}    = false
is_latin(str::MaybeSub{<:Str{C}}) where {C<:Latin_CSEs}   = true
is_bmp(str::MaybeSub{<:Str{C}}) where {C<:Latin_CSEs}     = true
is_unicode(str::MaybeSub{<:Str{C}}) where {C<:Latin_CSEs} = true

const _UBS = Union{ASCIICSE, Latin_CSEs}

function string(collection::MaybeSub{<:Str{_UBS}}...)
    length(collection) == 1 && return collection[1]
    len = 0
    @inbounds for str in collection
        len += ncodeunits(str)
    end
    buf, pnt = _allocate(len)
    @inbounds for str in collection
        len = ncodeunits(str)
        _memcpy(pnt, pointer(str), len)
        pnt += len
    end
    Str(LatinCSE, buf)
end

## transcoding to Latin1 ##

#convert(::Type{T}, s::T) where {T<:Str{<:Latin_CSEs}} = s
#convert(::Type{SubString{T}}, s::T) where {T<:Str{<:Latin_CSEs}} = SubString(s, 1)
#convert(::Type{T}, s::T) where {T<:SubString{<:Str{<:Latin_CSEs}}} = s
function convert(::Type{T}, s::MS_ASCIILatin) where {T<:Str{<:Latin_CSEs}}
    C = basecse(T)
    Str(C, _copysub(s))::Str{C,Nothing,Nothing,Nothing}
end
convert(::Type{<:SubString{T}}, s::MS_ASCIILatin) where {T<:Str{<:Latin_CSEs}} =
    SubString(Str(basecse(T), _copysub(s)), 1)

# Assumes that has already been checked for validity
function _utf8_to_latin(pnt::Ptr{UInt8}, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    while out < fin
        ch = get_codeunit(pnt)
        # Handle ASCII characters
        ch > 0x7f && (ch = ((ch & 3) << 6) | (get_codeunit(pnt += 1) & 0x3f))
        pnt += 1
        set_codeunit!(out, ch)
        out += 1
    end
    buf
end

function _latin_to_utf8(pnt::Ptr{UInt8}, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    while out < fin
        ch = get_codeunit(pnt)
        pnt += 1
        if ch > 0x7f
            set_codeunit!(out, 0xc0 | (ch >>> 6))
            ch = 0x80 | (ch & 0x3f)
            out += 1
        end
        set_codeunit!(out, ch)
        out += 1
    end
    buf
end

# Fast conversion from LatinStr or _LatinStr to UTF8Str
function convert(::Type{<:Str{UTF8CSE}}, str::Str{<:Latin_CSEs})
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_utf8
    @preserve str begin
        pnt = pointer(str)
        Str(UTF8CSE, (cnt = count_latin(len, pnt)) != 0 ? _latin_to_utf8(pnt, len + cnt) : str)
    end
end

# Fast conversion from LatinStr or _LatinStr to String
function convert(::Type{String}, str::Str{<:Latin_CSEs})
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_string
    @preserve str begin
        pnt = pointer(str)
        (cnt = count_latin(len, pnt)) != 0 ? _latin_to_utf8(pnt, len + cnt) : str.data
    end
end

function _convert(::Type{T}, str::AbstractString) where {C<:Latin_CSEs, T<:MaybeSub{<:Str{C}}}
    is_empty(str) && return C === _LatinCSE ? empty_ascii : empty_latin
    # Might want to have invalids_as here
    len, flags, num4byte, num3byte, num2byte = unsafe_check_string(str)
    num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
    buf, pnt = _allocate(UInt8, len)
    @inbounds for ch in str
        set_codeunit!(pnt, ch%UInt8)
        pnt += 1
    end
    Str((C === _LatinCSE && flags == 0) ? ASCIICSE : C, buf)
end

function convert(::Type{<:Str{C}},
                 str::MaybeSub{<:Str{<:Union{Word_CSEs,Quad_CSEs}}}) where {C<:Latin_CSEs}
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return C === _LatinCSE ? empty_ascii : empty_latin
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, len)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        Str((C === _LatinCSE && latinbyte == 0) ? ASCIICSE : C, _cvtsize(UInt8, pnt, len))
    end
end

function convert(::Type{<:Str{LatinCSE}}, str::MS_ByteStr)
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_latin
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, siz)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        Str(LatinCSE, flags == 0 ? _copysub(str) : _utf8_to_latin(pnt, len))
    end
end

_convert(::Type{_LatinCSE}, ch::UInt8) = _convert(ch <= 0x7f ? ASCIICSE : _LatinCSE, ch)
convert(::Type{<:Str{_LatinCSE}}, ch::Unsigned) =
    ch < 0xff ? _convert(_LatinCSE, ch%UInt8) : unierror(UTF_ERR_LATIN1, ch)

function convert(::Type{<:Str{_LatinCSE}}, str::MS_ByteStr)
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_ascii
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, siz)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        Str(latinbyte == 0 ? ASCIICSE : _LatinCSE,
            flags == 0 ? _copysub(str) : _utf8_to_latin(pnt, len))
    end
end

function convert(::Type{<:Str{C}}, vec::Vector{CU}) where {C<:Latin_CSEs,CU<:CodeUnitTypes}
    # handle zero length string quickly
    (len = length(vec)) == 0 && return C === _LatinCSE ? empty_ascii : empty_latin
    @preserve vec begin
        pnt = pointer(vec)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, len)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        Str((C === _LatinCSE && latinbyte == 0) ? ASCIICSE : C,
            (CU !== UInt8 || flags == 0) ? _cvtsize(UInt8, pnt, len) : _utf8_to_latin(pnt, len))
    end
end

function convert(::Type{<:Str{C}},
                 str::MaybeSub{<:Str{<:Union{Text2CSE,Text4CSE}}}) where {C<:Latin_CSEs}
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return C === _LatinCSE ? empty_ascii : empty_latin
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, len)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        Str((C === _LatinCSE && latinbyte == 0) ? ASCIICSE : C, _cvtsize(UInt8, pnt, len))
    end
end
