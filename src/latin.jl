#=
LatinStr/_LatinStr type (ISO Latin1 8-bit subset of Unicode)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for ASCIIString that used to be in Julia
=#

## overload methods for efficiency ##

is_ascii(str::MaybeSub{<:Str{<:_LatinCSE}}) = false
is_latin(str::MaybeSub{<:Str{<:LatinCSE}}) = true
is_bmp(str::MS_Latin) = true
is_unicode(str::MS_Latin) = true

const MS_ASCIILatin = MaybeSub{<:Str{<:Union{ASCIICSE, Latin_CSEs}}}

function string(collection::MS_ASCIILatin...)
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

function convert(::Type{<:Str{C}}, str::AbstractString) where {C<:Latin_CSEs}
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

const ASCIISubStr  = SubString{<:Str{ASCIICSE}}
const LatinSubStr  = SubString{<:Str{LatinCSE}}
const _LatinSubStr = SubString{<:Str{_LatinCSE}}

_cpyconvert(::Type{C}, str) where {C} = Str(C, _copysub(str))::Str{C,Nothing,Nothing,Nothing}
_cpysubset(::Type{_LatinCSE}, str) = Str(is_ascii(s) ? ASCIICSE : _LatinCSE, _copysub(s))

convert(::Type{<:Str{LatinCSE}}, s::Str{LatinCSE})  = s
convert(::Type{<:Str{LatinCSE}}, s::Str{ASCIICSE})  = _cpyconvert(LatinCSE, s)
convert(::Type{<:Str{LatinCSE}}, s::Str{_LatinCSE}) = _cpyconvert(LatinCSE, s)

convert(::Type{<:Str{LatinCSE}}, s::LatinSubStr)  = _cpyconvert(LatinCSE, s)
convert(::Type{<:Str{LatinCSE}}, s::ASCIISubStr)  = _cpyconvert(LatinCSE, s)
convert(::Type{<:Str{LatinCSE}}, s::_LatinSubStr) = _cpyconvert(LatinCSE, s)

convert(::Type{<:Str{_LatinCSE}}, s::Str{LatinCSE})  = s
convert(::Type{<:Str{_LatinCSE}}, s::Str{ASCIICSE})  = s
convert(::Type{<:Str{_LatinCSE}}, s::Str{_LatinCSE}) = s

convert(::Type{<:Str{_LatinCSE}}, s::LatinSubStr)  = _cpysubset(_LatinCSE, s)
convert(::Type{<:Str{_LatinCSE}}, s::ASCIISubStr)  = _cpyconvert(LatinCSE, s)
convert(::Type{<:Str{_LatinCSE}}, s::_LatinSubStr) = Str(_LatinCSE, _copysub(s))

# Assumes that has already been checked for validity

# These should be sped up to do chunks at a time, when no bytes > 0x7f
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

# Fast conversion from LatinStr or _LatinStr to RawUTF8Str/UTF8Str
function convert(::Type{<:Str{C}}, str::MS_Latin) where {C<:UTF8_CSEs}
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_str(C)
    @preserve str begin
        pnt = pointer(str)
        Str(C,(cnt = count_latin(len, pnt)) == 0 ? _copysub(str) : _latin_to_utf8(pnt, len + cnt))
    end
end

# Fast conversion from LatinStr or _LatinStr to String
function convert(::Type{String}, str::MS_Latin)
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_string
    @preserve str begin
        pnt = pointer(str)
        (cnt = count_latin(len, pnt)) == 0 ? _copysub(str) : _latin_to_utf8(pnt, len + cnt)
    end
end

function convert(::Type{<:Str{C}}, str::MaybeSub{<:Str{<:Union{Word_CSEs,Quad_CSEs}}}
                 ) where {C<:Latin_CSEs}
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
    (siz = sizeof(str)) == 0 && return empty_str(LatinCSE)
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
    (len = length(vec)) == 0 && return _empty_str(C)
    @preserve vec begin
        pnt = pointer(vec)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, len)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        Str((C === _LatinCSE && latinbyte == 0) ? ASCIICSE : C,
            ((CU !== UInt8 || flags == 0) ? _cvtsize(UInt8, pnt, len) : _utf8_to_latin(pnt, len)))
    end
end

function convert(::Type{<:Str{C}}, str::MaybeSub{<:Str{<:Union{Text2CSE,Text4CSE}}}
                 ) where {C<:Latin_CSEs}
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_str(C)
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, len)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        Str((C === _LatinCSE && latinbyte == 0) ? ASCIICSE : C, _cvtsize(UInt8, pnt, len))
    end
end
