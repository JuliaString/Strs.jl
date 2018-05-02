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

#convert(::Type{T}, s::T) where {T<:Str{<:Latin_CSEs}} = s
#convert(::Type{SubString{T}}, s::T) where {T<:Str{<:Latin_CSEs}} = SubString(s, 1)
#convert(::Type{T}, s::T) where {T<:SubString{<:Str{<:Latin_CSEs}}} = s
function convert(::Type{T}, s::MS_ASCIILatin) where {T<:Str{<:Latin_CSEs}}
    C = basecse(T)
    Str(C, _copysub(s))::Str{C,Nothing,Nothing,Nothing}
end
function convert(::Type{T}, s::MS_ASCIILatin) where {S<:Str{<:Latin_CSEs}, T<:SubString{S}}
    C = basecse(T)
    SubString{C}(Str(C, _copysub(s)), 1)
end

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

# Fast conversion from LatinStr or _LatinStr to String/RawUTF8Str/UTF8Str
function _convert(::Type{T}, str::MS_Latin
                 ) where {S<:Union{String,Str{RawUTF8CSE},Str{UTF8CSE}},T<:MaybeSub{S}}
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return _wrap_substr(T, empty_str(S))
    @preserve str begin
        pnt = pointer(str)
        buf = (cnt = count_latin(len, pnt)) == 0 ? _copysub(str) : _latin_to_utf8(pnt, len + cnt)
        _wrap_substr(T, S === String ? buf : Str(basecse(T), buf))
    end
end
convert(::Type{T}, str::MS_Latin) where {T<:Union{String,Str{RawUTF8CSE},Str{UTF8CSE}}} =
    _convert(T, str)
convert(::Type{T}, str::MS_Latin
        ) where {S<:Union{String,Str{RawUTF8CSE},Str{UTF8CSE}},T<:SubString{S}} = _convert(T, str)

function _convert(::Type{T}, str::AbstractString) where {C<:Latin_CSEs, T<:MaybeSub{<:Str{C}}}
    is_empty(str) && return C === _LatinCSE ? empty_ascii : _empty_latin
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
convert(::Type{T}, str::AbstractString) where {T<:Str{<:Latin_CSEs}} = _convert(T, str)
convert(::Type{<:SubString{T}}, str::AbstractString) where {T<:Str{<:Latin_CSEs}} =
    SubString(_convert(T, str), 1)

convert(::Type{T}, str::MaybeSub{String}) where {T<:Str{<:Latin_CSEs}} = _convert(T, str)
convert(::Type{<:SubString{T}}, str::MaybeSub{String}) where {T<:Str{<:Latin_CSEs}} =
    SubString{T}(_convert(T, str), 1)

function _convert(::Type{T}, str::MaybeSub{<:Str{<:Union{Word_CSEs,Quad_CSEs}}}
                 ) where {C<:Latin_CSEs, T<:MaybeSub{<:Str{C}}}
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return _empty_sub(T, C)
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, len)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        _wrap_substr(T, Str((C === _LatinCSE && latinbyte == 0) ? ASCIICSE : C,
                            _cvtsize(UInt8, pnt, len)))
    end
end
convert(::Type{T}, str::MaybeSub{<:Str{<:Union{Word_CSEs,Quad_CSEs}}}
        ) where {T<:Str{<:Latin_CSEs}} = _convert(T, str)
convert(::Type{T}, str::MaybeSub{<:Str{<:Union{Word_CSEs,Quad_CSEs}}}
        ) where {T<:SubString{<:Str{Latin_CSEs}}} = _convert(T, str)

function _convert(::Type{T}, str::MS_ByteStr) where {T<:MaybeSub{<:Str{LatinCSE}}}
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return _wrap_substr(T, empty_latin)
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, siz)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        _wrap_substr(T, Str(LatinCSE, flags == 0 ? _copysub(str) : _utf8_to_latin(pnt, len)))
    end
end
convert(::Type{T}, str::MS_ByteStr) where {T<:Str{LatinCSE}} = _convert(T, str)
convert(::Type{T}, str::MS_ByteStr) where {T<:SubString{<:Str{LatinCSE}}} = _convert(T, str)

_convert(::Type{_LatinCSE}, ch::UInt8) = _convert(ch <= 0x7f ? ASCIICSE : _LatinCSE, ch)
convert(::Type{<:Str{_LatinCSE}}, ch::Unsigned) =
    ch < 0xff ? _convert(_LatinCSE, ch%UInt8) : unierror(UTF_ERR_LATIN1, ch)

function _convert(::Type{T}, str::MS_ByteStr) where {T<:MaybeSub{<:Str{_LatinCSE}}}
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return _wrap_substr(T, empty_ascii)
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, siz)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        _wrap_substr(T, Str(latinbyte == 0 ? ASCIICSE : _LatinCSE,
                            flags == 0 ? _copysub(str) : _utf8_to_latin(pnt, len)))
    end
end
convert(::Type{T}, str::MS_ByteStr) where {T<:Str{_LatinCSE}} = _convert(T, str)
convert(::Type{T}, str::MS_ByteStr) where {T<:SubString{<:Str{_LatinCSE}}} = _convert(T, str)

function _convert(::Type{T}, vec::Vector{CU}
                 ) where {C<:Latin_CSEs,T<:MaybeSub{<:Str{C}},CU<:CodeUnitTypes}
    # handle zero length string quickly
    (len = length(vec)) == 0 && return _empty_sub(T, C)
    @preserve vec begin
        pnt = pointer(vec)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, len)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        _wrap_substr(T, Str((C === _LatinCSE && latinbyte == 0) ? ASCIICSE : C,
                            ((CU !== UInt8 || flags == 0)
                             ? _cvtsize(UInt8, pnt, len) : _utf8_to_latin(pnt, len))))
    end
end

convert(::Type{T}, vec::Vector{<:CodeUnitTypes}) where {T<:Str{<:Latin_CSEs}} =
    _convert(T, vec)
convert(::Type{T}, vec::Vector{<:CodeUnitTypes}) where {T<:SubString{<:Str{<:Latin_CSEs}}} =
    _convert(T, vec)

const MS_T24 = MaybeSub{<:Str{<:Union{Text2CSE,Text4CSE}}}

function _convert(::Type{T}, str::MS_T24) where {C<:Latin_CSEs,T<:MaybeSub{<:Str{C}}}
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return _empty_sub(T, C)
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, len)
        num4byte + num3byte + num2byte == 0 || unierror(UTF_ERR_INVALID_LATIN1)
        _wrap_substr(T, Str((C === _LatinCSE && latinbyte == 0) ? ASCIICSE : C,
                            _cvtsize(UInt8, pnt, len)))
    end
end

convert(::Type{T}, str::MS_T24) where {T<:Str{<:Latin_CSEs}} = _convert(T, str)
convert(::Type{T}, str::MS_T24) where {T<:SubString{<:Str{<:Latin_CSEs}}} = _convert(T, str)
