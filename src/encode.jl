#=
Constructors for Str strings

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

function _encode_ascii_latin(str, len)
    buf, out = _allocate(UInt8, len)
    @inbounds for ch in str
        set_codeunit!(out, ch%UInt8)
        out += 1
    end
    buf
end

function _encode_ascii_latin(pnt::Ptr{UInt8}, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    @inbounds while out < fin
        ch8 = get_codeunit(pnt)
        set_codeunit!(out,
                      (ch8 <= 0x7f ? ch8 : (((ch8 & 3) << 6) | (get_codeunit(pnt += 1) & 0x3f))))
        pnt += 1
        out += 1
    end
    buf
end

@inline function safe_copy(::Type{Vector{T}}, ::Type{C}, str) where {T<:CodeUnitTypes,C<:CSE}
    @preserve str buf begin
        len = ncodeunits(str)
        buf, out = _allocate(T, len)
        _memcpy(out, pointer(str), len)
        Str(C, buf)
    end
end

@inline safe_copy(::Type{<:Str}, ::Type{C}, str) where {C<:CSE} = Str(C, str.data)
@inline safe_copy(::Type{String}, ::Type{C}, str) where {C<:CSE} = Str(C, _data(str))

function _str(str::AbstractString)
    # handle zero length string quickly
    isempty(str) && return empty_ascii
    len, flags, num4byte, num3byte, num2byte, latin1byte = unsafe_check_string(str)
    if flags == 0
        Str(ASCIICSE, _encode_ascii_latin(str, len))
    elseif num4byte != 0
        Str(_UTF32CSE, _encode_utf32(str, len))
    elseif num3byte + num2byte != 0
        Str(_UCS2CSE, _encode_utf16(str, len))
    else
        Str(latin1byte == 0 ? ASCIICSE : _LatinCSE, _encode_ascii_latin(str, len))
    end
end

function _str(str::T) where {T<:Union{Vector{UInt8}, Str{<:Binary_CSEs}, String}}
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_ascii
    @preserve str begin
        pnt = pointer(str)
        len, flags, num4byte, num3byte, num2byte, latin1byte = fast_check_string(pnt, siz)
        if flags == 0
            buf, out = _allocate(UInt8, len)
            _memcpy(out, pnt, len)
            Str(ASCIICSE, buf)
        elseif num4byte != 0
            Str(_UTF32CSE, _encode_utf32(pnt, len))
        elseif num3byte + num2byte != 0
            Str(_UCS2CSE, _encode_utf16(pnt, len))
        else
            Str(latin1byte == 0 ? ASCIICSE : _LatinCSE, _encode_ascii_latin(pnt, len))
        end
    end
end

function _str_cpy(::Type{T}, str, len) where {T}
    @preserve str begin
        buf, pnt = _allocate(T, len)
        @inbounds for ch in str
            set_codeunit!(pnt, ch%T)
            pnt += sizeof(T)
        end
        buf
    end
end

utf_length(::Type{<:CSE}, b, e) = Int(e - b) + 1

utf_length(l) = l < 0x80 ? l : ifelse(l < 0x800, l*2-0x80, l*3-0x880)

# Must have either 1,2,3 byte characters, or 3,4 byte characters
utf_length(::Type{UTF8CSE}, b, e) = utf_length(e) - utf_length(b)

utf_length(::Type{UTF16CSE}, b, e) =
    e <= 0xffff ? (e-b+1) : ifelse(b > 0xffff, (e-b+1)*2, (e*2-b-0xfffe))

function convert(::Type{<:Str{C}}, rng::UnitRange{<:CodeUnitTypes}) where {C<:CSE}
    isempty(rng) && return empty_str(C)
    b, e = rng.start, rng.stop
    isvalid(C, b) || unierror(UTF_ERR_INVALID, 1, b)
    isvalid(C, e) || unierror(UTF_ERR_INVALID, length(rng), e)
    # Need to calculate allocation length
    Str(C, _str_cpy(C, rng, Int(e - b) + 1))
end

function convert(::Type{<:Str{C}}, rng::UnitRange{<:CodeUnitTypes}
                 ) where {C<:Union{ASCIICSE,Latin_CSEs,UCS2_CSEs,UTF32_CSEs}}
    isempty(rng) && return empty_str(C)
    b, e = rng.start, rng.stop
    isvalid(C, b) || unierror(UTF_ERR_INVALID, 1, b)
    isvalid(C, e) || unierror(UTF_ERR_INVALID, length(rng), e)
    # If contains range 0xd800-0xdfff, then also invalid
    isempty(intersect(b%UInt32:e%UInt32, 0xd800:0xdfff)) || unierror(UTF_ERR_INVALID, 0, rng)
    Str(C, _str_cpy(C, rng, Int(e - b) + 1))
end

function convert(::Type{<:Str{C}}, rng::UnitRange{T}) where {C<:CSE,T<:AbstractChar}
    isempty(rng) && return empty_str(C)
    b, e = rng.start, rng.stop
    isvalid(C, b) || unierror(UTF_ERR_INVALID, 1, b)
    isvalid(C, e) || unierror(UTF_ERR_INVALID, length(rng), e)
    # If contains range 0xd800-0xdfff, then also invalid
    isempty(intersect(b%UInt32:e%UInt32, 0xd800:0xdfff)) || unierror(UTF_ERR_INVALID, 0, rng)
    # get counts in range
    Str(C, _str_cpy(C, rng, utf_length(C, b%UInt32, e%UInt32)))
end

function convert(::Type{<:Str{C}}, rng::UnitRange{<:CodeUnitTypes}
                 ) where {C<:Union{UTF8CSE,UTF16CSE}}
    isempty(rng) && return empty_str(C)
    b, e = rng.start, rng.stop
    isvalid(C, b) || unierror(UTF_ERR_INVALID, 1, b)
    isvalid(C, e) || unierror(UTF_ERR_INVALID, length(rng), e)
    # If contains range 0xd800-0xdfff, then also invalid
    isempty(intersect(b%UInt32:e%UInt32, 0xd800:0xdfff)) || unierror(UTF_ERR_INVALID, 0, rng)
    len = utf_length(C, b%UInt32, e%UInt32)
    buf, out = allocate(codeunit(T), len)
    if C === UTF8CSE
        while b <= min(e, 0x7f)
            set_codeunit!(out, b)
            out += 1
            b += 0x01
        end
        while b <= min(e, 0x7ff)
            out = output_utf8_2byte!(out, b)
            b += 0x01
        end
        while b <= min(e, 0xffff)
            out = output_utf8_3byte!(out, b)
            b += 0x01
        end
        while b <= e
            out = output_utf8_4byte!(out, b)
            b += 0x01
        end
    else
        while b <= min(e, 0xffff)
            set_codeunit!(out, b)
            out += 2
            b += 0x01
        end
        while b <= e
            c1, c2 = get_utf16(ch)
            set_codeunit!(out,     c1)
            set_codeunit!(out + 2, c2)
            out += 4
            b += 0x01
        end
    end
    Str(C, buf)
end

convert(::Type{S}, rng::UnitRange{T}) where {T<:AbstractChar,S<:Str{Union{UTF8CSE,UTF16CSE}}} =
    convert(S, (rng.start%UInt32):(rng.stop%UInt32))

convert(::Type{Str}, str::AbstractString) = _str(str)
convert(::Type{Str}, str::String)         = _str(str)
convert(::Type{Str}, str::Str) = str

convert(::Type{<:Str{C}}, str::AbstractString) where {C<:CSE} = convert(Str{C}, _str(str))
convert(::Type{<:Str{C}}, str::Str{C}) where {C<:CSE} = str

convert(::Type{<:Str{RawUTF8CSE}}, str::Str{ASCIICSE}) = Str(RawUTF8CSE, str.data)
convert(::Type{<:Str{RawUTF8CSE}}, str::Str{UTF8CSE})  = Str(RawUTF8CSE, str.data)
convert(::Type{<:Str{RawUTF8CSE}}, str::String)        = Str(RawUTF8CSE, str)

convert(::Type{UniStr}, str::AbstractString) = _str(str)
convert(::Type{UniStr}, str::String)         = _str(str)
convert(::Type{UniStr}, str::Str{<:Union{ASCIICSE,SubSet_CSEs}}) = str

function convert(::Type{T},
                 vec::AbstractArray{UInt8}) where {C<:Union{UTF8CSE,ASCIICSE},T<:Str{C}}
    is_valid(T, vec) || unierror(UTF_ERR_INVALID)
    Str(C, _str_cpy(UInt8, vec, length(vec)))
end

convert(::Type{<:Str{C}}, vec::AbstractArray{UInt8}) where {C<:Union{BinaryCSE,Text1CSE}} =
    Str(C, _str_cpy(UInt8, vec, length(vec)))
convert(::Type{<:Str{Text2CSE}}, vec::AbstractArray{UInt16}) =
    Str(Text2CSE, _str_cpy(UInt16, vec, length(vec)))
convert(::Type{<:Str{Text4CSE}}, vec::AbstractArray{UInt32}) =
    Str(Text4CSE, _str_cpy(UInt32, vec, length(vec)))

(::Type{Str})(str::AbstractString) = _str(str)
(::Type{Str})(str::String)         = _str(str)
(::Type{Str})(str::Str) = str

(::Type{UniStr})(str::AbstractString) = _str(str)
(::Type{UniStr})(str::String)         = _str(str)
(::Type{UniStr})(str::Str{<:Union{ASCIICSE,SubSet_CSEs}}) = str

function convert(::Type{UniStr}, str::T) where {T<:Str}
    # handle zero length string quickly
    is_empty(str) && return empty_ascii
    len, flags = count_chars(T, pointer(str), ncodeunits(str))
    if flags == 0
        Str(ASCIICSE, codeunit(T) == UInt8 ? str.data : _str_cpy(UInt8, str, len))
    elseif (flags & ~(UTF_LATIN1%UInt)) == 0
        Str(_LatinCSE, codeunit(T) == UInt8 ? str.data : _str_cpy(UInt8, str, len))
    elseif (flags & UTF_UNICODE4) == 0
        Str(_UCS2CSE, codeunit(T) == UInt16 ? str.data : _str_cpy(UInt16, str, len))
    else
        Str(_UTF32CSE, codeunit(T) == UInt32 ? str.data : _str_cpy(UInt32, str, len))
    end
end

#convert(::Type{<:Str{C}}, str::String) where {C} = convert(C, _str(str))

convert(::Type{<:Str{Text1CSE}}, str::MaybeSub{String}) = Str(Text1CSE, String(str))
convert(::Type{<:Str{BinaryCSE}}, str::MaybeSub{String}) = Str(BinaryCSE, String(str))
convert(::Type{<:Str{BinaryCSE}}, str::Str{<:Union{ASCIICSE,Latin_CSEs,UTF8_CSEs}}) =
    Str(BinaryCSE, str.data)
convert(::Type{<:Str{BinaryCSE}}, str::SubString{<:Str{<:Union{ASCIICSE,Latin_CSEs,UTF8_CSEs}}}) =
    Str(BinaryCSE, String(str))

#convert(::Type{<:Str{LatinCSE}}, str::Str{_LatinCSE}) = Str(LatinCSE, str.data)
convert(::Type{<:Str{UCS2CSE}},  str::Str{_UCS2CSE})  = Str(UCS2CSE,  str.data)
convert(::Type{<:Str{UTF32CSE}}, str::Str{_UTF32CSE}) = Str(UTF32CSE, str.data)

#convert(::Type{<:Str{LatinCSE}}, str::Str{ASCIICSE})  = Str(LatinCSE, str.data)

convert(::Type{String}, str::Str{<:Union{ASCIICSE,Text1CSE,BinaryCSE}}) = str.data

function convert(::Type{String}, vec::AbstractArray{<:Chr})
    out = get_iobuffer(sizeof(vec))
    @inbounds for ch in vec
        print(out, ch)
    end
    String(take!(out))
end

function convert(::Type{String}, vec::AbstractArray{<:Union{Text1Chr,ASCIIChr}})
    @preserve buf begin
        buf, pnt = _allocate(UInt8, length(vec))
        @inbounds for byt in vec
            set_codeunit!(pnt, byt)
            pnt += 1
        end
        buf
    end
end

function Str(vec::AbstractArray{T}) where {CS,BT,T<:Chr{CS,BT}}
    C = codepoint_cse(T)
    buf, pnt = _allocate(BT, length(vec))
    @inbounds for ch in vec
        set_codeunit!(pnt, ch%BT)
        pnt += sizeof(BT)
    end
    Str(C, buf)
end

"""Convert to a UniStr if valid Unicode, otherwise return a Text1Str"""
function unsafe_str(str::Union{Vector{UInt8}, T, SubString{T}};
                    accept_long_null  = false,
                    accept_surrogates = false,
                    accept_long_char  = false,
                    accept_invalids   = true
                    ) where {T <: Union{BinaryStr, Text1Str, String}}
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_ascii
    @preserve str begin
        pnt = pointer(str)::Ptr{UInt8}
        len, flags, num4byte, num3byte, num2byte, latin1byte, invalids =
            unsafe_check_string(pnt, 1, siz;
                                accept_long_null  = accept_long_null,
                                accept_surrogates = accept_surrogates,
                                accept_long_char  = accept_long_char,
                                accept_invalids   = accept_invalids)
        if flags == 0
            # Don't allow this to be aliased to a mutable Vector{UInt8}
            safe_copy(T, ASCIICSE, str)
        elseif invalids != 0
            safe_copy(T, Text1CSE, str)
        elseif num4byte != 0
            Str(_UTF32CSE, _encode_utf32(pnt, len))
        elseif num2byte + num3byte != 0
            Str(_UCS2CSE, _encode_utf16(pnt, len))
        else
            Str(latin1byte == 0 ? ASCIICSE : _LatinCSE, _encode_ascii_latin(pnt, len))
        end
    end
end

"""Convert to a UniStr if valid Unicode, otherwise return a Text1Str/Text2Str/Text4Str"""
function unsafe_str(str::Union{MaybeSub{<:AbstractString},AbstractArray{T}};
                    accept_long_null  = false,
                    accept_surrogates = false,
                    accept_long_char  = false,
                    accept_invalids   = true
                    ) where {T<:Union{AbstractChar,CodeUnitTypes}}
    # handle zero length string quickly
    is_empty(str) && return empty_ascii
    len, flags, num4byte, num3byte, num2byte, latin1byte, invalids =
        unsafe_check_string(str;
                            accept_long_null  = accept_long_null,
                            accept_surrogates = accept_surrogates,
                            accept_long_char  = accept_long_char,
                            accept_invalids   = accept_invalids)
    if flags == 0
        Str(ASCIICSE, _cvtsize(UInt8, str, len))
    elseif invalids != 0
        # Todo: Make sure this handles different sorts of SubStrings effectively
        siz = sizeof(eltype(T))
        C = siz == 4 ? Text4CSE : (siz == 2 ? Text2CSE : Text1CSE)
        S = codeunit(C)
        buf, pnt = _allocate(S, len)
        @inbounds for ch in str
            set_codeunit!(pnt, ch%S)
            pnt += siz
        end
        Str(C, buf)
    elseif num4byte != 0
        Str(_UTF32CSE, _encode_utf32(str, len))
    elseif num2byte + num3byte != 0
        Str(_UCS2CSE, _encode_utf16(str, len))
    else
        Str(latin1byte == 0 ? ASCIICSE : _LatinCSE, _encode_ascii_latin(str, len))
    end
end

# Fallback constructors for Str types, from any AbstractString
(::Type{T})(vec::S) where {T<:Str, S<:AbstractArray} = convert(T, vec)
(::Type{T})(str::S) where {T<:Str, S<:AbstractString} = convert(T, str)
(::Type{T})(str::S) where {T<:Str, S<:Str} = convert(T, str)
(::Type{T})(str::T) where {T<:Str} = str
