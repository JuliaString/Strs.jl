#=
UTF16Str and UCS2Str types (UTF-16 encoding and pure BMP UCS-2)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for UTF16String that used to be in Julia
=#

const _trail_mask = 0xdc00_dc00_dc00_dc00
const _hi_bit_16  = 0x8000_8000_8000_8000

@inline _mask_surr(v)  = xor((v | v<<1 | v<<2 | v<<3 | v<<4 | v<<5) & _hi_bit_16, _hi_bit_16)
@inline _get_masked(qpnt) = _mask_surr(xor(unsafe_load(qpnt), _trail_mask))

@inline get_utf16(ch) = (0xd7c0 + (ch >> 10))%UInt16, (0xdc00 + (ch & 0x3ff))%UInt16

# These only work when no SubStr field
function _length(::CodeUnitMulti, str::T) where {T<:Str{UTF16CSE,Nothing}}
    (siz = sizeof(str)) == 0 && return 0
    siz == 2 && return 1
    @preserve str begin
        cnt = chroff(UInt16, siz)
        pnt, fin = _calcpnt(str, siz)
        while (pnt += CHUNKSZ) <= fin
            cnt -= count_ones(_get_masked(pnt))
        end
        pnt - CHUNKSZ == fin ? cnt : (cnt - count_ones(_get_masked(pnt) & _mask_bytes(siz)))
    end
end

function is_ascii(str::T) where {T<:Str{<:Union{Text2CSE, UCS2CSE, UTF16CSE},Nothing}}
    (siz = sizeof(str)) == 0 && return true
    @preserve str begin
        siz < CHUNKSZ &&
            return ((unsafe_load(_pnt64(str)) & _mask_bytes(siz)) & _ascii_mask(UInt16)) == 0
        pnt, fin = _calcpnt(str, siz)
        while (pnt += CHUNKSZ) <= fin
            (unsafe_load(pnt) & _ascii_mask(UInt16)) == 0 || return false
        end
        pnt - CHUNKSZ == fin || ((unsafe_load(pnt) & _mask_bytes(siz)) & _ascii_mask(UInt16)) == 0
    end
end

function is_latin(str::T) where {T<:Str{<:Union{Text2CSE, UCS2CSE, UTF16CSE},Nothing}}
    (siz = sizeof(str)) == 0 && return true
    @preserve str begin
        siz < CHUNKSZ &&
            return ((unsafe_load(_pnt64(str)) & _mask_bytes(siz)) & _latin_mask(UInt16)) == 0
        pnt, fin = _calcpnt(str, siz)
        while (pnt += CHUNKSZ) <= fin
            (unsafe_load(pnt) & _latin_mask) == 0 || return false
        end
        pnt - CHUNKSZ == fin || ((unsafe_load(pnt) & _mask_bytes(siz)) & _latin_mask(UInt16)) == 0
    end
end

# Check for any surrogate characters
function is_bmp(str::T) where {T<:Str{UTF16CSE,Nothing}}
    (siz = sizeof(str)) == 0 && return true
    @preserve str begin
        siz < CHUNKSZ && return (_get_masked(_pnt64(str)) & _mask_bytes(siz)) == 0

        pnt, fin = _calcpnt(str, siz)
        while (pnt += CHUNKSZ) <= fin
            _get_masked(pnt) == 0 || return false
        end
        pnt - CHUNKSZ == fin || (_get_masked(pnt) & _mask_bytes(siz)) == 0
    end
end

is_ascii(str::Str{_UCS2CSE}) = false
is_latin(str::Str{_UCS2CSE}) = false
is_bmp(str::UCS2Strings) = true

# Speed this up accessing 64 bits at a time
@propagate_inbounds function _cnt_non_bmp(len, pnt::Ptr{UInt16})
    cnt = 0
    @inbounds for i = 1:len
        cnt += is_surrogate_lead(get_codeunit(pnt, i))
    end
    cnt
end

@inline lastindex(str::Str{<:UTF16CSE}) =
    ((len = _len(str)) != 0
     ? (is_surrogate_codeunit(get_codeunit(_pnt(str), len)) ? len-1 : len) : 0)

get_supplementary(lead::Unsigned, trail::Unsigned) = (UInt32(lead-0xd7f7)<<10 + trail)

function _nextcpfun(::CodeUnitMulti, ::Type{UTF16CSE}, pnt)
    ch = get_codeunit(pnt)
    (is_surrogate_lead(ch)
     ? (get_supplementary(ch, get_codeunit(pnt + 2)), pnt + 4)
     : (ch%UInt32, pnt + 2))
end

@propagate_inbounds function _next(::CodeUnitMulti, T, str::Str{<:UTF16CSE}, pos::Int)
    @boundscheck pos <= _len(str) || boundserr(str, pos)
    pnt = _pnt(str) + (pos<<1)
    ch = get_codeunit(pnt - 2)
    (is_surrogate_lead(ch)
     ? (T(get_supplementary(ch, get_codeunit(pnt))), pos + 2)
     : (T(ch), pos + 1))
end

@propagate_inbounds @inline function _thisind(::CodeUnitMulti, str::Str{UTF16CSE}, len, pnt, pos)
    @boundscheck 1 <= pos <= len || boundserr(str, pos)
    pos - is_surrogate_trail(get_codeunit(pnt, pos))
end

@propagate_inbounds @inline function _nextind(::CodeUnitMulti, str::Str{<:UTF16CSE}, pos::Int)
    pos == 0 && return 1
    @boundscheck 1 <= pos <= _len(str) || boundserr(str, pos)
    pos + 1 + is_surrogate_lead(get_codeunit(_pnt(str), pos))
end

@propagate_inbounds @inline function _prevind(::CodeUnitMulti, str::Str{<:UTF16CSE}, pos::Int)
    (pos -= 1) == 0 && return 0
    numcu = _len(str)
    @boundscheck 0 < pos <= numcu || boundserr(str, pos + 1)
    pos - is_surrogate_trail(get_codeunit(_pnt(str), pos))
end

# Todo: _prevind with nchar argument
@propagate_inbounds function _nextind(::CodeUnitMulti, str::Str{<:UTF16CSE}, pos::Int, cnt::Int)
    cnt < 0 && neginderr(str, cnt)
    @boundscheck 0 <= pos <= _len(str) || boundserr(str, pos)
    cnt == 0 && return thisind(str, pos) == pos ? pos : unierror("Invalid position", str, pos)
    pos + cnt + is_surrogate_lead(get_codeunit(_pnt(str), pos + cnt))
end

function reverseind(str::Str{<:UTF16CSE}, i::Integer)
    len, pnt = _lenpnt(str)
    j = len - i
    is_surrogate_trail(get_codeunit(pnt, j)) ? j - 1 : j
end

function _reverse(::CodeUnitMulti, ::Type{UTF16CSE}, len, pnt::Ptr{T}) where {T<:CodeUnitTypes}
    buf, beg = _allocate(UInt16, len)
    out = bytoff(beg, len)
    while out > beg
        ch = get_codeunit(pnt)
        is_surrogate_lead(ch) && set_codeunit!(out -= 2, get_codeunit(pnt += 2))
        set_codeunit!(out -= 2, ch)
        pnt += 2
    end
    Str(UTF16CSE, buf)
end

@inline _isvalid_char_pos(::CodeUnitMulti, str::Str{<:UTF16CSE}, pos::Integer) =
    !is_surrogate_trail(get_codeunit(_pnt(str), pos))

function is_valid(::Type{<:UCS2Strings}, data::AbstractArray{UInt16})
    @inbounds for ch in data
        is_surrogate_codeunit(ch) && return false
    end
    true
end

function is_valid(::Type{<:UCS2Strings}, pnt::Ptr{UInt16}, len)
    pos = 0
    @inbounds while (pos += 1) < len # check for surrogates
        is_surrogate_codeunit(get_codeunit(pnt, pos)) && return false
    end
    true
end

# These need to change to output directly, not converted to UTF-8

# Single character conversion

@inline function _convert_utf_n(::Type{C}, ch::UInt32) where {C<:UTF16CSE}
    buf, pnt = _allocate(UInt16, 2)
    # output surrogate pair
    c1, c2 = get_utf16(ch)
    set_codeunit!(pnt,     c1)
    set_codeunit!(pnt + 1, c2)
    Str(C, buf)
end

convert(::Type{<:Str{C}}, ch::Unsigned) where {C<:UTF16CSE} =
    (is_unicode(ch)
     ? (ch <= 0xffff ? _convert(UTF16CSE, ch%UInt16) : _convert_utf_n(C, ch%UInt32))
     : unierror(UTF_ERR_INVALID, 0, ch))

# Type _UCS2CSE must have at least 1 character > 0xff, so use other types as well
convert(::Type{<:Str{_UCS2CSE}}, ch::Unsigned) =
    (ch > 0xff
     ? (is_bmp(ch) ? _convert(_UCS2CSE, ch%UInt16) : unierror(UTF_ERR_INVALID, 0, ch))
     : _convert(_LatinCSE, ch%UInt8))

function convert(::Type{<:Str{C}}, str::AbstractString) where {C<:UCS2_CSEs}
    is_empty(str) && return empty_str(C)
    # Might want to have an invalids_as argument
    len, flags, num4byte = unsafe_check_string(str)
    num4byte == 0 || unierror(UTF_ERR_INVALID_UCS2)
    buf, pnt = _allocate(UInt16, len)
    @inbounds for ch in str
        set_codeunit!(pnt, ch%UInt16)
        pnt += 2
    end
    Str(C, buf)
end

function convert(::Type{<:Str{C}}, str::String) where {C<:UCS2_CSEs}
    # Might want to have an invalids_as argument
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_str(C)
    # Check that is correct UTF-8 encoding and get number of words needed
    len, flags, num4byte = unsafe_check_string(str, 1, siz)
    num4byte == 0 || unierror(UTF_ERR_INVALID_UCS2)
    # Optimize case where no characters > 0x7f
    Str(C, flags == 0 ? _cvtsize(UInt16, str, len) : _encode_utf16(str, len))
end

# handle zero length string quickly, just widen these
convert(::Type{<:Str{C}}, str::Str{<:Union{ASCIICSE, Latin_CSEs}}) where {C<:UCS2_CSEs} =
    (siz = sizeof(str)) == 0 ? empty_str(C) : Str(C, _cvtsize(UInt16, _pnt(str), siz))

function convert(::Type{<:Str{C}}, str::Str{UTF16CSE}) where {C<:UCS2_CSEs}
    # Might want to have an invalids_as argument
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_str(C)
    # Check if conversion is valid
    is_bmp(str) || unierror(UTF_ERR_INVALID_UCS2)
    Str(C, _cvtsize(UInt16, _pnt(str), len))
end

function is_valid(::Type{<:Str{UTF16CSE}}, data::AbstractArray{UInt16})
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
function is_valid(::Type{<:Str{UTF16CSE}}, data::Ptr{UInt16}, len)
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

function convert(::Type{<:Str{UTF16CSE}}, str::AbstractString)
    is_empty(str) && return empty_utf16
    len, flags, num4byte = unsafe_check_string(str)
    buf, pnt = _allocate(UInt16, len + num4byte)
    @inbounds for ch in str
        c = ch%UInt32
        if c > 0x0ffff
            # output surrogate pair
            set_codeunit!(pnt, (0xd7c0 + (c >>> 10))%UInt16)
            pnt += 2
            c = 0xdc00 + (c & 0x3ff)
        end
        set_codeunit!(pnt, c%UInt16)
        pnt += 2
    end
    Str(UTF16CSE, buf)
end

function convert(::Type{<:Str{UTF16CSE}}, str::String)
    # handle zero length string quickly
    is_empty(str) && return empty_utf16
    # Check that is correct UTF-8 encoding and get number of words needed
    len, flags, num4byte = unsafe_check_string(str, 1, sizeof(str))
    # Optimize case where no characters > 0x7f
    Str(UTF16CSE, flags == 0 ? _cvtsize(UInt16, str, len) : _encode_utf16(str, len + num4byte))
end

function convert(::Type{<:Str{UTF16CSE}}, str::Str{UTF8CSE})
    # handle zero length string quickly
    is_empty(str) && return empty_utf16
    pnt = _pnt(str)
    len, flags, num4byte = count_chars(UTF8Str, pnt, _len(str))
    # Optimize case where no characters > 0x7f
    Str(UTF16CSE, flags == 0 ? _cvtsize(UInt16, pnt, len) : _encode_utf16(pnt, len + num4byte))
end

"""
Converts an already validated UTF-8 encoded vector of `UInt8` to a `UTF16Str`

Input Arguments:

*   `pnt` `Ptr{UInt8}` of UTF-8 encoded data
*   `len` length of output in 16-bit words

Returns:

*   `UTF16Str`
"""
function _encode_utf16(pnt::Ptr{UInt8}, len)
    buf, out = _allocate(UInt16, len)
    fin = out + (len<<1)
    @inbounds while out < fin
        ch = get_codeunit(pnt)%UInt16
        # Handle ASCII characters
        if ch <= 0x7f
        # Handle range 0x80-0x7ff
        elseif ch < 0xe0
            ch = ((ch & 0x1f) << 6) | (get_codeunit(pnt += 1) & 0x3f)
        # Handle range 0x800-0xffff
        elseif ch < 0xf0
            ch = get_utf8_3byte(pnt += 2, ch)
        # Handle range 0x10000-0x10ffff
        else
            ch32 = get_utf8_4byte(pnt += 3, ch)
            # output surrogate pair
            set_codeunit!(out, (0xd7c0 + (ch32 >>> 10))%UInt16)
            out += 2
            ch = (0xdc00 + (ch32 & 0x3ff))%UInt16
        end
        set_codeunit!(out, ch)
        out += 2
        pnt += 1
    end
    buf
end

_encode_utf16(dat::Vector{UInt8}, len) = _encode_utf16(pointer(dat), len)
_encode_utf16(str::String, len)        = _encode_utf16(_pnt(str), len)

@inline _cvt_16_to_utf8(::Type{<:Str{UTF16CSE}}, pnt, len) = _transcode_utf8(pnt, len)
@inline _cvt_16_to_utf8(::Type{<:UCS2Strings}, pnt, len)   = _encode_utf8(pnt, len)
@inline _cvt_16_to_utf8(::Type{<:UTF32Strings}, pnt, len)  = _encode_utf8(pnt, len)

function _cvt_utf8(::Type{T}, str::S) where {T<:Union{String, Str{UTF8CSE}}, S}
    # handle zero length string quickly
    (len = _len(str)) == 0 && return empty_str(T)
    # get number of bytes to allocate (use faster count for validated strings)
    pnt = _pnt(str)
    len, flags, num4byte, num3byte, num2byte, latin1 = count_chars(S, pnt, len)
    Str(cse(T), (flags == 0
                 ? _cvtsize(UInt8, pnt, len)
                 : _cvt_16_to_utf8(S, pnt, len + latin1 + num2byte + num3byte*2 + num4byte*3)))
end

# Split this way to avoid ambiguity errors
convert(::Type{String},  str::Str{<:Union{Wide_CSEs,Text2CSE,Text4CSE}}) = _cvt_utf8(String, str)
convert(::Type{UTF8Str}, str::Str{<:Union{Wide_CSEs,Text2CSE,Text4CSE}}) = _cvt_utf8(UTF8Str, str)

"""
Converts an already validated UTF-32 encoded vector of `UInt32` to a `UTF16Str`

Input Arguments:

*   `dat` `Vector{UInt32}` of UTF-32 encoded data
*   `len` length of output in 16-bit words

Returns:

*   `Vector{UInt8}`
"""
function _encode_utf16(dat::Ptr{UInt32}, len)
    buf, pnt = _allocate(UInt16, len)
    fin = pnt + (len<<1)
    @inbounds while pnt < fin
        ch = get_codeunit(dat)
        dat += 4
        if ch > 0x0ffff
            # Output surrogate pair for 0x10000-0x10ffff
            set_codeunit!(pnt, (0xd7c0 + (ch >>> 10))%UInt16)
            pnt += 2
            ch = 0xdc00 + (ch & 0x3ff)
        end
        set_codeunit!(pnt, ch%UInt16)
        pnt += 2
    end
    buf
end

# Copies because not safe to expose the internal array (would allow mutation)
function convert(::Type{Vector{UInt16}}, str::WordStr)
    len = _len(str)
    vec = create_vector(UInt16, len)
    @inbounds unsafe_copyto!(pointer(vec), _pnt(str), len)
    vec
end

# Todo: Some of these need to be fixed to account for SubStr, when that is added
convert(::Type{T},  str::S) where {T<:UCS2Strings, S<:UCS2Strings} = str
#convert(::Type{UTF16Str}, str::UTF16Str) = str
convert(::Type{UTF16Str}, str::UCS2Strings) = Str(UTF16CSE, str.data)

unsafe_convert(::Type{Ptr{UInt16}}, s::Str{UTF16CSE}) = _pnt(s)

function convert(::Type{UTF16Str}, dat::AbstractVector{UInt16})
    is_empty(dat) && return empty_utf16
    len, flags, num4byte = unsafe_check_string(dat, 1, lastindex(dat))
    # Optimize case where no surrogate characters
    Str(UTF16CSE, flags == 0 ? _cvtsize(UInt16, dat, len) : _encode_utf16(dat, len + num4byte))
end

_convert(pnt::Ptr{T}, len, T1) where {T<:Union{UInt16,UInt16_U,UInt16_S,UInt16_US}} =
    ((ch = unsafe_load(pnt)) == 0xfffe
     ? _convert(reinterpret(Ptr{T1}, pnt + 2), len - 1)
     : (ch == 0xfeff ? _convert(pnt + 2, len - 1) : _convert(pnt, len)))

function _convert(pnt::Ptr{T}, len) where {T}
    buf, out = _allocate(basetype(T), len)
    @inbounds for i in 1:len
        set_codeunit!(out, i, unsafe_load(pnt))
        pnt += sizeof(T)
    end
    buf, out
end

function convert(::Type{UTF16Str}, bytes::AbstractArray{UInt8})
    is_empty(bytes) && return empty_utf16
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
    is_valid(UTF16Str, out, len) || unierror(UTF_ERR_INVALID, 0, 0)
    Str(UTF16CSE, buf)
end

@inline function pushchar!(rst, uc)
    if uc <= 0x0ffff
        push!(rst, uc%UInt16)
    else
        push!(rst, (0xd7c0 + (uc >> 10))%UInt16)
        push!(rst, (0xdc00 + (uc & 0x3ff))%UInt16)
    end
end

"""Handle case where result vector is longer"""
function _maprest(fun, str, len, pnt, fin, buf, out, uc)
    rst = Vector{UInt16}()
    pushchar!(rst, uc)
    while pnt < fin
        ch = get_codeunit(pnt)%UInt32
        # check for surrogate pair
        is_surrogate_lead(ch) &&
            (ch = get_supplementary(ch, get_codeunit(pnt += 2)))
        pushchar!(rst, check_valid(UInt32(fun(ch)), (pnt - pointer(str))>>>1))
        pnt += 2
    end
    # We now have a vector to add to the end of buf
    lenrst = length(rst)
    totbuf, totpnt = _allocate(UInt16, len + lenrst)
    unsafe_copyto!(totpnt, 1, pnt, 1, (out - pointer(buf))>>>1)
    unsafe_copyto!(totpnt, out + 1, pointer(rst), 1, lenrst)
    Str(UTF16CSE, totbuf)
end

function _map(::Type{C}, ::Type{T}, fun, len, str) where {C<:CSE, T<:Str}
    pnt = _pnt(str)
    buf, out = _allocate(UInt16, len)
    surrflag = false
    fin = pnt + sizeof(str)
    outend = out + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)%UInt32
        # check for surrogate pair
        T == UTF16Str && is_surrogate_lead(ch) &&
            (ch = get_supplementary(ch, get_codeunit(pnt += 2)))
        # Note: no checking for invalid here, UTF16Str is always valid
        uc = check_valid(UInt32(fun(ch)), (pnt - pointer(str))>>>1)
        if uc < 0x10000
            out < outend || return _maprest(fun, str, len, buf, pnt, fin, out, uc)
            set_codeunit!(out, uc%UInt16)
            out += 2
        else
            out + 2 < outend || return _maprest(fun, str, len, buf, pnt, fin, out, uc)
            set_codeunit!(out,     (0xd7c0 + (uc >> 10))%UInt16)
            set_codeunit!(out + 2, (0xdc00 + (uc & 0x3ff))%UInt16)
            surrflag = true
            out += 4
        end
        pnt += 2
    end
    out < outend && resize!(buf, out - pointer(buf))
    if !surrflag
        Str(C, buf)
    elseif T == _UCS2Str
        # Convert to 32-bit, to keep result in UniStr type union
        # TODO: check this
        convert(_UTF32Str, Str(UTF16CSE, buf))
    else
        Str(UTF16CSE, buf)
    end
end

map(fun, str::T) where {C<:Union{UCS2CSE, UTF16CSE},T<:Str{C}} =
    (len = _len(str)) == 0 ? empty_str(T) : @preserve str _map(C, T, fun, len, str)
