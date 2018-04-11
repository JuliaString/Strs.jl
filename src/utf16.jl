#=
UTF16Str and UCS2Str types (UTF-16 encoding and pure BMP UCS-2)

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones,
and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in (small) part on code for UTF16String that used to be in Julia
=#

const _trail_mask = 0xdc00_dc00_dc00_dc00
const _hi_bit_16  = 0x8000_8000_8000_8000

@inline _mask_surr(v)  = xor((v | v<<1 | v<<2 | v<<3 | v<<4 | v<<5) & _hi_bit_16, _hi_bit_16)
@inline _get_masked(v::UInt64) = _mask_surr(xor(v, _trail_mask))
@inline _get_masked(qpnt::Ptr{UInt64}) = _get_masked(unsafe_load(qpnt))
@inline _get_lead(qpnt) = xor(_get_masked(qpnt), _hi_bit_16)

@inline get_utf16(ch) = (0xd7c0 + (ch >> 10))%UInt16, (0xdc00 + (ch & 0x3ff))%UInt16

@inline function _align_len_utf16(pnt, cnt, v)
    len = 0
    fin = pnt + cnt
    while (pnt += CHUNKSZ) < fin
        len += count_ones(v)
        v = _get_lead(pnt)
    end
    len + count_ones((cnt & CHUNKMSK) == 0 ? v : (v & _mask_bytes(cnt)))
end

_length_al(::CodeUnitMulti, ::Type{UTF16CSE}, beg::Ptr{UInt16}, cnt::Int) =
    (pnt = reinterpret(Ptr{UInt64}, beg); _align_len_utf16(pnt, cnt<<1, _get_lead(pnt)))

function _length(::CodeUnitMulti, ::Type{UTF16CSE}, beg::Ptr{UInt16}, cnt::Int)
    align = reinterpret(UInt, beg)
    pnt = reinterpret(Ptr{UInt64}, align & ~CHUNKMSK)
    v = _get_lead(pnt)
    if (align &= CHUNKMSK) != 0
        msk = _mask_bytes(align)
        v = (v & ~msk) | (msk & _trail_mask)
        cnt += (align>>>1)
    end
    _align_len_utf16(pnt, cnt<<1, v)
end

function _nextind(::CodeUnitMulti, str::MS_UTF16, pos::Int, nchar::Int)
    nchar < 0 && ncharerr(nchar)
    siz = ncodeunits(str)
    @boundscheck 0 <= pos <= siz || boundserr(str, pos)
    @preserve str begin
        beg = pointer(str)
        pnt = bytoff(beg, pos - 1)
        fin = bytoff(beg, siz)
        cu = get_codeunit(pnt)
        nchar == 0 && (is_surrogate_trail(cu) ? index_error(str, pos) : return pos)
        is_surrogate_trail(cu) && (pnt += 2)
        # pnt should now point to a valid start of a character
        # This could be sped up, by looking at chunks, and if all ASCII (common case),
        # simply move forward 8
        while (nchar -= 1) > 0 && pnt < fin
            pnt += ifelse(is_surrogate_lead(get_codeunit(pnt)), 4, 2)
        end
        chrdiff(pnt, beg)
    end
end

function _prevind(::CodeUnitMulti, str::MS_UTF16, pos::Int, nchar::Int)
    nchar < 0 && ncharerr(nchar)
    @boundscheck 0 < pos <= ncodeunits(str)+1 || boundserr(str, pos)
    @preserve str begin
        beg = pointer(str)
        pnt = bytoff(beg, pos - 1)
        nchar == 0 && (is_surrogate_trail(get_codeunit(pnt)) ? index_error(str, pos) : return pos)
        # This could be sped up, by looking at chunks, and if all ASCII (common case),
        # simply move back 8
        while pnt >= beg
            pnt -= ifelse(is_surrogate_trail(get_codeunit(pnt)), 4, 2)
            (nchar -= 1) > 0 || break
        end
        chrdiff(pnt + 2, beg)
    end
end

# Check for any surrogate characters
function is_bmp(str::MS_UTF16)
    (siz = sizeof(str)) == 0 && return true
    # Todo: handle unaligned for ARM32
    @preserve str begin
        siz < CHUNKSZ && return (_get_masked(_pnt64(str)) & _mask_bytes(siz)) == 0

        pnt, fin = _calcpnt(str, siz)
        while (pnt += CHUNKSZ) <= fin
            _get_masked(pnt) == 0 || return false
        end
        pnt - CHUNKSZ == fin || (_get_masked(pnt) & _mask_bytes(siz)) == 0
    end
end

@inline function _check_bmp_utf16_al(pnt, cnt, v)
    fin = pnt + cnt
    v = _get_masked(v)
    while (pnt += CHUNKSZ) < fin
        v == 0 || return false
        v = _get_masked(pnt)
    end
    ((cnt & CHUNKMSK) == 0 ? v : (v & _mask_bytes(cnt))) == 0
end
@inline _check_bmp_utf16_al(pnt, cnt) = _check_bmp_utf16_al(pnt, cnt, unsafe_load(pnt))

@inline function _check_bmp_utf16_ul(beg, cnt)
    align = reinterpret(UInt, beg)
    pnt = reinterpret(Ptr{UInt64}, align & ~CHUNKMSK)
    v = unsafe_load(pnt)
    if (align &= CHUNKMSK) != 0
        v &= ~_mask_bytes(align)
        cnt += align
    end
    _check_bmp_utf16_al(pnt, cnt, v)
end

is_bmp(str::Str{UTF16CSE}) =
    (cnt = sizeof(str)) == 0 ? true :
    @preserve str _check_bmp_utf16_al(reinterpret(Ptr{UInt64}, pointer(str)), cnt)

is_bmp(str::SubString{<:Str{UTF16CSE}}) =
    (cnt = sizeof(str)) == 0 ? true : @preserve str _check_bmp_utf16_ul(pointer(str), cnt)

is_bmp(str::MaybeSub{<:Str{<:UCS2_CSEs}}) = true

is_ascii(str::Str{_UCS2CSE}) = false
is_latin(str::Str{_UCS2CSE}) = false

is_ascii(str::SubString{<:Str{_UCS2CSE}}) = is_ascii(Str{UCS2CSE}(str.data))
is_latin(str::SubString{<:Str{_UCS2CSE}}) = is_latin(Str{UCS2CSE}(str.data))

# Speed this up accessing 64 bits at a time
@propagate_inbounds function _cnt_non_bmp(len, pnt::Ptr{UInt16})
    cnt = 0
    fin = bytoff(pnt, len)
    while pnt < fin
        cnt += is_surrogate_lead(get_codeunit(pnt))
        pnt += 2
    end
    cnt
end

@inline _lastindex(::CodeUnitMulti, str::MS_UTF16) =
    ((len = ncodeunits(str)) != 0
     ? (is_surrogate_codeunit(get_codeunit(pointer(str), len)) ? len-1 : len) : 0)

get_supplementary(lead::Unsigned, trail::Unsigned) = (UInt32(lead-0xd7f7)<<10 + trail)

function _nextcpfun(::CodeUnitMulti, ::Type{UTF16CSE}, pnt)
    ch = get_codeunit(pnt)
    (is_surrogate_lead(ch)
     ? (get_supplementary(ch, get_codeunit(pnt + 2)), pnt + 4)
     : (ch%UInt32, pnt + 2))
end

@propagate_inbounds function _next(::CodeUnitMulti, T, str::MS_UTF16, pos::Int)
    @boundscheck pos <= ncodeunits(str) || boundserr(str, pos)
    @preserve str begin
        pnt = bytoff(pointer(str), pos)
        ch = get_codeunit(pnt - 2)
        (is_surrogate_lead(ch)
         ? (T(get_supplementary(ch, get_codeunit(pnt))), pos + 2)
         : (T(ch), pos + 1))
    end
end

@inline _thisind(::CodeUnitMulti, str::MS_UTF16, len, pnt, pos) =
    Int(pos) - is_surrogate_trail(get_codeunit(pnt, pos))

@propagate_inbounds @inline function _nextind(::CodeUnitMulti, str::MS_UTF16, pos::Int)
    pos == 0 && return 1
    @boundscheck 1 <= pos <= ncodeunits(str) || boundserr(str, pos)
    @preserve str pos + 1 + is_surrogate_lead(get_codeunit(pointer(str), pos))
end

@propagate_inbounds @inline function _prevind(::CodeUnitMulti, str::MS_UTF16, pos::Int)
    (pos -= 1) == 0 && return 0
    numcu = ncodeunits(str)
    @boundscheck 0 < pos <= numcu || boundserr(str, pos + 1)
    @preserve str pos - is_surrogate_trail(get_codeunit(pointer(str), pos))
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

@inline _isvalid_char_pos(::CodeUnitMulti, ::Type{UTF16CSE}, str, pos::Integer) =
    @preserve str !is_surrogate_trail(get_codeunit(pointer(str), pos))

function is_valid(::Type{<:Str{<:UCS2_CSEs}}, data::AbstractArray{UInt16})
    @inbounds for ch in data
        is_surrogate_codeunit(ch) && return false
    end
    true
end

function is_valid(::Type{<:Str{<:UCS2_CSEs}}, pnt::Ptr{UInt16}, len)
    fin = bytoff(pnt, len)
    while pnt < fin # check for surrogates
        is_surrogate_codeunit(get_codeunit(pnt)) && return false
        pnt += 2
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
    len, flags, num4byte, num3byte = unsafe_check_string(str)
    num4byte == 0 || unierror(UTF_ERR_INVALID_UCS2)
    buf, pnt = _allocate(UInt16, len)
    @inbounds for ch in str
        set_codeunit!(pnt, ch%UInt16)
        pnt += 2
    end
    Str((C === _UCS2CSE && num3byte != 0) ? _UCS2CSE : UCS2CSE, buf)
end

function convert(::Type{<:Str{C}}, str::MS_ByteStr) where {C<:UCS2_CSEs}
    # Might want to have an invalids_as argument
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_str(C)
    @preserve str begin
        pnt = pointer(str)
        # Check that is correct UTF-8 encoding and get number of words needed
        len, flags, num4byte, num3byte = fast_check_string(pnt, siz)
        num4byte == 0 || unierror(UTF_ERR_INVALID_UCS2)
        # Optimize case where no characters > 0x7f
        Str((C === _UCS2CSE && num3byte != 0) ? _UCS2CSE : UCS2CSE,
            flags == 0 ? _cvtsize(UInt16, pnt, len) : _encode_utf16(pnt, len))
    end
end

# handle zero length string quickly, just widen these
convert(::Type{<:Str{C}},
        str::MaybeSub{<:Str{Union{ASCIICSE, Latin_CSEs}}}) where {C<:UCS2_CSEs} =
    (siz = sizeof(str)) == 0 ? empty_str(C) : Str(C, _cvtsize(UInt16, pointer(str), siz))

function convert(::Type{<:Str{C}}, str::MS_UTF16) where {C<:UCS2_CSEs}
    # Might want to have an invalids_as argument
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_str(C)
    # Check if conversion is valid
    is_bmp(str) || unierror(UTF_ERR_INVALID_UCS2)
    @preserve str Str(C, _cvtsize(UInt16, pointer(str), len))
end

function is_valid(::Type{<:Str{UTF16CSE}}, data::AbstractArray{UInt16})
    (len = length(data) - 1) < 0 && return true
    pos = 0
    @inbounds while pos < len # check for unpaired surrogates
        ch = data[pos += 1]
        is_surrogate_codeunit(ch) && is_surrogate_lead(ch) &&
            is_surrogate_trail(data[pos += 1]) ||
            return false
    end
    @inbounds return pos > len || !is_surrogate_codeunit(get_codeunit(data[pos + 1]))
end

# This can be sped up, to check 4 words at a time, only checking for unpaired
# or out of order surrogates when one is found in the UInt64
function is_valid(::Type{<:Str{UTF16CSE}}, pnt::Ptr{UInt16}, len)
    len == 0 && return true
    fin = bytoff(pnt, len - 1)
    while pnt < fin # check for unpaired surrogates
        ch = get_codeunit(pnt)
        is_surrogate_codeunit(ch) && is_surrogate_lead(ch) &&
            is_surrogate_trail(get_codeunit(pnt += 2)) ||
            return false
        pnt += 2
    end
    pnt > fin || !is_surrogate_codeunit(get_codeunit(pnt))
end

function convert(::Type{<:Str{UTF16CSE}}, str::AbstractString)
    is_empty(str) && return empty_utf16
    len, flags, num4byte = unsafe_check_string(str)
    buf, pnt = _allocate(UInt16, len + num4byte)
    @inbounds for ch in str
        c = ch%UInt32
        if c > 0x0ffff
            # output surrogate pair
            w1, w2 = get_utf16(c)
            set_codeunit!(pnt, w1)
            set_codeunit!(pnt+2, w2)
            pnt += 4
        else
            set_codeunit!(pnt, c%UInt16)
            pnt += 2
        end
    end
    Str(UTF16CSE, buf)
end

function convert(::Type{<:Str{UTF16CSE}}, str::MS_ByteStr)
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_utf16
    @preserve str begin
        pnt = pointer(str)
        # Check that is correct UTF-8 encoding and get number of words needed
        len, flags, num4byte = fast_check_string(pnt, siz)
        # Optimize case where no characters > 0x7f
        Str(UTF16CSE, flags == 0 ? _cvtsize(UInt16, pnt, len) : _encode_utf16(pnt, len + num4byte))
    end
end

function convert(::Type{<:Str{UTF16CSE}}, str::MS_UTF8)
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_utf16
    @preserve str begin
        pnt = pointer(str)
        len, flags, num4byte = count_chars(UTF8Str, pnt, siz)
        # Optimize case where no characters > 0x7f
        Str(UTF16CSE, flags == 0 ? _cvtsize(UInt16, pnt, len) : _encode_utf16(pnt, len + num4byte))
    end
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
            w1, ch = get_utf16(ch32)
            set_codeunit!(out, w1)
            out += 2
        end
        set_codeunit!(out, ch)
        out += 2
        pnt += 1
    end
    buf
end

_encode_utf16(dat::Vector{UInt8}, len) = @preserve dat _encode_utf16(pointer(dat), len)
_encode_utf16(str::MS_ByteStr, len)    = @preserve str _encode_utf16(pointer(str), len)

@inline _cvt_16_to_utf8(::Type{<:Str{UTF16CSE}}, pnt, len)     = _transcode_utf8(pnt, len)
@inline _cvt_16_to_utf8(::Type{<:Str{<:UCS2_CSEs}}, pnt, len)  = _encode_utf8(pnt, len)
@inline _cvt_16_to_utf8(::Type{<:Str{<:UTF32_CSEs}}, pnt, len) = _encode_utf8(pnt, len)

function _cvt_utf8(::Type{T}, str::S) where {T<:Union{String, Str{UTF8CSE}}, S}
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_str(T)
    @preserve str begin
        # get number of bytes to allocate (use faster count for validated strings)
        pnt = pointer(str)
        len, flags, num4byte, num3byte, num2byte, latin1 = count_chars(S, pnt, len)
        Str(cse(T),
            (flags == 0
             ? _cvtsize(UInt8, pnt, len)
             : _cvt_16_to_utf8(S, pnt, len + latin1 + num2byte + num3byte*2 + num4byte*3)))
    end
end

# Split this way to avoid ambiguity errors
convert(::Type{String},  str::MaybeSub{<:Str{Union{Word_CSEs,Quad_CSEs}}}) =
    _cvt_utf8(String, str)
convert(::Type{<:Str{UTF8CSE}}, str::MaybeSub{<:Str{Union{Word_CSEs,Quad_CSEs}}}) =
    _cvt_utf8(UTF8Str, str)

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
            w1, w2 = get_utf16(ch)
            set_codeunit!(pnt,   w1)
            set_codeunit!(pnt+2, w2)
            pnt += 4
        else
            set_codeunit!(pnt, ch%UInt16)
            pnt += 2
        end
    end
    buf
end

# Copies because not safe to expose the internal array (would allow mutation)
function convert(::Type{Vector{UInt16}}, str::Str{<:Word_CSEs})
    len = ncodeunits(str)
    vec = create_vector(UInt16, len)
    @inbounds unsafe_copyto!(pointer(vec), pointer(str), len)
    vec
end

# Todo: Some of these need to be fixed to account for SubStr, when that is added
convert(::Type{T},  str::MaybeSub{T}) where {T<:Str{<:Union{UCS2_CSEs, UTF32_CSEs}}} = str
convert(::Type{<:Str{UTF16CSE}}, str::MaybeSub{<:Str{<:UCS2_CSEs}}) = Str(UTF16CSE, str.data)

unsafe_convert(::Type{Ptr{UInt16}}, s::MS_UTF16) = pointer(s)

function convert(::Type{<:Str{UTF16CSE}}, dat::AbstractArray{UInt16})
    is_empty(dat) && return empty_utf16
    len, flags, num4byte = unsafe_check_string(dat, 1, lastindex(dat))
    # Optimize case where no surrogate characters
    Str(UTF16CSE, flags == 0 ? _cvtsize(UInt16, dat, len) : _encode_utf16(dat, len + num4byte))
end

function convert(::Type{<:Str{UTF16CSE}}, dat::Vector{UInt16})
    (len = length(dat)) == 0 && return empty_utf16
    @preserve dat fast_check_string(pointer(dat), len)
    Str(UTF16CSE, _copysub(dat))
end

function convert(::Type{<:Str{UTF16CSE}}, str::MaybeSub{<:Str{Text2CSE}})
    (len = ncodeunits(str)) == 0 && return empty_utf16
    @preserve str fast_check_string(pointer(str), len)
    Str(UTF16CSE, _copysub(str))
end

convert(::Type{<:Str{Text2CSE}},
        str::MaybeSub{<:Str{C}}) where {C<:Union{ASCIICSE,Text1CSE,BinaryCSE,Latin_CSEs}} =
            @preserve str Str(Text2CSE, _cvtsize(UInt16, pointer(str), ncodeunits(str)))

function convert(::Type{<:Str{C}},
                 str::MaybeSub{<:Str{T}}) where {C<:UCS2_CSEs,T<:Union{Text2CSE,Text4CSE}}
    (len = ncodeunits(str)) == 0 && return C === _UCS2CSE ? empty_ascii : empty_ucs2
    @preserve str begin
        pnt = pointer(str)
        len, flags, num4byte, num3byte, num2byte, latin1byte = fast_check_string(pnt, len)
        num4byte == 0 || unierror(UTF_ERR_INVALID_UCS2)
        (C === UCS2CSE || (num2byte+num3byte) != 0) &&
            return Str(C, T===Text2CSE ? _copysub(str) : _cvtsize(UInt16, pnt, len))
        Str(latin1byte == 0 ? ASCIICSE : _LatinCSE, _cvtsize(UInt8, pnt, len))
    end
end

_convert(pnt::Ptr{T}, len, T1) where {T<:Union{UInt16,UInt16_U,UInt16_S,UInt16_US}} =
    ((ch = unsafe_load(pnt)) == 0xfffe
     ? _convert(reinterpret(Ptr{T1}, pnt + 2), len - 1)
     : (ch == 0xfeff ? _convert(pnt + 2, len - 1) : _convert(pnt, len)))

function _convert(pnt::Ptr{T}, len) where {T}
    BT = basetype(T)
    buf, out = _allocate(BT, len)
    fin = bytoff(out, len)
    while out < fin
        set_codeunit!(out, get_codeunit(pnt))
        pnt += sizeof(T)
        out += sizeof(BT)
    end
    buf, out
end

function convert(::Type{<:Str{UTF16CSE}}, bytes::AbstractArray{UInt8})
    is_empty(bytes) && return empty_utf16
    # Note: there are much better ways of detecting what the likely encoding is here,
    # this only deals with big or little-ending UTF-32
    # It really should detect at a minimum UTF-8, UTF-16 big and little
    len = length(bytes)
    isodd(len) && unierror(UTF_ERR_ODD_BYTES_16, len, 0)
    len >>>= 1
    @preserve bytes begin
        pnt = pointer(bytes)
        if isodd(reinterpret(UInt, pnt))
            buf, out = _convert(reinterpret(Ptr{UInt16_U}, pnt), len, swappedtype(UInt16_U))
        else
            buf, out = _convert(reinterpret(Ptr{UInt16}, pnt), len, swappedtype(UInt16))
        end
        is_valid(UTF16Str, out, len) || unierror(UTF_ERR_INVALID, 0, 0)
        Str(UTF16CSE, buf)
    end
end

@inline function pushchar!(rst, uc)
    if uc <= 0x0ffff
        push!(rst, uc%UInt16)
    else
        w1, w2 = get_utf16(uc)
        push!(rst, w1, w2)
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
        pushchar!(rst, check_valid(UInt32(fun(ch%UTF32Chr)), chrdiff(pnt, pointer(str))))
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
    pnt = pointer(str)
    buf = Base.StringVector(len * sizeof(UInt16))
    beg = out = reinterpret(Ptr{UInt16}, pointer(buf))
    surrflag = false
    fin = pnt + sizeof(str)
    outend = out + sizeof(str)
    while pnt < fin
        ch = get_codeunit(pnt)%UInt32
        # check for surrogate pair
        T == UTF16Str && is_surrogate_lead(ch) &&
            (ch = get_supplementary(ch, get_codeunit(pnt += 2)))
        # Note: no checking for invalid here, UTF16Str is always valid
        uc = check_valid(UInt32(fun(ch%UTF32Chr)), chrdiff(pnt, pointer(str)))
        if uc < 0x10000
            out < outend || return _maprest(fun, str, len, buf, pnt, fin, out, uc)
            set_codeunit!(out, uc%UInt16)
            out += 2
        else
            out + 2 < outend || return _maprest(fun, str, len, buf, pnt, fin, out, uc)
            w1, w2 = get_utf16(uc)
            set_codeunit!(out, w1)
            set_codeunit!(out + 2, w2)
            surrflag = true
            out += 4
        end
        pnt += 2
    end
    out < outend && resize!(buf, chrdiff(UInt16, out - beg))
    s = String(buf)
    if !surrflag
        Str(C, s)
    elseif T == _UCS2Str
        # Convert to 32-bit, to keep result in UniStr type union
        # TODO: check this
        convert(_UTF32Str, Str(UTF16CSE, s))
    else
        Str(UTF16CSE, s)
    end
end

map(fun, str::MaybeSub{T}) where {C<:Union{UCS2_CSEs, UTF16CSE},T<:Str{C}} =
    (len = ncodeunits(str)) == 0 ? empty_str(T) : @preserve str _map(C, T, fun, len, str)
