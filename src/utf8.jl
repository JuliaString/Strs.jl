#=
UTF8Str type

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones,
and other contributors to the Julia language

Licensed under MIT License, see LICENSE.md
Based in part on code for UTF8String that used to be in Julia
=#

# UTF-8 support functions

@inline checkcont(pnt) = is_valid_continuation(get_codeunit(pnt))

# Get rest of character ch from 2-byte UTF-8 sequence at pnt - 1
@inline get_utf8_2byte(pnt, ch) =
    (((ch & 0x1f)%UInt16 << 6) | (get_codeunit(pnt) & 0x3f))

# Get rest of character ch from 3-byte UTF-8 sequence at pnt - 2
@inline get_utf8_3byte(pnt, ch) =
    (((ch & 0xf)%UInt16 << 12)
     | ((get_codeunit(pnt - 1)%UInt16 & 0x3f) << 6)
     | (get_codeunit(pnt) & 0x3f))

# Get rest of character ch from 4-byte UTF-8 sequence at pnt - 3
@inline get_utf8_4byte(pnt, ch) =
    (((ch & 0x7)%UInt32 << 18)
     | ((get_codeunit(pnt - 2)%UInt32 & 0x3f) << 12)
     | ((get_codeunit(pnt - 1)%UInt32 & 0x3f) << 6)
     | (get_codeunit(pnt) & 0x3f))

@inline get_utf8_2(ch) =
    0xc0 | (ch >>> 6)%UInt8, 0x80 | (ch & 0x3f)%UInt8
@inline get_utf8_3(ch) =
    (0xe0 | (ch >>> 12)%UInt8, 0x80 | ((ch >>> 6) & 0x3f)%UInt8, 0x80 | (ch & 0x3f)%UInt8)
@inline get_utf8_4(ch) =
    (0xf0 | (ch >>>  18)%UInt8, 0x80 | ((ch >>> 12) & 0x3f)%UInt8,
     0x80 | ((ch >>>  6) & 0x3f)%UInt8, 0x80 | (ch & 0x3f)%UInt8)

# Output a character as a 2-byte UTF-8 sequence
@inline function output_utf8_2byte!(pnt, ch)
    b1, b2 = get_utf8_2(ch)
    set_codeunit!(pnt,     b1)
    set_codeunit!(pnt + 1, b2)
    pnt + 2
end

# Output a character as a 3-byte UTF-8 sequence
@inline function output_utf8_3byte!(pnt, ch)
    b1, b2, b3 = get_utf8_3(ch)
    set_codeunit!(pnt,     b1)
    set_codeunit!(pnt + 1, b2)
    set_codeunit!(pnt + 2, b3)
    pnt + 3
end

# Output a character as a 4-byte UTF-8 sequence
@inline function output_utf8_4byte!(pnt, ch)
    b1, b2, b3, b4 = get_utf8_4(ch)
    set_codeunit!(pnt,     b1)
    set_codeunit!(pnt + 1, b2)
    set_codeunit!(pnt + 2, b3)
    set_codeunit!(pnt + 3, b4)
    pnt + 4
end

@inline eq_bytes(pnt, b1)         = get_codeunit(pnt) == b1
@inline eq_bytes(pnt, b1, b2)     = get_codeunit(pnt+1) == b2 && eq_bytes(pnt, b1)
@inline eq_bytes(pnt, b1, b2, b3) = get_codeunit(pnt+2) == b3 && eq_bytes(pnt, b1, b2)

## required core functionality ##

utf_trail(c::UInt8) = (0xe5000000 >>> ((c & 0xf0) >> 3)) & 0x3

@inline function _lastindex(::CodeUnitMulti, str::MS_UTF8)
    (len = ncodeunits(str)) > 1 || return len
    @preserve str begin
        pnt = pointer(str) + len - 1
        len - (checkcont(pnt) ? (checkcont(pnt - 1) ? checkcont(pnt - 2) + 2 : 1) : 0)
    end
end
@inline function _lastindex(::CodeUnitMulti, str::MaybeSub{<:Str{RawUTF8CSE}})
    (len = ncodeunits(str)) > 1 || return len
    @preserve str begin
        pnt = pointer(str) + len - 1
        len - (checkcont(pnt) ? (checkcont(pnt - 1) ? checkcont(pnt - 2) + 2 : 1) : 0)
    end
end

#=
_cont(byt, n) = (byt >>> n)%UInt8 != 0x80
cnt += 8
else
val &= 0xe0e0_e0e0_e0e0_e0e0
# Count ones that are not valid continuation
cnt += _cont(val, 0x0) + _cont(val, 0x8) + _cont(val, 0x10) + _cont(val, 0x18) +
_cont(val, 0x20) + _cont(val, 0x28) + _cont(val, 0x30) + _cont(val, 0x38)
end

xor 80 then << 1 then |
00 -> 10 -> 1
01 -> 11 -> 1
10 -> 00 -> 0
11 -> 01 -> 1
=#

const hi_mask = 0x8080_8080_8080_8080

@inline _count_cont(v) = (v = xor(v, hi_mask); count_ones(xor(((v << 1) | v), hi_mask) & hi_mask))
@inline msk_lead(v) = (v = xor(v, hi_mask); xor(xor(((v << 1) | v), hi_mask) & hi_mask, hi_mask))

@inline function _align_len_utf8(pnt, cnt, v)
    len = 0
    fin = pnt + cnt
    v = msk_lead(v)
    while (pnt += CHUNKSZ) < fin
        len += count_ones(v)
        v = msk_lead(unsafe_load(pnt))
    end
    len + count_ones(cnt & CHUNKMSK == 0 ? v : (v & _mask_bytes(cnt)))
end

_length_al(::CodeUnitMulti, ::Type{UTF8CSE}, beg::Ptr{UInt8}, cnt::Int) =
    (pnt = reinterpret(Ptr{UInt64}, beg); _align_len_utf8(pnt, cnt, unsafe_load(pnt)))

function _length(::CodeUnitMulti, ::Type{UTF8CSE}, beg::Ptr{UInt8}, cnt::Int)
    align = reinterpret(UInt, beg)
    pnt = reinterpret(Ptr{UInt64}, align & ~CHUNKMSK)
    v = unsafe_load(pnt)
    if (align &= CHUNKMSK) != 0
        msk = _mask_bytes(align)
        v = (v & ~msk) | (msk & hi_mask)
        cnt += align
    end
    _align_len_utf8(pnt, cnt, v)
end

@inline function _check_mask_al(pnt, cnt, msk, v)
    fin = pnt + cnt
    while (pnt += CHUNKSZ) < fin
        (v & msk) == 0 || return false
        v = unsafe_load(pnt)
    end
    (cnt & CHUNKMSK == 0 ? v : (v & _mask_bytes(cnt))) & msk == 0
end
@inline _check_mask_al(pnt, cnt, msk) = _check_mask_al(pnt, cnt, msk, unsafe_load(pnt))

@inline function _check_mask_ul(beg, cnt, msk)
    align = reinterpret(UInt, beg)
    pnt = reinterpret(Ptr{UInt64}, align & ~CHUNKMSK)
    v = unsafe_load(pnt)
    if (align &= CHUNKMSK) != 0
        v &= ~_mask_bytes(align)
        cnt += align
    end
    _check_mask_al(pnt, cnt, msk, v)
end

_ascii_mask(::Type{UInt8})  = hi_mask
_ascii_mask(::Type{UInt16}) = 0xff80_ff80_ff80_ff80
_ascii_mask(::Type{UInt32}) = 0xffffff80_ffffff80

_latin_mask(::Type{UInt16}) = 0xff00_ff00_ff00_ff00
_latin_mask(::Type{UInt32}) = 0xffffff00_ffffff00

const _bmp_mask_32   = 0xffff0000_ffff0000

is_ascii(str::SubString{<:Str{C}}) where {C<:Union{UTF8CSE,LatinCSE,Binary_CSEs,UTF16CSE,UCS2CSE,
                                                   Text2CSE,Text4CSE,UTF32CSE}} =
    (cnt = sizeof(str)) == 0 ? true :
    @preserve str _check_mask_ul(pointer(str), cnt, _ascii_mask(codeunit(C)))

is_ascii(vec::Vector{T}) where {T<:CodeUnitTypes} =
    (cnt = sizeof(vec)) == 0 ? true :
    @preserve str _check_mask_ul(pointer(vec), cnt, _ascii_mask(T))

is_ascii(str::Str{C}) where {C<:Union{UTF8_CSEs,LatinCSE,Binary_CSEs,UTF16CSE,UCS2CSE,
                                      Text2CSE,Text4CSE,UTF32CSE}} =
    (cnt = sizeof(str)) == 0 ? true :
    @preserve str _check_mask_al(reinterpret(Ptr{UInt64}, pointer(str)), cnt,
                                 _ascii_mask(codeunit(C)))

# Todo! Here you need to see that 0b11yyyyxx at least 1 y must be set,
# which indicates a non-Latin1 character
_all_latin(val) = ((val & (val<<1) & (val<<2 | (val<<3) | (val<<4) | (val<<5))) & hi_mask) == 0

@inline function _check_latin_utf8_al(pnt, cnt, v)
    fin = pnt + cnt
    while (pnt += CHUNKSZ) < fin
        _all_latin(v) || return false
        v = unsafe_load(pnt)
    end
    _all_latin(cnt & CHUNKMSK == 0 ? v : (v & _mask_bytes(cnt)))
end
@inline _check_latin_utf8_al(pnt, cnt) = _check_latin_utf8_al(pnt, cnt, unsafe_load(pnt))

@inline function _check_latin_utf8_ul(beg, cnt)
    align = reinterpret(UInt, beg)
    pnt = reinterpret(Ptr{UInt64}, align & ~CHUNKMSK)
    v = unsafe_load(pnt)
    if (align &= CHUNKMSK) != 0
        v &= ~_mask_bytes(align)
        cnt += align
    end
    _check_latin_utf8_al(pnt, cnt, v)
end

is_latin(str::Str{UTF8CSE}) =
    (siz = sizeof(str)) == 0 ? true :
    @preserve str _check_latin_utf8_al(reinterpret(Ptr{UInt64}, pointer(str)), siz)

is_latin(str::SubString{<:Str{UTF8CSE}}) =
    (cnt = sizeof(str)) == 0 ? true : @preserve str _check_latin_utf8_ul(pointer(str), cnt)

is_latin(vec::Vector{T}) where {T<:Union{UInt16,UInt32}} =
    (cnt = sizeof(vec)) == 0 ? true :
    @preserve str _check_mask_ul(pointer(vec), cnt, _latin_mask(T))

is_latin(str::SubString{<:Str{C}}) where {C<:Union{Word_CSEs,Quad_CSEs}} =
    (cnt = sizeof(str)) == 0 ? true :
    @preserve str _check_mask_ul(pointer(str), cnt, _latin_mask(codeunit(C)))

is_latin(str::Str{C}) where {C<:Union{Word_CSEs,Quad_CSEs}} =
    (cnt = sizeof(str)) == 0 ? true :
    @preserve str _check_mask_al(pointer(str), cnt, _latin_mask(codeunit(C)))

# All 4 top bits must be 1 (i.e. 0xfx) for this to be non-BMP
_all_bmp(val) = ((val | (val<<1) | (val<<2) | (val<<3)) & hi_mask) == 0

@inline function _check_bmp_utf8_al(pnt, cnt, v)
    fin = pnt + cnt
    while (pnt += CHUNKSZ) < fin
        _all_bmp(v) || return false
        v = unsafe_load(pnt)
    end
    _all_bmp(cnt & CHUNKMSK == 0 ? v : (v & _mask_bytes(cnt)))
end
@inline _check_bmp_utf8_al(pnt, cnt) = _check_bmp_utf8_al(pnt, cnt, unsafe_load(pnt))

@inline function _check_bmp_utf8_ul(beg, cnt)
    align = reinterpret(UInt, beg)
    pnt = reinterpret(Ptr{UInt64}, align & ~CHUNKMSK)
    v = unsafe_load(pnt)
    if (align &= CHUNKMSK) != 0
        v &= ~_mask_bytes(align)
        cnt += align
    end
    _check_bmp_utf8_al(pnt, cnt, v)
end

is_bmp(str::Str{UTF8CSE}) =
    (cnt = sizeof(str)) == 0 ? true :
    @preserve str _check_bmp_utf8_al(reinterpret(Ptr{UInt64}, pointer(str)), cnt)

is_bmp(str::SubString{<:Str{UTF8CSE}}) =
    (cnt = sizeof(str)) == 0 ? true : @preserve str _check_bmp_utf8_ul(pointer(str), cnt)

is_bmp(str::SubString{<:Str{<:Union{Text4CSE,UTF32CSE}}}) =
    (cnt = sizeof(str)) == 0 ? true : @preserve str _check_mask_ul(pointer(str), cnt, _bmp_mask_32)

is_bmp(str::Str{<:Union{Text4CSE,UTF32CSE}}) =
    (cnt = sizeof(str)) == 0 ? true : @preserve str _check_mask_al(pointer(str), cnt, _bmp_mask_32)

is_unicode(str::MS_UTF8) = true

is_unicode(str::String) = @preserve str _check_utf8_al(ncodeunits(str), pointer(str)) >= 0
is_unicode(str::SubString{String}) = @preserve str _check_utf8(ncodeunits(str), pointer(str)) >= 0

"""
Return index of first invalid codeunit (negative),
0 if all ASCII, or index of first non-ASCII codeunit
"""
check_utf8(str) = @preserve str _check_utf8(ncodeunits(str), pointer(str))

@inline _check_utf8(len, pnt)    = _check_utf8(pnt, pnt, pnt + len)
@inline _check_utf8_al(len, pnt) = _check_utf8(pnt, pnt, pnt + len)

function _check_utf8_rest(pnt, fin, ch)
    while true
        # Check UTF-8 encoding
        if ch < 0xc2
            # Found continuation character or invalid 0xc0/0xc1 
            break
        elseif ch < 0xe0
            # 2-byte UTF-8 sequence (i.e. characters 0x80-0x7ff)
            pnt < fin || break
            is_valid_continuation(get_codeunit(pnt)) || break
            pnt += 1
        elseif ch < 0xf0
            # 3-byte UTF-8 sequence (i.e. characters 0x800-0xffff)
            pnt + 1 < fin || break
            b2 = get_codeunit(pnt)     ; is_valid_continuation(b2) || break
            b3 = get_codeunit(pnt + 1) ; is_valid_continuation(b3) || break
            wrd = ((ch & 0x0f)%UInt32 << 12) | ((b2 & 0x3f)%UInt32 << 6) | (b3 & 0x3f)
            # check for surrogate pairs, make sure correct
            (wrd < 0x0800 || is_surrogate_codeunit(wrd)) && break
            pnt += 2
        elseif ch < 0xf5
            # 4-byte UTF-8 sequence (i.e. characters > 0xffff)
            pnt + 2 < fin || break
            b2 = get_codeunit(pnt)     ; is_valid_continuation(b2) || break
            b3 = get_codeunit(pnt + 1) ; is_valid_continuation(b3) || break
            b4 = get_codeunit(pnt + 2) ; is_valid_continuation(b4) || break
            (((ch & 0x07)%UInt32 << 18) | ((b2 & 0x3f)%UInt32 << 12) |
             ((b3 & 0x3f)%UInt32 << 6) | (b4 & 0x3f)) - 0x10000 < 0x100000 ||
             break
            pnt += 3
        else
            break
        end
        # Skip ascii characters as fast as possible
        while true
            pnt < fin || return C_NULL
            ch = get_codeunit(pnt)
            pnt += 1
            ch < 0x7f || break
        end
    end
    pnt
end

function _check_utf8(beg, pnt, fin)
    while pnt < fin
        ch = get_codeunit(pnt)
        pnt += 1
        if ch > 0x7f
            nxt = _check_utf8_rest(pnt, fin, ch)
            return nxt == C_NULL ? Int(pnt - beg) : -Int(nxt - beg)
        end
    end
    0
end

function _nextcpfun(::CodeUnitMulti, ::Type{UTF8CSE}, pnt)
    ch = get_codeunit(pnt)
    if ch < 0x80
        ch%UInt32, pnt + 1
    elseif ch < 0xe0
        get_utf8_2byte(pnt + 1, ch)%UInt32, pnt + 2
    elseif ch < 0xf0
        get_utf8_3byte(pnt + 2, ch)%UInt32, pnt + 3
    else
        get_utf8_4byte(pnt + 3, ch), pnt + 4
    end
end

# Gets next codepoint
@propagate_inbounds function _next(::CodeUnitMulti, ::Type{T}, str::MS_UTF8,
                                   pos::Int) where {T<:Chr}
    len = ncodeunits(str)
    @boundscheck 0 < pos <= len || boundserr(str, pos)
    @preserve str begin
        pnt = pointer(str) + pos - 1
        ch = get_codeunit(pnt)
        if ch < 0x80
            T(ch), pos + 1
        elseif ch < 0xc0
            index_error(str, pos)
        elseif ch < 0xe0
            T(get_utf8_2byte(pnt + 1, ch)), pos + 2
        elseif ch < 0xf0
            T(get_utf8_3byte(pnt + 2, ch)), pos + 3
        else
            T(get_utf8_4byte(pnt + 3, ch)), pos + 4
        end
    end
end

_next(::CodeUnitMulti, ::Type{T}, str::Str{RawUTF8CSE}, pos::Int) where {T} =
    next(str.data, pos)
_next(::CodeUnitMulti, ::Type{T}, str::SubString{<:Str{RawUTF8CSE}}, pos::Int) where {T} =
    next(SubString(str.string.data, s.offset + pos, s.offset + ncodeunits(s)), 1)

done(str::Str{<:UTF8_CSEs}, i::Int) = i > sizeof(str.data)

length(it::CodePoints{<:AbstractString}, i::Int) = length(it.xs)

@propagate_inbounds function next(it::CodePoints{<:AbstractString}, state)
    ch, state = next(it.xs, state)
    UTF32Chr(ch%UInt32), state
end

## overload methods for efficiency ##

@inline _isvalid_char_pos(::CodeUnitMulti, ::Type{<:UTF8_CSEs}, str, pos::Integer) =
    !is_valid_continuation(get_codeunit(pointer(str) + pos - 1))

function _thisind(::CodeUnitMulti, str::MaybeSub{T}, len, pnt,
                  pos::Integer) where {T<:Union{Str{<:UTF8_CSEs},String}}
    pnt += pos - 1
    Int(pos) - (checkcont(pnt) ? (checkcont(pnt - 1) ? checkcont(pnt - 2) + 2 : 1) : 0)
end

@propagate_inbounds function _nextind(::CodeUnitMulti, str::MS_UTF8, pos::Integer)
    pos == 0 && return 1
    numcu = ncodeunits(str)
    @boundscheck 1 <= pos <= numcu || boundserr(str, pos)
    @preserve str begin
        pnt = pointer(str) + pos - 1
        cu = get_codeunit(pnt)
        pos + (cu < 0x80 ? 1
               : (cu < 0xc0
                  ? (pos == numcu ? 1
                     : (checkcont(pnt + 1) ? (2 + (pos < numcu - 1 && checkcont(pnt + 2))) : 1))
                  : ifelse(cu < 0xe0, 2, ifelse(cu < 0xf0, 3, 4))))
    end
end

@propagate_inbounds function _prevind(::CodeUnitMulti, str::MS_UTF8, pos::Integer)
    (pos -= 1) == 0 && return 0
    numcu = ncodeunits(str)
    @preserve str begin
        pnt = pointer(str) + pos
        pos == numcu &&
            return pos - (checkcont(pnt-1) ? (checkcont(pnt-2) ? checkcont(pnt-3) + 2 : 1) : 0)
        @boundscheck 0 < pos < numcu || boundserr(str, pos+1)
        if checkcont(pnt)
            pos - (checkcont(pnt - 1) ? checkcont(pnt - 2) + 1 : 0)
        elseif pos != 1
            pos - (checkcont(pnt - 1) ? (checkcont(pnt - 2) ? checkcont(pnt - 3) + 2 : 1) : 0)
        else
            1
        end
    end
end

@propagate_inbounds function _nextind(::CodeUnitMulti, str::MS_UTF8, pos::Int, nchar::Int)
    nchar < 0 && ncharerr(nchar)
    siz = ncodeunits(str)
    @boundscheck 0 <= pos <= siz || boundserr(str, pos)
    siz == 0 && return Int(nchar != 0)
    @preserve str begin
        beg = pointer(str)
        # Get starting position
        if pos == 0
            nchar <= 1 && return nchar
            pnt = beg + utf_trail(get_codeunit(beg)) + 1
            nchar -= 1
        else
            pnt = beg + pos - 1
            nchar == 0 && (checkcont(pnt) ? index_error(str, pos) : return pos)
            cu = get_codeunit(pnt)
            if !is_valid_continuation(cu)
                pnt += utf_trail(cu) + 1
            elseif pos == siz
                # At end of string already
                return siz + 1
            else
                pnt += checkcont(pnt + 1) ? checkcont(pnt + 2) + 2 : 1
            end
        end
        fin = beg + siz
        # pnt should now point to a valid start of a character
        # This could be sped up, by looking at chunks, and if all ASCII (common case),
        # simply move forward 8
        while (nchar -= 1) > 0 && pnt < fin
            pnt += utf_trail(get_codeunit(pnt)) + 1
        end
        Int(pnt - beg + 1)
    end
end

@propagate_inbounds function _prevind(::CodeUnitMulti, str::MS_UTF8, pos::Int, nchar::Int)
    nchar < 0 && ncharerr(nchar)
    numcu = ncodeunits(str)
    @boundscheck 0 < pos <= numcu+1 || boundserr(str, pos)
    numcu == 0 && return Int(nchar == 0)
    @preserve str begin
        beg = pointer(str)
        if pos > numcu
            (nchar -= 1) < 0 && return pos
            pos = numcu
        elseif nchar == 0
            checkcont(beg + pos - 1) ? index_error(str, pos) : return pos
        end
        pnt = beg + pos
        # This could be sped up, by looking at chunks, and if all ASCII (common case),
        # simply move back 8
        while nchar >= 0
            (pnt -= 1) < beg && return 0
            nchar -= !checkcont(pnt)
        end
        Int(pnt - beg + 1)
    end
end

@propagate_inbounds function getindex(str::MS_UTF8, rng::UnitRange{Int})
    isempty(rng) && return SubString(empty_utf8, 1, 0)
    beg = first(rng)
    len = ncodeunits(str)
    @boundscheck 1 <= beg <= len || boundserr(str, beg)
    @preserve str begin
        pnt = pointer(str)
        ch = get_codeunit(pnt, beg)
        is_valid_continuation(ch) && unierror(UTF_ERR_INVALID_INDEX, beg, ch)
        lst = last(rng)
        @boundscheck lst > len && boundserr(str, lst)
        if lst != len
            ch = get_codeunit(pnt, lst)
            is_valid_continuation(ch) && unierror(UTF_ERR_INVALID_INDEX, lst, ch)
        end
    end
    SubString(str, beg, lst)
end

# For now, use string support in Base for unvalidated UTF-8 strings
_nextind(::CodeUnitMulti, str::Str{RawUTF8CSE}, pos::Int, nchar::Int) =
    nextind(str.data, pos, nchar)
_nextind(::CodeUnitMulti, str::Str{RawUTF8CSE}, pos::Int) =
    nextind(str.data, pos)
_prevind(::CodeUnitMulti, str::Str{RawUTF8CSE}, pos::Int, nchar::Int) =
    prevind(str.data, pos, nchar)
_prevind(::CodeUnitMulti, str::Str{RawUTF8CSE}, pos::Int) =
    prevind(str.data, pos)

#=
_nextind(::CodeUnitMulti, str::SubString{<:Str{RawUTF8CSE}}, pos::Int, nchar::Int) =
    nextind(str, pos, nchar)
_nextind(::CodeUnitMulti, str::SubString{<:Str{RawUTF8CSE}}, pos::Int) =
    nextind(str, pos)
_prevind(::CodeUnitMulti, str::SubString{<:Str{RawUTF8CSE}}, pos::Int, nchar::Int) =
    prevind(str, pos, nchar)
_prevind(::CodeUnitMulti, str::SubString{<:Str{RawUTF8CSE}}, pos::Int) =
    prevind(str, pos)
=#

const _ByteStr = Union{Str{ASCIICSE}, SubString{<:Str{ASCIICSE}},
                       Str{UTF8CSE},  SubString{<:Str{UTF8CSE}},
                       String}

string(c::_ByteStr...) = length(c) == 1 ? c[1]::UTF8Str : UTF8Str(_string(c))
    # ^^ at least one must be UTF-8 or the ASCII-only method would get called

function _reverse(::CodeUnitMulti, ::Type{UTF8CSE}, len, pnt::Ptr{T}) where {T<:CodeUnitTypes}
    buf, beg = _allocate(T, len)
    out = beg + len
    while out >= beg
        ch = get_codeunit(pnt)
        if ch > 0xdf
            if ch < 0xf0
                set_codeunit!(out -= 2, get_codeunit(pnt += 1))
                set_codeunit!(out + 1,  get_codeunit(pnt += 1))
            else
                set_codeunit!(out -= 3, get_codeunit(pnt += 1))
                set_codeunit!(out + 1,  get_codeunit(pnt += 1))
                set_codeunit!(out + 2,  get_codeunit(pnt += 1))
            end
        elseif ch > 0x7f
            set_codeunit!(out -= 1, get_codeunit(pnt += 1))
        end
        set_codeunit!(out -= 1, ch)
        pnt += 1
    end
    Str(UTF8CSE, buf)
end

@inline get_ch(dat, pos, off) = (get_codeunit(dat, pos + off) & 0x3f)%UInt32

## transcoding to UTF-8 ##

@propagate_inbounds function _transcode_utf8(pnt::Ptr{UInt8}, len)
    buf, out = _allocate(UInt8, len)
    fin = out + len
    @inbounds while out < fin
        ch = get_codeunit(pnt)
        # Handle ASCII characters
        if ch <= 0x7f
            set_codeunit!(out, ch)
            out += 1
        # Handle overlong < 0x100
        elseif ch < 0xc2
            ch = ((ch & 3) << 6) | (get_codeunit(pnt += 1) & 0x3f)
            set_codeunit!(out, ch)
            out += 1
        # Handle 0x100-0x7ff
        elseif ch < 0xe0
            set_codeunit!(out, ch)
            set_codeunit!(out + 1, get_codeunit(pnt += 1))
            out += 2
        elseif ch != 0xed
            set_codeunit!(out, ch)
            set_codeunit!(out + 1, get_codeunit(pnt += 1))
            set_codeunit!(out + 2, get_codeunit(pnt += 1))
            out += 3
            # Copy 4-byte encoded value
            ch >= 0xf0 && (set_codeunit!(out, get_codeunit(pnt += 1)) ; out += 1)
        # Handle surrogate pairs
        else
            ch = get_codeunit(pnt += 1)
            if ch < 0xa0 # not surrogate pairs
                set_codeunit!(out, 0xed)
                set_codeunit!(out + 1, ch)
                set_codeunit!(out + 2, get_codeunit(pnt += 1))
                out += 3
            else
                # Pick up surrogate pairs (CESU-8 format)
                ch32 = (((ch & 0x3f)%UInt32 << 16) | (get_ch(pnt + 1) << 10)) +
                    (get_ch(pnt + 3) << 6 | get_ch(pnt + 4)) - 0x01f0c00
                pnt += 4
                out = output_utf8_4byte!(out, ch32)
            end
        end
        pnt += 1
    end
    buf
end
_transcode_utf8(dat::Vector{UInt8}, len) = _transcode_utf8(pointer(dat), len)

# Single character conversion
function convert(::Type{<:Str{UTF8CSE}}, ch::Unsigned)
    if ch <= 0x7f
        buf = _allocate(1)
        @inbounds buf[1] = ch%UInt8
    elseif ch <= 0x7ff
        buf = _allocate(2)
        @inbounds buf[1], buf[2] = get_utf8_2(ch)
    elseif ch - 0xd800 < 0x800
        unierror(UTF_ERR_INVALID, 0, ch)
    elseif ch <= 0xffff
        buf = _allocate(3)
        @inbounds buf[1], buf[2], buf[3] = get_utf8_3(ch)
    elseif ch <= 0x10ffff
        buf = _allocate(4)
        @inbounds buf[1], buf[2], buf[3], buf[4] = get_utf8_4(ch)
    else
        unierror(UTF_ERR_INVALID, 0, ch)
    end
    Str(UTF8CSE, buf)
end

convert(::Type{<:SubString{<:Str{UTF8CSE}}}, s::SubString{<:Str{ASCIICSE}}) =
    SubString(Str(UTF8CSE, s), 1)

function convert(::Type{<:Str{UTF8CSE}}, dat::Vector{UInt8})
    # handle zero length string quickly
    isempty(dat) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte =
        @preserve dat fast_check_string(pointer(dat), length(dat))
    #=
    # Copy, but eliminate over-long encodings and surrogate pairs
    if flags & (UTF_LONG | UTF_SURROGATE) == 0
        siz = sizeof(dat)
        buf = _allocate(siz)
        unsafe_copyto!(pointer(buf), pointer(dat), siz)
        Str(UTF8CSE, buf)
    else
    =#
    Str(UTF8CSE, _transcode_utf8(dat, len + latinbyte + num2byte + num3byte*2 + num4byte*3))
    #end
end

function convert(::Type{<:Str{UTF8CSE}}, str::AbstractString)
    # handle zero length string quickly
    isempty(str) && return empty_utf8
    # get number of bytes to allocate
    len, flags, num4byte, num3byte, num2byte, latinbyte = unsafe_check_string(str)
    if flags == 0
        # Speed this up if only ASCII, no overlong encodings
        buf, pnt = _allocate(UInt8, len)
        for ch in str
            set_codeunit!(pnt, ch%UInt8)
            pnt += 1
        end
    else
        # Copy, but eliminate over-long encodings and surrogate pairs
        buf, out = _allocate(UInt8, len + latinbyte + num2byte + num3byte*2 + num4byte*3)
        for ch in str
            out = _encode_char_utf8(out, codepoint(ch))
        end
    end
    Str(UTF8CSE, buf)
end

function convert(::Type{<:Str{UTF8CSE}}, str::Union{MS_ByteStr, MS_RawUTF8})
    # handle zero length string quickly
    (siz = sizeof(str)) == 0 && return empty_utf8
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, siz)
        # Copy, but eliminate over-long encodings and surrogate pairs
        # Speed this up if no surrogates, long encodings
        Str(UTF8CSE,
            (flags & (UTF_LONG | UTF_SURROGATE) == 0
             ? _copysub(str)
             : _transcode_utf8(pnt, len + latinbyte + num2byte + num3byte*2 + num4byte*3)))
    end
end

function convert(::Type{<:Str{UTF8CSE}}, str::MaybeSub{<:Str{<:Union{Text2CSE,Text4CSE}}})
    # handle zero length string quickly
    (len = ncodeunits(str)) == 0 && return empty_utf8
    @preserve str begin
        pnt = pointer(str)
        # get number of bytes to allocate
        len, flags, num4byte, num3byte, num2byte, latinbyte = fast_check_string(pnt, len)
        # Copy, but eliminate over-long encodings and surrogate pairs
        # Speed this up if no surrogates, long encodings
        Str(UTF8CSE,
            flags == 0
             ? _cvtsize(UInt8, pnt, len)
             : _transcode_utf8(pnt, len + latinbyte + num2byte + num3byte*2 + num4byte*3))
    end
end

convert(::Type{<:Str{UTF8CSE}}, s::Str{ASCIICSE}) = Str(UTF8CSE, s.data)
convert(::Type{<:Str{UTF8CSE}}, s::Str{UTF8CSE}) = s

const WideCodeUnit = Union{UInt16, UInt32}

@inline function _encode_char_utf8(pnt, ch::WideCodeUnit)
    # Handle ASCII characters
    if ch <= 0x7f
        set_codeunit!(pnt, ch)
        pnt + 1
    # Handle 0x80-0x7ff
    elseif ch < 0x800
        output_utf8_2byte!(pnt, ch)
    # Handle 0x10000-0x10ffff
    elseif ch > 0xffff # this is only for T == UInt32, should not be generated for UInt16
        output_utf8_4byte!(pnt, ch)
    # Handle 0x800-0xd7ff, 0xe000-0xffff UCS-2 characters
    else
        output_utf8_3byte!(pnt, ch)
    end
end

"""
Converts an already validated vector of `UInt16` or `UInt32` to a `UTF8Str`

Input Arguments:

* `dat` Vector of code units (`UInt16` or `UInt32`)
* `len` length of output in bytes

Returns:

* `UTF8Str`
"""
function _encode_utf8(pnt::Ptr{T}, len) where {T<:WideCodeUnit}
    buf, out = _allocate(UInt8, len)
    fin = out + len
    while out < fin
        out = _encode_char_utf8(out, get_codeunit(pnt))
        pnt += sizeof(T)
    end
    buf
end
_encode_utf8(dat::Vector{<:WideCodeUnit}, len) = _encode_utf8(pointer(dat), len)

function _transcode_utf8(pnt::Ptr{T}, len) where {T<:WideCodeUnit}
    buf, out = _allocate(UInt8, len)
    fin = out + len
    while out < fin
        ch = get_codeunit(pnt)
        out = (is_surrogate_codeunit(ch)
               ? output_utf8_4byte!(out, get_supplementary(ch, get_codeunit(pnt += sizeof(T))))
               : _encode_char_utf8(out, ch))
        pnt += sizeof(T)
    end
    buf
end
_transcode_utf8(dat::Vector{<:WideCodeUnit}, len) = _transcode_utf8(pointer(dat), len)

function convert(::Type{<:Str{UTF8CSE}}, str::MS_UTF16)
    # handle zero length string quickly
    (siz = ncodeunits(str)) == 0 && return empty_utf8
    @preserve str begin
        pnt = pointer(str)
        len, flags, num4byte, num3byte, num2byte, latinbyte = count_chars(UTF16Str, pnt, siz)
        if flags == 0
            Str(UTF8CSE, _cvtsize(UInt8, pnt, len))
        elseif num4byte == 0
            Str(UTF8CSE, _encode_utf8(pnt, len + latinbyte + num2byte + num3byte*2))
        else
            Str(UTF8CSE, _transcode_utf8(pnt, len + latinbyte + num2byte + num3byte*2 + num4byte*3))
        end
    end
end

function convert(::Type{<:Str{UTF8CSE}},
                 str::MaybeSub{T}) where {C<:Union{UCS2_CSEs,UTF32_CSEs},T<:Str{C}}
    # handle zero length string quickly
    (siz = ncodeunits(str)) == 0 && return empty_utf8
    @preserve str begin
        pnt = pointer(str)
        len, flags, num4byte, num3byte, num2byte, latinbyte = count_chars(T, pnt, siz)
        Str(UTF8CSE, (flags == 0
                      ? _cvtsize(UInt8, pnt, len)
                      : _encode_utf8(pnt, len + latinbyte + num2byte + num3byte*2 + num4byte*3)))
    end
end
