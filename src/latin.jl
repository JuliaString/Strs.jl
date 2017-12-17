#=
LatinStr/LatinUStr type (ISO Latin1 8-bit subset of Unicode)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for ASCIIString that used to be in Julia
=#

## overload methods for efficiency ##

# isascii is handled in utf8.jl

bytestring(s::LatinStrings) = s

function search(str::LatinStrings, c::UInt32, i::Integer)
    len, dat = _lendata(str)
    i == len + 1 && return 0
    1 <= i <= len && throw(BoundsError(s, i))
    c <= 0xff ? search(dat, c%UInt8, i) : 0
end

rsearch(s::LatinStrings, c::UInt32, i::Integer) =
    c <= 0xff ? rsearch(_data(s), c%UInt8, i) : 0

function string(c::UnicodeByteStrings...)
    length(c) == 1 && return c[1]
    n = 0
    for s in c
        n += _len(s)
    end
    buf = _allocate(n)
    off = 1
    for str in c
        len, dat = _lendata(str)
        unsafe_copyto!(buf, off, dat, 1, len)
        off += len
    end
    LatinUStr(buf)
end

function ucfirst(str::LatinStr)
    dat = _data(str)
    isempty(dat) && return str
    ch = get_codeunit(dat, 1)%LatinChr
    if islower(ch%LatinChr)
        t = copy(dat)
        t[1] = ch - 32
        LatinStr(t)
    else
        str
    end
end

# Special handling for characters that can't map into Latin1
function ucfirst(str::LatinUStr)
    dat = _data(str)
    isempty(dat) && return str
    @inbounds ch = dat[1]
    if islower(ch%LatinChr)
        @inbounds t = copy(dat)
        @inbounds t[1] = ch - 32
        T(t)
    elseif (ch == 0xb5) | (ch == 0xff)
        buf, pnt = _allocate(UInt16, len)
        set_codeunit!(pnt, 1, ifelse(ch == 0xb5, 0x39c, 0x178))
        @inbounds for i = 2:len ; set_codeunit!(pnt, i, dat[i]%UInt16) ; end
        UCS2Str(buf)
    else
        str
    end
end

function lcfirst(str::T) where {T<:LatinStrings}
    dat = _data(str)
    (isempty(dat) || !isupper(dat[1]%LatinChr)) && return str
    @inbounds t = copy(dat)
    @inbounds t[1] += 32
    T(t)
end

function _upper(::Type{LatinStr}, d, i, len)
    td = copy(d)
    @inbounds for j = i:len
        islower(td[j]%LatinChr) && (td[j] -= 32)
    end
    LatinStr(td)
end

function _upper(::Type{LatinUStr}, d, i, len)
    # Need to scan the rest of the string to see if _widenupper needs to be called
    @inbounds begin
        for j = i:len
            ((ch = d[j]) == 0xb5) | (ch == 0xff) && return _widenupper(d, i, len)
        end
        td = copy(d)
        for j = i:len
            islower(td[j]%LatinChr) && (td[j] -= 32)
        end
    end
    LatinUStr(td)
end

function _widenupper(dat, i, len)
    buf, pnt = _allocate(UInt16, len)
    @inbounds for j = 1:i-1
        set_codeunit!(pnt, j, dat[j])
    end
    @inbounds for j = i:len
        ch = dat[j]%UInt8
        if ch == 0xb5
            set_codeunit!(pnt, j, 0x39c)
        elseif ch == 0xff
            set_codeunit!(pnt, j, 0x178)
        else
            set_codeunit!(pnt, j, islower(ch%LatinChr) ? ch - 0x20 : ch)
        end
    end
    UCS2Str(buf)
end

function uppercase(str::LatinStr)
    len, dat = _lendata(str)
    @inbounds for i = 1:len
        islower(dat[1]%LatinChr) && return _upper(LatinStr, dat, i, len)
    end
    str
end

function uppercase(str::LatinUStr)
    len, dat = _lendata(str)
    @inbounds for i = 1:len
        ch = dat[i]
        ((ch == 0xb5) | (ch == 0xff)) && return _widenupper(dat, i, len)
        islower(ch%LatinChr) && return _upper(LatinUStr, dat, i, len)
    end
    str
end

function _lower(::Type{T}, d, i) where {T<:LatinStrings}
    td = copy(d)
    @inbounds for j = i:length(td)
        _islatinupper(td[j]) && (td[j] += 32)
    end
    T(td)
end

function lowercase(str::T) where {T<:LatinStrings}
    len, dat = _lendata(str)
    for i = 1:len
        _islatinupper(dat[i]) && return _lower(T, dat, i)
    end
    str
end

reverse(s::T) where {T<:LatinStrings} = T(reverse(_data(s)))

## outputting Latin 1 strings ##

function write(io::IO, str::LatinStrings)
    len, dat = _lendata(str)
    # Skip and write out ASCII sequences together
    cnt = pos = 0
    while pos < len
        # Skip to first non-ASCII sequence
        i = pos
        ch = 0x0
        while (ch = dat[i += 1]) < 0x80
            if i == len
                # Write out remaining data, from pos+1:i
                write(io, dat[pos+1:i])
                return len + cnt
            end
        end
        # Now we have from pos+1:i-1 that are ASCII
        pos+1 <= i-1 && write(io, dat[pos+1:i-1])
        cnt += 1
        ch = dat[i]
        # Write out two bytes of Latin1 character encoded as UTF-8
        write(io, 0xc0 | (ch >>> 6), 0x80 | (ch & 0x3f))
        pos = i
    end
    len + cnt
end

function convert(::Type{T}, ch::UInt32) where {T<:LatinStrings}
    ch <= 0xff || throw(UnicodeError(UTF_ERR_INVALID_LATIN1))
    buf = _allocate(1)
    buf[1] = ch%UInt8
    T(buf)
end

function convert(::Type{ASCIIStr}, ch::UInt32)
    ch <= 0x7f || throw(UnicodeError(UTF_ERR_INVALID_ASCII))
    buf = _allocate(1)
    buf[1] = ch%UInt8
    ASCIIStr(buf)
end

## transcoding to Latin1 ##

latin1(x) = convert(LatinUStr, x)
convert(::Type{T}, s::T) where {T<:LatinStrings} = s
convert(::Type{T}, s::S) where {T<:LatinStrings,S<:UnicodeByteStrings} = T(_data(s))
convert(::Type{T}, s::String) where {T<:LatinStrings} = convert(T, (Vector{UInt8}(s)))
convert(::Type{T}, s::UTF8Str) where {T<:LatinStrings} = convert(T, _data(s))
convert(::Type{T}, a::Vector{UInt8}) where {T<:LatinStrings} = convert(T, a)

latin1(p::Ptr{UInt8}) =
    latin1(p, p == C_NULL ? Csize_t(0) : ccall(:strlen, Csize_t, (Ptr{UInt8},), p))
function latin1(p::Ptr{UInt8}, len::Integer)
    p == C_NULL && throw(ArgumentError("cannot convert NULL to string"))
    LatinUStr(ccall(:jl_pchar_to_array, Vector{UInt8}, (Ptr{UInt8}, Csize_t), p, len))
end

function convert(::Type{T}, str::AbstractString) where {T<:LatinStrings}
    # Might want to have invalids_as here
    len, flags = unsafe_checkstring(str, 1, endof(str))
    (flags & ~(UTF_LONG|UTF_LATIN)) == 0 || throw(UnicodeError(UTF_ERR_INVALID_LATIN1))
    buf = _allocate(len)
    out = 0
    @inbounds for ch in str
        buf[out += 1] = ch%UInt8
    end
    T(buf)
end
