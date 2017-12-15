#=
LatinStr type (ISO Latin1 8-bit subset of Unicode)

Copyright 2017 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for ASCIIString that used to be in Julia
=#

## overload methods for efficiency ##

function isascii(s::LatinStr)
end

bytestring(s::LatinStr) = s

function search(str::LatinStr, c::Char, i::Integer)
    len, dat = _lendata(str)
    i == len + 1 && return 0
    1 <= i <= len && throw(BoundsError(s, i))
    c%UInt32 <= 0xff ? search(dat, c%UInt8, i) : 0
end

rsearch(s::LatinStr, c::Char, i::Integer) =
    c%UInt32 <= 0xff ? rsearch(_data(s), c%UInt8, i) : 0

function string(c::Unicode8...)
    length(c) == 1 && return c[1]
    n = 0
    for s in c
        n += _len(s)
    end
    buf = _sv(n)
    off = 1
    for str in c
        len, dat = _lendata(str)
        unsafe_copy!(buf, off, dat, 1, len)
        off += len
    end
    LatinStr(buf)
end

@inline _islatinlower(c::UInt8) = 'a'%UInt8 <= c <= 'z'%UInt8 | (0xdf <= c <= 0xfe & !(c == 0xf7))
@inline _islatinupper(c::UInt8) = 'A'%UInt8 <= c <= 'Z'%UInt8 | (0xc0 <= c <= 0xde & !(c == 0xd7))

function ucfirst(str::LatinStr)
    dat = _data(str)
    isempty(dat) && return str
    # Special handling for characters that can't map into Latin1
    if (ch = dat[1]) == 0xb5 || ch == 0xff
        buf, pnt = _allocate(UInt16, len)
        set_codeunit!(pnt, ifelse(ch == 0xb5, 0x39c, 0x178), 1)
        @inbounds for i = 2:len ; set_codeunit!(pnt, dat[i]%UInt16, i) ; end
        UCS2Str(buf)
    elseif _islatinlower(ch)
        t = copy(dat)
        t[1] = ch - 32
        LatinStr(t)
    else
        str
    end
end

function lcfirst(str::LatinStr)
    dat = _data(str)
    (isempty(dat) || !_islatinupper(v[1])) && return str
    t = copy(dat)
    t[1] += 32
    LatinStr(t)
end

function _upper(::Type{LatinStr}, d, i, len)
    td = copy(d)
    @inbounds for j = i:len
        _islatinlower(td[j]) && (td[j] -= 32)
    end
    LatinStr(td)
end

function _widenupper(dat, i, len)
    buf, pnt = _allocate(UInt16, len)
    @inbounds for j = 1:i-1
        set_codeunit!(pnt, dat[j], j)
    end
    @inbounds for j = i:len
        ch = dat[j]%UInt16
        if ch == 0xb5
            set_codeunit!(pnt, 0x39c, j)
        elseif ch == 0xff
            set_codeunit!(pnt, 0x178, j)
        else
            set_codeunit!(pnt, _islatinlower(ch) ? ch - 0x20 : ch, j)
        end
    end
    UCS2Str(buf)
end

function uppercase(str::LatinStr)
    len, dat = _lendata(str)
    @inbounds for i = 1:len
        ch = dat[i]
        (ch == 0xb5 || ch == 0xff) && return _widenupper(dat, i, len)
        _islatinlower(ch) && return _upper(LatinStr, dat, i, len)
    end
    str
end

function _lower(::Type{LatinStr}, d, i)
    td = copy(d)
    @inbounds for j = i:length(td)
        _islatinupper(td[j]) && (td[j] += 32)
    end
    LatinStr(td)
end

function lowercase(str::LatinStr)
    len, dat = _lendata(str)
    for i = 1:len
        _islatinupper(dat[i]) && return _lower(LatinStr, dat, i)
    end
    str
end

reverse(s::LatinStr) = LatinStr(reverse(_data(s)))

## outputting Latin 1 strings ##

function write(io::IO, str::LatinStr)
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

## transcoding to ASCII ##

latin1(x) = convert(LatinStr, x)
convert(::Type{LatinStr}, s::ASCIIStr) = LatinStr(_dat(a))
convert(::Type{LatinStr}, s::LatinStr) = s
convert(::Type{LatinStr}, s::String) = latin1(Vector{UInt8}(s))
convert(::Type{LatinStr}, s::UTF8Str) = latin1(_data(s))
convert(::Type{LatinStr}, a::Vector{UInt8}) = LatinStr(a)

latin1(p::Ptr{UInt8}) =
    latin1(p, p == C_NULL ? Csize_t(0) : ccall(:strlen, Csize_t, (Ptr{UInt8},), p))
function latin1(p::Ptr{UInt8}, len::Integer)
    p == C_NULL && throw(ArgumentError("cannot convert NULL to string"))
    LatinStr(ccall(:jl_pchar_to_array, Vector{UInt8}, (Ptr{UInt8}, Csize_t), p, len))
end

function convert(::Type{LatinStr}, str::AbstractString)
    # Might want to have invalids_as here
    len, flags, num4byte, num3byte, num2byte = unsafe_checkstring(str, 1, endof(str))
    (flags & ~(UTF_LONG|UTF_LATIN)) == 0 || throw(UnicodeError(UTF_ERR_INVALID_LATIN1))
    buf = _sv(len)
    out = 0
    @inbounds for ch in str
        buf[out += 1] = UInt8(ch)
    end
    LatinStr(buf)
end
