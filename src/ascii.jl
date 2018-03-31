#=
ASCIIStr type

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones,
and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on code for ASCIIString that used to be in Julia
=#

## overload methods for efficiency ##

function _string(coll)
    n = 0
    for str in coll
        n += ncodeunits(str)
    end
    buf, out = _allocate(UInt8, n)
    for str in coll
        @preserve str begin
            len = ncodeunits(str)
            unsafe_copyto!(out, pointer(str), len)
            out += len
        end
    end
    buf
end

string(c::MaybeSub{<:Str{ASCIICSE}}...) = length(c) == 1 ? c[1] : Str(ASCIICSE, _string(c))

## transcoding to ASCII ##

function convert(::Type{<:Str{ASCIICSE}}, str::AbstractString)
    isempty(str) && return empty_ascii
    len = length(str)
    buf, pnt = _allocate(UInt8, len)
    @inbounds for ch in str
        is_ascii(ch) || unierror(UTF_ERR_INVALID_ASCII, pnt - pointer(buf) + 1, ch)
        set_codeunit!(pnt, ch%UInt8)
        pnt += 1
    end
    Str(ASCIICSE, buf)
end

convert(::Type{<:Str{ASCIICSE}}, str::Str{<:SubSet_CSEs}) = unierror(UTF_ERR_INVALID_ASCII)

convert(::Type{<:Str{ASCIICSE}}, str::Str{<:Union{Text1CSE,BinaryCSE,LatinCSE,UTF8CSE}}) =
    (is_empty(str) ? empty_ascii
     : is_ascii(str) ? Str(ASCIICSE, str.data) : unierror(UTF_ERR_INVALID_ASCII))

function convert(::Type{<:Str{ASCIICSE}}, dat::Vector{UInt8})
    is_empty(dat) && return empty_ascii
    is_ascii(dat) || unierror(UTF_ERR_INVALID_ASCII)
    siz = sizeof(dat)
    buf = _allocate(siz)
    @preserve dat unsafe_copyto!(pointer(buf), pointer(dat), siz)
    Str(ASCIICSE, buf)
end

function convert(::Type{<:Str{ASCIICSE}}, str::SubString{T}) where
    {T<:Union{String, Str{<:Union{Binary_CSEs,LatinCSE,UTF8CSE}}}}
    is_empty(str) && return empty_ascii
    is_ascii(str) || unierror(UTF_ERR_INVALID_ASCII)
    @preserve str begin
        len = ncodeunits(str)
        buf, out = _allocate(UInt8, len)
        unsafe_copyto!(out, pointer(str), len)
        Str(ASCIICSE, buf)
    end
end

convert(::Type{<:Str{ASCIICSE}}, str::String) =
    (isempty(str) ? empty_ascii
     : is_ascii(str) ? Str(ASCIICSE, str) : unierror(UTF_ERR_INVALID_ASCII))


# TODO: allow transform as the first argument to replace?

ascii(str::MaybeSub{<:Str}) = is_ascii(str) ? convert(ASCIIStr, str).data : ascii_err()
ascii(str::MaybeSub{<:Str{<:SubSet_CSEs}}) = ascii_err()
ascii(str::Str{T}) where {T<:ASCIICSE} = str.data

# This should really use a keyword argument, and allow for the following possibilities:
# 1) use default substitution character \u{1a} for ASCII/Latin1 (SUB) or \u{fffd} for Unicode
# 2) use a different substitution character
# 3) use a substition string
# 4) use a substition function, which gets passed the location of the start of the invalid sequence
#    and returns a replacement string, and the new location after the invalid sequence.
#    Note: for that to work, would need to use the same function in the check_string function
# 5) throw an exception, with enough information that one can determine where in the input
#    the invalid character was, and what it was

# Todo: optimize this!
function _convert_ascii(a, invlen, invdat)
    len = length(a)
    cnt = 0
    @inbounds for i = 1:len ; cnt += (a[i] >= 0x80) ; end
    # Note: this doesn't make a copy if only ascii characters,
    # so that changes later to the vector will affect the string!
    # that is only safe if the vector came from something else immutable
    cnt == 0 && return Str(ASCIICSE, a)
    v = _allocate(len + cnt*invlen)
    out = 1
    @inbounds for i = 1:len
        if (ch = a[i]) <= 0x7f
            v[out] = ch
            out += 1
        else
            unsafe_copyto!(v, out, invdat, 1, invlen)
            out += invlen
        end
    end
    Str(ASCIICSE, v)
end

convert(::Type{<:Str{ASCIICSE}}, a::Vector{UInt8}, invalids_as::String) =
    _convert_ascii(a, sizeof(invalids_as), ascii(invalids_as))

convert(::Type{<:Str{ASCIICSE}}, a::Vector{UInt8}, invalids_as::Str{ASCIICSE}) =
    _convert_ascii(a, sizeof(invalids_as), invalids_as.data)

convert(::Type{<:Str{ASCIICSE}}, a::Vector{UInt8}, invalids_as::AbstractString) =
    convert(ASCIIStr, a, ascii(invalids_as))
