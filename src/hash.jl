#=
CRC and Hashing functions for Str types (to make compatible with String hashes)

Copyright 2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

# Use crc32c to make CRC32c of UTF8 view of string, for use with hashing
# where different string types are supposed to compare as ==

export utf8crc

@static if V6_COMPAT
    unsafe_crc32c(a, n, crc) = ccall(:jl_crc32c, UInt32, (UInt32, Ptr{UInt8}, Csize_t), crc, a, n)
else
    import Base: unsafe_crc32c
end

const utf8crc = @static V6_COMPAT ? Base.crc32c : Base._crc32c

function utf8crc(str::Union{S,SubString{S}}, seed::UInt) where {S<:Str}
    @preserve str begin
        len, pnt = _lenpnt(str)
        len == 0 && return utf8crc(str, seed)
        len, flags, num4byte, num3byte, num2byte, latin1 = count_chars(S, pnt, len)
        # could be UCS2, _UCS2, UTF32, _UTF32, Text2, Text4
        buf = (flags == 0
               ? _cvtsize(UInt8, pnt, len)
               : _encode_utf8(pnt, len += latin1 + num2byte + num3byte*2 + num4byte*3))
        utf8crc(buf, seed)
    end
end

function utf8crc(str::Union{S,SubString{S}},
                 seed::UInt) where {S<:Str{<:Union{Text1CSE,Latin_CSEs}}}
    @preserve str begin
        len, pnt = _lenpnt(str)
        len != 0 && (cnt = count_latin(len, pnt)) != 0 && (str = _latin_to_utf8(pnt, len + cnt))
        utf8crc(str, seed)
    end
end

function utf8crc(str::Union{S,SubString{S}}, seed::UInt) where {S<:Str{UTF16CSE}}
    @preserve str begin
        len, pnt = _lenpnt(str)
        len == 0 && return utf8crc(str, seed)
        len, flags, num4byte, num3byte, num2byte, latin1 = count_chars(S, pnt, len)
        buf = (flags == 0
               ? _cvtsize(UInt8, pnt, len)
               : _cvt_16_to_utf8(S, pnt, len += latin1 + num2byte + num3byte*2 + num4byte*3))
        utf8crc(buf, seed)
    end
end

utf8crc(str::Union{S,SubString{S}},
    seed::UInt32=0%UInt32) where {S<:Str{<:Union{ASCIICSE,UTF8CSE,BinaryCSE}}} =
        unsafe_crc32c(pointer(str), sizeof(s) % Csize_t, seed)
    

# Support for higher performance hashing, while still compatible with hashed UTF8 String

_memhash(siz, ptr, seed) =
    ccall(Base.memhash, UInt, (Ptr{UInt8}, Csize_t, UInt32), ptr, siz, seed % UInt32)

_hash(siz, ptr, seed) = _memhash(siz, ptr, seed) + seed

# Optimize conversion to ASCII or UTF8 to calculate compatible hash value
                          
function hash(str::Union{S,SubString{S}}, seed::UInt) where {S<:Str}
    @preserve str begin
        len, pnt = _lenpnt(str)
        len == 0 && return _hash(len, pnt, seed + Base.memhash_seed)
        len, flags, num4byte, num3byte, num2byte, latin1 = count_chars(S, pnt, len)
        # could be UCS2, _UCS2, UTF32, _UTF32, Text2, Text4
        buf = (flags == 0
               ? _cvtsize(UInt8, pnt, len)
               : _encode_utf8(pnt, len += latin1 + num2byte + num3byte*2 + num4byte*3))
        _hash(len, buf, seed + Base.memhash_seed)
    end
end

function hash(str::Union{S,SubString{S}}, seed::UInt) where {S<:Str{<:Union{Text1CSE,Latin_CSEs}}}
    @preserve str begin
        len, pnt = _lenpnt(str)
        len != 0 && (cnt = count_latin(len, pnt)) != 0 &&
            (str = _latin_to_utf8(pnt, len += cnt) ; pnt = _pnt(str))
        _hash(len, pnt, seed + Base.memhash_seed)
    end
end

function hash(str::Union{S,SubString{S}}, seed::UInt) where {S<:Str{UTF16CSE}}
    @preserve str begin
        len, pnt = _lenpnt(str)
        len == 0 && return _hash(len, pnt, seed + Base.memhash_seed)
        len, flags, num4byte, num3byte, num2byte, latin1 = count_chars(S, pnt, len)
        buf = (flags == 0
               ? _cvtsize(UInt8, pnt, len)
               : _cvt_16_to_utf8(S, pnt, len += latin1 + num2byte + num3byte*2 + num4byte*3))
        _hash(len, buf, seed + Base.memhash_seed)
    end
end

# Directly calculate hash for "compatible" types

hash(str::Union{S,SubString{S}}, seed::UInt) where {S<:Str{<:Union{ASCIICSE,UTF8CSE,BinaryCSE}}} =
    @preserve str _hash(sizeof(str), pointer(str), seed + Base.memhash_seed)


