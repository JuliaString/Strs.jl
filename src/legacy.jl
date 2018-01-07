# Convenience functions that used to be in Julia, moved to LegacyStrings.jl
# Copyright 2017 Scott P. Jones and other contributors to the Julia language
# Licensed under MIT License, see LICENSE.md

ascii(str) = convert(ASCIIStr, str)

ascii(pnt::Ptr{UInt8}) =
    ascii(pnt, pnt == C_NULL ? Csize_t(0) : ccall(:strlen, Csize_t, (Ptr{UInt8},), pnt))
ascii(pnt::Ptr{UInt8}, len::Integer) = begin
    pnt == C_NULL && nullerr()
    vec = ccall(:jl_pchar_to_array, Vector{UInt8}, (Ptr{UInt8}, Csize_t), pnt, len)
    isvalid(ASCIIStr, vec) || unierror(UTF_ERR_INVALID_ASCII)
    ASCIIStr(vec)
end

latin1(x) = convert(_LatinStr, x)
latin1(p::Ptr{UInt8}) =
    latin1(p, p == C_NULL ? Csize_t(0) : ccall(:strlen, Csize_t, (Ptr{UInt8},), p))
function latin1(p::Ptr{UInt8}, len::Integer)
    p == C_NULL && nulerr()
    Str(_LatinCSE, ccall(:jl_pchar_to_array, Vector{UInt8}, (Ptr{UInt8}, Csize_t), p, len))
end

utf8(x) = convert(UTF8Str, x)
function utf8(p::Ptr{UInt8}, len::Integer)
    p == C_NULL && nullerr()
    Str(UTF8CSE, ccall(:jl_pchar_to_array, Vector{UInt8}, (Ptr{UInt8}, Csize_t), p, len))
end
utf8(p::Ptr{UInt8}) =
    utf8(p, p == C_NULL ? Csize_t(0) : ccall(:strlen, Csize_t, (Ptr{UInt8},), p))

"""
    utf8(s)

Create a UTF-8 string from a byte array, array of `UInt8`, or any other string type. (Data
must be valid UTF-8. Conversions of byte arrays check for a byte-order marker in the first
two bytes, and do not include it in the resulting string.)
"""
utf8(s)

"""
    utf8(::Union{Ptr{UInt8}, Ptr{Int8}} [, length])

Create a string from the address of a NUL-terminated UTF-8 string. A copy is made; the
pointer can be safely freed. If `length` is specified, the string does not have to be
NUL-terminated.
"""
utf8(::Union{Ptr{UInt16}, Ptr{Int16}}, length=length)

utf16(x) = convert(UTF16Str, x)
utf16(p::Ptr{Int16}) = utf16(reinterpret(Ptr{UInt16}, p))
utf16(p::Ptr{Int16}, len::Integer) = utf16(reinterpret(Ptr{UInt16}, p), len)

function utf16(pnt::Ptr{UInt16})
    len = 0
    while unsafe_load(pnt, len + 1) != 0
        len += 1
    end
    utf16(pnt, len)
end

"""
    utf16(s)

Create a UTF-16 string from a byte array, array of `UInt16`, or any other string type. (Data
must be valid UTF-16. Conversions of byte arrays check for a byte-order marker in the first
two bytes, and do not include it in the resulting string.)
"""
utf16(s)

"""
    utf16(::Union{Ptr{UInt16}, Ptr{Int16}} [, length])

Create a string from the address of a NUL-terminated UTF-16 string. A copy is made; the
pointer can be safely freed. If `length` is specified, the string does not have to be
NUL-terminated.
"""
utf16(::Union{Ptr{UInt16}, Ptr{Int16}}, length=length)

utf32(x) = convert(UTF32Str, x)

# These are broken by #24999

#utf32(p::Union{Ptr{Char}, Ptr{Int32}}, len::Integer) = utf32(reinterpret(Ptr{UInt32}, p), len)
#utf32(p::Union{Ptr{Char}, Ptr{Int32}}) = utf32(reinterpret(Ptr{UInt32}, p))

function utf32(pnt::Ptr{UInt32})
    len = 0
    while (ch = unsafe_load(pnt, len += 1)) != 0
        check_valid(ch, len)
    end
    buf, out = _allocate(UInt32, len)
    unsafe_copyto!(out, 1, pnt, 1, len)
    Str(UTF32CSE, buf)
end

"""
    utf32(s)

Create a UTF-32 string from a byte array, array of `Char` or `UInt32`, or any other string
type. (Conversions of byte arrays check for a byte-order marker in the first four bytes, and
do not include it in the resulting string.)
"""
utf32(s)

"""
    utf32(::Union{Ptr{Char}, Ptr{UInt32}, Ptr{Int32}} [, length])

Create a string from the address of a NUL-terminated UTF-32 string. A copy is made; the
pointer can be safely freed. If `length` is specified, the string does not have to be
NUL-terminated.
"""
utf32(::Union{Ptr{Char}, Ptr{UInt32}, Ptr{Int32}}, length=length)
