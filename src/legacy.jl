# Convenience functions that used to be in Julia, moved to LegacyStrings.jl
# Copyright 2017 Scott P. Jones and other contributors to the Julia language
# Licensed under MIT License, see LICENSE.md

# Renamed from ascii to to_ascii, to prevent issues with Base.ascii
to_ascii(str) = convert(ASCIIStr, str)
to_ascii(pnt::Ptr{UInt8}) = convert(ASCIIStr, unsafe_string(p))
to_ascii(pnt::Ptr{UInt8}, len::Integer) = convert(ASCIIStr, unsafe_string(p, len))

"""
    utf8(s)

Create a UTF-8 string from a byte array, array of `UInt8`, or any other string type. (Data
must be valid UTF-8. Conversions of byte arrays check for a byte-order marker in the first
two bytes, and do not include it in the resulting string.)
"""
utf8(x) = convert(UTF8Str, x)

"""
    utf8(::Union{Ptr{UInt8}, Ptr{Int8}} [, length])

Create a string from the address of a NUL-terminated UTF-8 string. A copy is made; the
pointer can be safely freed. If `length` is specified, the string does not have to be
NUL-terminated.
"""
utf8(p::Ptr{UInt8}) = utf8(unsafe_string(p))
utf8(p::Ptr{UInt8}, len::Integer) = utf8(unsafe_string(p, len))

"""
    utf16(s)

Create a UTF-16 string from a byte array, array of `UInt16`, or any other string type. (Data
must be valid UTF-16. Conversions of byte arrays check for a byte-order marker in the first
two bytes, and do not include it in the resulting string.)
"""
utf16(x) = convert(UTF16Str, x)

"""
    utf16(::Union{Ptr{UInt16}, Ptr{Int16}} [, length])

Create a string from the address of a NUL-terminated UTF-16 string. A copy is made; the
pointer can be safely freed. If `length` is specified, the string does not have to be
NUL-terminated.
"""
function utf16(pnt::Ptr{UInt16}, len::Unsigned)
    buf, out = _allocate(UInt16, len)
    _memcpy(out, pnt, len)
    Str(UTF16CSE, buf)
end
function utf16(pnt::Ptr{UInt16})
    len = 0
    while (ch = unsafe_load(pnt, len += 1)) != 0
        check_valid(ch, len)
    end
    utf16(pnt, len%UInt)
end
utf16(pnt, len::Signed) = len < 0 ? error("negative length") : utf16(pnt, len%Unsigned)

"""
    utf32(s)

Create a UTF-32 string from a byte array, array of `Char` or `UInt32`, or any other string
type. (Conversions of byte arrays check for a byte-order marker in the first four bytes, and
do not include it in the resulting string.)
"""
utf32(x) = convert(UTF32Str, x)

# These are broken by #24999

#utf32(p::Union{Ptr{Char}, Ptr{Int32}}, len::Integer) = utf32(reinterpret(Ptr{UInt32}, p), len)
#utf32(p::Union{Ptr{Char}, Ptr{Int32}}) = utf32(reinterpret(Ptr{UInt32}, p))

"""
    utf32(::Ptr{UInt32}, [, length])

Create a string from the address of a NUL-terminated UTF-32 string. A copy is made; the
pointer can be safely freed. If `length` is specified, the string does not have to be
NUL-terminated.
"""
function utf32(pnt::Ptr{UInt32}, len::Unsigned)
    buf, out = _allocate(UInt32, len)
    _memcpy(out, pnt, len)
    Str(UTF32CSE, buf)
end
function utf32(pnt::Ptr{UInt32})
    len = 0
    while (ch = unsafe_load(pnt, len += 1)) != 0
        check_valid(ch, len)
    end
    utf32(pnt, len%UInt)
end
utf32(pnt, len::Signed) = len < 0 ? error("negative length") : utf32(pnt, len%Unsigned)
