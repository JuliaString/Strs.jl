# Copyright 2017 Gandalf Software, Inc., Scott P. Jones
# Licensed under MIT License, see LICENSE.md

# Todo, rewrite this as a parameterized type, with basetype, swapped, aligned
# Also, add UInt64 and UInt128

abstract type AccessType end
primitive type UInt16_U  <: AccessType 16 end # Unaligned 16-bit word
primitive type UInt32_U  <: AccessType 32 end # Unaligned 32-bit word
primitive type UInt16_S  <: AccessType 16 end # Swapped 16-bit word
primitive type UInt32_S  <: AccessType 32 end # Swapped 32-bit word
primitive type UInt16_US <: AccessType 16 end # Unaligned & swapped 16-bit word
primitive type UInt32_US <: AccessType 32 end # Unaligned & swapped 32-bit word

basetype(::Type{UInt16})    = UInt16
basetype(::Type{UInt16_U})  = UInt16
basetype(::Type{UInt16_S})  = UInt16
basetype(::Type{UInt16_US}) = UInt16

basetype(::Type{UInt32})    = UInt32
basetype(::Type{UInt32_U})  = UInt32
basetype(::Type{UInt32_S})  = UInt32
basetype(::Type{UInt32_US}) = UInt32

# Provide the correct type to access as swapped
swappedtype(::Type{UInt16})    = UInt16_S
swappedtype(::Type{UInt16_U})  = UInt16_US
swappedtype(::Type{UInt16_S})  = UInt16
swappedtype(::Type{UInt16_US}) = UInt16_U

swappedtype(::Type{UInt32})    = UInt32_S
swappedtype(::Type{UInt32_U})  = UInt32_US
swappedtype(::Type{UInt32_S})  = UInt32
swappedtype(::Type{UInt32_US}) = UInt32_U

# Provide the correct type to access if aligned
alignedtype(::Type{UInt16})    = UInt16
alignedtype(::Type{UInt16_U})  = UInt16
alignedtype(::Type{UInt16_S})  = UInt16_S
alignedtype(::Type{UInt16_US}) = UInt16_S

alignedtype(::Type{UInt32})    = UInt32
alignedtype(::Type{UInt32_U})  = UInt32
alignedtype(::Type{UInt32_S})  = UInt32_S
alignedtype(::Type{UInt32_US}) = UInt32_S

@inline _ul(pnt::Ptr{T}, shift) where {T} =
    unsafe_load(reinterpret(Ptr{UInt8}, pnt))%basetype(T) << shift

@inline _load(pnt::Ptr{T}) where {T<:Union{UInt16_US,UInt32_US}} =
    bswap(unsafe_load(reinterpret(Ptr{basetype(T)}, pnt)))

@inline _load(pnt::Ptr{UInt16_U})  = _ul(pnt, 0) | _ul(pnt+1, 8)
@inline _load(pnt::Ptr{UInt16_US}) = _ul(pnt, 8) | _ul(pnt+1, 0)
@inline _load(pnt::Ptr{UInt32_U})  = _ul(pnt, 0) | _ul(pnt+1, 8) | _ul(pnt+2, 16) | _ul(pnt+3, 24)
@inline _load(pnt::Ptr{UInt32_US}) = _ul(pnt, 24) | _ul(pnt+1, 16) | _ul(pnt+2, 8) | _ul(pnt+3, 0)

@inline unsafe_load(pnt::Ptr{T}) where {T<:AccessType} = _load(pnt)
@inline unsafe_load(pnt::Ptr{T}, i) where {T<:AccessType} = _load(pnt + (i-1)*sizeof(basetype(T)))
