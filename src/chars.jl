#=
CodePoint types

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
In small part based on code for Char in Julia
=#
export CodePoint

abstract type CodePoint <: AbstractChar end

primitive type _ASCII <: CodePoint  8 end
primitive type _Latin <: CodePoint  8 end
primitive type _UCS2  <: CodePoint 16 end
primitive type _UTF32 <: CodePoint 32 end

basetype(::Type{_ASCII}) = UInt8
basetype(::Type{_Latin}) = UInt8
basetype(::Type{_UCS2})  = UInt16
basetype(::Type{_UTF32}) = UInt32

tobase(v::T) where {T<:CodePoint} = reinterpret(basetype(T), v)

const ByteChars = Union{_ASCII, _Latin}
const WideChars = Union{_UCS2, _UTF32}

typemin(::Type{T}) where {T<:CodePoint} = reinterpret(T, typemin(basetype(T)))
typemin(::Type{T}) where {T<:Union{_Latin,_UCS2}} = reinterpret(T, typemax(basetype(T)))
typemax(::Type{_ASCII}) = reinterpret(_ASCII, 0x7f)
typemax(::Type{_UTF32}) = reinterpret(_UTF32, 0x10ffff)

isvalid(::Type{T}, v::Signed) where {T<:ByteChars} = typemin(T) <= v <= typemax(T)
isvalid(::Type{T}, v::Unsigned) where {T<:ByteChars} = v <= typemax(T)
isvalid(::Type{T}, v::Signed) where {T<:WideChars} = v >= 0 && isvalid(T, Unsigned(v))
isvalid(::Type{T}, v::Unsigned) where {T<:WideChars} =
    (v <= typemax(T)) & !is_surrogate_codeunit(v)

convert(::Type{T}, v::S) where {T<:Integer, S<:CodePoint} = convert(T, tobase(v))
convert(::Type{T}, v::Signed) where {T<:CodePoint} =
    isvalid(T, v) ? convert(T, Unsigned(v)) : error("Invalid CodePoint:$T $v")
convert(::Type{T}, v::Unsigned) where {T<:CodePoint} =
    isvalid(T, v) ? reinterpret(T, basetype(T)(v)) : error("Invalid CodePoint:$T $v")

rem(x::S, ::Type{T}) where {S<:CodePoint, T<:Number} = rem(basetype(S)(x), T)

size(cp::CodePoint) = ()
size(cp::CodePoint, dim) = convert(Int, dim) < 1 ? throw(BoundsError()) : 1
ndims(cp::CodePoint) = 0
ndims(::Type{<:CodePoint}) = 0
length(cp::CodePoint) = 1
endof(cp::CodePoint) = 1
getindex(cp::CodePoint) = cp
getindex(cp::CodePoint, i::Integer) = i == 1 ? cp : throw(BoundsError(i))
getindex(cp::CodePoint, I::Integer...) = all(x -> x == 1, I) ? cp : throw(BoundsError())
first(cp::CodePoint) = cp
last(cp::CodePoint) = cp
eltype(::Type{CodePoint}) = CodePoint

start(cp::CodePoint) = false
next(cp::CodePoint, state) = (cp, true)
done(cp::CodePoint, state) = state
isempty(cp::CodePoint) = false
in(x::CodePoint, y::CodePoint) = x == y

==(x::CodePoint, y::CodePoint) = tobase(x) == tobase(y)
==(x::CodePoint, y::Char) = Char(tobase(x)) == y
==(x::Char, y::CodePoint) = y == x

isless(x::CodePoint, y::CodePoint) = tobase(x) < tobase(y)
isless(x::CodePoint, y::Char) = Char(tobase(x)) < y
isless(x::Char, y::CodePoint) = x < Char(tobase(y))

hash(x::CodePoint, h::UInt) =
    hash_uint64(((UInt32(x) + 0x0d4d64234) << 32) ⊻ UInt64(h))

-(x::CodePoint, y::CodePoint) = Int(x) - Int(y)
-(x::CodePoint, y::Integer) = CodePoint(Int32(x) - Int32(y))
+(x::CodePoint, y::Integer) = CodePoint(Int32(x) + Int32(y))
+(x::Integer, y::CodePoint) = y + x
