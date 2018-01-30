#=
CodeUnit support

The code in this file is a part of Julia, from base/strings/basic.jl
License is MIT: http://julialang.org/license
It is used for CodeUnit support in pre v0.7 versions of Julia
=#

## code unit access ##

codeunit(str::AbstractString) = UInt8
ncodeunits(str::AbstractString) = sizeof(str)

"""
    CodeUnits(s::AbstractString)

Wrap a string (without copying) in an immutable vector-like object that accesses the code units
of the string's representation.
"""
struct CodeUnits{T,S<:AbstractString} <: DenseVector{T}
    s::S
    CodeUnits(s::S) where {S<:AbstractString} = new{codeunit(s),S}(s)
end

length(s::CodeUnits) = ncodeunits(s.s)
sizeof(s::CodeUnits{T}) where {T} = ncodeunits(s.s) * sizeof(T)
size(s::CodeUnits) = (length(s),)
strides(s::CodeUnits) = (1,)
@propagate_inbounds getindex(s::CodeUnits, i::Int) = codeunit(s.s, i)
IndexStyle(::Type{<:CodeUnits}) = IndexLinear()
start(s::CodeUnits) = 1
next(s::CodeUnits, i) = (@_propagate_inbounds_meta; (s[i], i+1))
done(s::CodeUnits, i) = (@_inline_meta; i == length(s)+1)

write(io::IO, s::CodeUnits) = write(io, s.s)

unsafe_convert(::Type{Ptr{T}},    s::CodeUnits{T}) where {T} = unsafe_convert(Ptr{T}, s.s)
unsafe_convert(::Type{Ptr{Int8}}, s::CodeUnits{UInt8}) = unsafe_convert(Ptr{Int8}, s.s)

"""
    codeunits(s::AbstractString)

Obtain a vector-like object containing the code units of a string.
Returns a `CodeUnits` wrapper by default, but `codeunits` may optionally be defined
for new string types if necessary.
"""
codeunits(s::AbstractString) = CodeUnits(s)
