#=
CodePoint iterator support

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#
# CodePoints -- return the code points of a string

struct CodePoints{T}
    xs::T
end

"""
    codepoints(str)

An iterator that generates the code points of a string

# Examples
```jldoctest
julia> a = Str("abc\U1f596")

julia> collect(a)

julia> collect(codepoints(a))
```
"""
codepoints(xs) = CodePoints(xs)
eltype(::Type{<:CodePoints{S}}) where {S} = eltype(S)
length(it::CodePoints) = length(it.xs)
start(it::CodePoints) = start(it.xs)
done(it::CodePoints, state) = done(it.xs, state)
@propagate_inbounds next(it::CodePoints{T}, state) where {T<:Str} =
    _next(CodePointStyle(T), eltype(T), it.xs, state)
