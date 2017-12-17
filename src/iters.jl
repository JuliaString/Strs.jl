# CodeUnits -- return the code units of a string

struct CodeUnits{S}
    xs::S
end

"""
    codeunits(str)

An iterator that generates the code units of a string

# Examples
```jldoctest
julia> a = 1:2:11
1:2:11

julia> collect(a)
6-element Array{Int64,1}:
  1
  3
  5
  7
  9
 11

julia> collect(codeunits(str))
3-element Array{Int64,1}:
 1
 3
 5
```
"""
codeunits(xs) = CodeUnits(xs)
eltype(::Type{CodeUnits{S}}) where {S} = codeunit_type(S)
length(it::CodeUnits) = _len(it.xs)
start(it::CodeUnits) = start(it.xs)
done(it::CodeUnits, state) = done(it.xs, state)

@propagate_inbounds next(it::CodeUnits, state) = (get_codeunit(_pnt(it.xs), state), state+1)


# CodePoints -- return the code points of a string

struct CodePoints{S}
    xs::S
end

"""
    codepoints(str)

An iterator that generates the code points of a string

# Examples
```jldoctest
julia> a = Str("abc\U1f596")

julia> collect(a)
4-element Array{Int32,1}:
  1
  3
  5
  7
  9
 11

julia> collect(codepoints(a))
4-element Array{Int64,1}:
 1
 3
 5
```
"""
codepoints(xs) = CodePoints(xs)
eltype(::Type{CodePoints{S}}) where {S} = eltype(S)
length(it::CodePoints) = length(it.xs)
start(it::CodePoints) = start(it.xs)
done(it::CodePoints, state) = done(it.xs, state)
@propagate_inbounds next(it::CodePoints{T}, state) where {T<:Str} =
    _next(CodePointStyle(T), codepoint_type(T), it.xs, state)
