# This file contains code that was a part of Julia (in operators.jl)
# License is MIT: https://julialang.org/license
# It is used to support the new string searching syntax on v0.6.2

"""
    Fix2(f, x)

A type representing a partially-applied version of function `f`, with the second
argument fixed to the value "x".
In other words, `Fix2(f, x)` behaves similarly to `y->f(y, x)`.
"""
struct Fix2{F,T} <: Function
    f::F
    x::T

    Fix2(f::F, x::T) where {F,T} = new{F,T}(f, x)
    Fix2(f::Type{F}, x::T) where {F,T} = new{Type{F},T}(f, x)
end

(f::Fix2)(y) = f.f(y, f.x)

"""
    isequal(x)

Create a function that compares its argument to `x` using [`isequal`](@ref), i.e.
a function equivalent to `y -> isequal(y, x)`.

The returned function is of type `Base.Fix2{typeof(isequal)}`, which can be
used to implement specialized methods.
"""
isequal(x) = Fix2(isequal, x)

const EqualTo = Fix2{typeof(isequal)}

"""
    ==(x)

Create a function that compares its argument to `x` using [`==`](@ref), i.e.
a function equivalent to `y -> y == x`.

The returned function is of type `Base.Fix2{typeof(==)}`, which can be
used to implement specialized methods.
"""
==(x) = Fix2(==, x)

"""
    in(x)

Create a function that checks whether its argument is [`in`](@ref) `x`, i.e.
a function equivalent to `y -> y in x`.

The returned function is of type `Base.Fix2{typeof(in)}`, which can be
used to implement specialized methods.
"""
in(x) = Fix2(in, x)

const OccursIn = Fix2{typeof(in)}

