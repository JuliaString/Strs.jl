# Handle some name changes between v0.6 and master   
const copyto! = copy!
const unsafe_copyto! = unsafe_copy!
const Nothing = Void
const Cvoid = Void
abstract type AbstractChar end
export AbstractChar

@noinline index_error(s::AbstractString, i::Integer) =
    throw(UnicodeError(UTF_ERR_INVALID_INDEX, Int(i), codeunit(s, i)))

function thisind(str::String, pos::Integer)
    @boundscheck 0 < pos <= _len(str) || boundserr(str, pos)
    pnt = _pnt(str) + pos - 1
    pos - (checkcont(pnt) ? (checkcont(pnt - 1) ? (checkcont(pnt - 2) ? 3 : 2) : 1) : 0)
end

macro preserve(args...)
    syms = args[1:end-1]
    for x in syms
        isa(x, Symbol) || error("Preserved variable must be a symbol")
    end
    #=
    s, r = gensym(), gensym()
    esc(quote
        $s = $(Expr(:gc_preserve_begin, syms...))
        $r = $(args[end])
        $(Expr(:gc_preserve_end, s))
        $r
        $(args[end])
    end)
    =#
    esc(quote ; $(args[end]) ; end)
end

Base.SubString(str::AbstractString, rng::UnitRange) = SubString(str, first(rng), last(rng))

Base.checkbounds(::Type{Bool}, s::AbstractString, i::Integer) = 1 <= i <= ncodeunits(s)
