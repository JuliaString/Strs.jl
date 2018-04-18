#=
Regex functions for Str strings

Copyright 2018 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on julia/base/regex.jl
=#

using Base.PCRE

using Base: DEFAULT_COMPILER_OPTS, DEFAULT_MATCH_OPTS
import Base: Regex, match, compile, eachmatch

export StrRegex, StrRegexMatch, StrRegex

const Regex_CSEs = Union{ASCIICSE,Latin_CSEs,Binary_CSEs}

# Default is to act as if validated UTF8
cvt_compile(::Type{<:CSE}, co) = UInt32(co) | PCRE.NO_UTF_CHECK | PCRE.UTF
cvt_match(::Type{<:CSE}, co)   = UInt32(co) | PCRE.NO_UTF_CHECK

cvt_compile(::Type{Regex_CSEs}, co) = UInt32(co) & ~PCRE.UTF

cvt_compile(::Type{<:Str{C}}, co) where {C<:CSE} = cvt_compile(C, co)
cvt_match(::Type{<:Str{C}}, co) where {C<:CSE}   = cvt_match(C, co)

# String type is not validated
cvt_compile(::Type{String}, co) = (UInt32(co) & ~PCRE.NO_UTF_CHECK) | PCRE.UTF
cvt_match(::Type{String}, co)   = (UInt32(co) & ~PCRE.NO_UTF_CHECK)

@noinline compile_opts(::Type{C}, options) where {C} =
    (options & ~PCRE.COMPILE_MASK) == 0 ? cvt_compile(C, options) :
    throw(ArgumentError("invalid regex compile options: $options"))
@noinline match_opts(::Type{C}, options) where {C} =
    (options & ~PCRE.EXECUTE_MASK) == 0 ? cvt_match(C, options) :
    throw(ArgumentError("invalid regex match options: $options"))

mutable struct StrRegex{T<:AbstractString}
    pattern::T
    compile_options::UInt32
    match_options::UInt32
    regex::Ptr{Cvoid}
    extra::Ptr{Cvoid}
    ovec::Vector{Csize_t}
    match_data::Ptr{Cvoid}

    function StrRegex(pattern::T,
                      compile_options::Integer, match_options::Integer) where {T<:AbstractString}
        re = compile(new{T}(pattern,
                            compile_opts(T, compile_options), match_opts(T, match_options),
                            C_NULL, C_NULL, Csize_t[], C_NULL))
        finalizer(re) do re
            re.regex == C_NULL || PCRE.free_re(re.regex)
            re.match_data == C_NULL || PCRE.free_match_data(re.match_data)
        end
        re
    end
end

const RegexTypes = Union{Regex, StrRegex}

function _update_compiler_opts(flags)
    options = DEFAULT_COMPILER_OPTS
    for f in flags
        options |= f=='i' ? PCRE.CASELESS  :
                   f=='m' ? PCRE.MULTILINE :
                   f=='s' ? PCRE.DOTALL    :
                   f=='x' ? PCRE.EXTENDED  :
                   throw(ArgumentError("unknown regex flag: $f"))
    end
    options
end

StrRegex(pattern::AbstractString) =
    StrRegex(pattern, DEFAULT_COMPILER_OPTS, DEFAULT_MATCH_OPTS)
StrRegex(pattern::AbstractString, flags::AbstractString) =
    StrRegex(pattern, _update_compile_opts(flags), DEFAULT_MATCH_OPTS)

Regex(pattern::MaybeSub{<:Str}, co, mo) = StrRegex(pattern, co, mo)
Regex(pattern::MaybeSub{<:Str}, flags::AbstractString) = StrRegex(pattern, flags)
Regex(pattern::MaybeSub{<:Str}) = StrRegex(pattern)

function compile(regex::StrRegex)
    if regex.regex == C_NULL
        regex.regex = PCRE.compile(regex.pattern, regex.compile_options)
        PCRE.jit_compile(regex.regex)
        regex.match_data = PCRE.create_match_data(regex.regex)
        regex.ovec = PCRE.get_ovec(regex.match_data)
    end
    regex
end

export @R_str

macro R_str(pattern, flags...) StrRegex(pattern, flags...) end

function show(io::IO, re::StrRegex{T}) where {T}
    imsx = PCRE.CASELESS|PCRE.MULTILINE|PCRE.DOTALL|PCRE.EXTENDED
    opts = re.compile_options
    if (opts & ~imsx) == DEFAULT_COMPILER_OPTS
        print(io, 'R')
        Base.print_quoted_literal(io, re.pattern)
        if (opts & PCRE.CASELESS ) != 0; print(io, 'i'); end
        if (opts & PCRE.MULTILINE) != 0; print(io, 'm'); end
        if (opts & PCRE.DOTALL   ) != 0; print(io, 's'); end
        if (opts & PCRE.EXTENDED ) != 0; print(io, 'x'); end
    else
        print(io, "StrRegex{$T}(")
        show(io, re.pattern)
        print(io, ',')
        show(io, opts)
        print(io, ')')
    end
end

struct StrRegexMatch{T<:AbstractString,M<:Union{Regex,StrRegex{T}}}
    match::SubString{T}
    captures::Vector{Union{Nothing,SubString{T}}}
    offset::Int
    offsets::Vector{Int}
    regex::M
end

function show(io::IO, m::StrRegexMatch{T}) where {T}
    print(io, "StrRegexMatch{$T}(")
    show(io, m.match)
    idx_to_capture_name = PCRE.capture_names(m.regex.regex)
    if !is_empty(m.captures)
        print(io, ", ")
        for i = 1:length(m.captures)
            # If the capture group is named, show the name.
            # Otherwise show its index.
            print(io, get(idx_to_capture_name, i, i), "=")
            show(io, m.captures[i])
            i < length(m.captures) && print(io, ", ")
        end
    end
    print(io, ")")
end

# Capture group extraction
getindex(m::StrRegexMatch, idx::Integer) = m.captures[idx]
function getindex(m::StrRegexMatch, name::Symbol)
    idx = PCRE.substring_number_from_name(m.regex.regex, name)
    idx <= 0 && error("no capture group named $name found in regex")
    m[idx]
end

getindex(m::StrRegexMatch, name::AbstractString) = m[Symbol(name)]

"""
Check if the compile flags match the current search

If not, free up old compilation, and recompile
For better performance, the StrRegex object should hold spots for 5 compiled regexes,
for 8-bit, UTF-8, 16-bit, UTF-16, and 32-bit code units
"""
function check_compile(::Type{C}, regex::RegexTypes) where {C<:Union{CSE,String}}
    re = regex.regex
    # ASCII is compatible with all (current) types, don't recompile
    C == ASCIICSE && re != C_NULL && return
    oldopt = regex.compile_options
    cvtcomp = cvt_compile(C, oldopt)
    if cvtcomp != oldopt
        regex.compile_options = cvtcomp
        re == C_NULL || (PCRE.free_re(re); regex.regex = re = C_NULL)
        regex.match_data == C_NULL ||
            (PCRE.free_match_data(regex.match_data); regex.match_data = C_NULL)
    end
    if re == C_NULL
        regex.regex = re = PCRE.compile(regex.pattern, cvtcomp)
        PCRE.jit_compile(re)
        regex.match_data = PCRE.create_match_data(re)
        regex.ovec = PCRE.get_ovec(regex.match_data)
    end
    nothing
end

function _match(::Type{C}, re, str, idx, add_opts) where {C<:CSE}
    check_compile(C, re)
    PCRE.exec(re.regex, str, idx-1, cvt_match(C, re.match_options | add_opts), re.match_data) ||
        return nothing
    ovec = re.ovec
    n = div(length(ovec),2) - 1
    mat = SubString(str, ovec[1]+1, prevind(str, ovec[2]+1))
    cap = Union{Nothing,SubString{typeof(str)}}[ovec[2i+1] == PCRE.UNSET ?
                                               nothing :
                                               SubString(str, ovec[2i+1]+1,
                                                         prevind(str, ovec[2i+2]+1)) for i=1:n]
    off = Int[ ovec[2i+1]+1 for i=1:n ]
    StrRegexMatch(mat, cap, Int(ovec[1]+1), off, re)
end

match(re::T, str::MaybeSub{<:Str{C}}, idx::Integer,
      add_opts::UInt32=UInt32(0)) where {T<:RegexTypes,C<:CSE} =
    error("$T not supported yet on strings with codeunit == UInt16 or UInt32")

match(re::Regex, str::MaybeSub{<:Str})   = match(re, str, 1)
match(re::StrRegex, str::AbstractString) = match(re, str, 1)

match(re::Regex, str::MaybeSub{<:Str{C}}, idx::Integer, add_opts::UInt32=UInt32(0)) where {C} =
    _match(basecse(C), re, str, Int(idx), add_opts)
match(re::StrRegex, str::MaybeSub{<:Str{C}}, idx::Integer, add_opts::UInt32=UInt32(0)) where {C} =
    _match(basecse(C), re, str, Int(idx), add_opts)

@inline function __find(::Type{C}, re, str, idx) where {C}
    check_compile(C, re)
    (PCRE.exec(re.regex, str, idx, cvt_match(C, re.match_options), re.match_data)
     ? ((Int(re.ovec[1]) + 1) : prevind(str, Int(re.ovec[2]) + 1)) : _not_found)
end

_find(::Type{C}, re, str) where {C} = __find(C, re, str, 0)

_find(::Type{C}, re, str, idx) where {C} =
    (idx-1 <= ncodeunits(str)
     ? __find(C, re, str, idx-1)
     : (@boundscheck boundserr(str, idx) ; return _not_found))

find(::Type{Fwd}, re::RegexTypes, str::MaybeSub{<:AbstractString}, idx::Integer) =
    throw(ArgumentError("regex search is only available for the String or Str types with " *
                        "codeunit == UInt8, or substrings of those types, " *
                        "use UTF8Str to convert"))

find(::Type{Fwd}, re::RegexTypes, str::MaybeSub{<:Str{C}}, idx::Integer) where {C<:Regex_CSEs} =
    _find(C, re, str, idx)
find(::Type{Fwd}, re::RegexTypes, str::MaybeSub{<:Str{UTF8CSE}}, idx::Integer) =
    _find(UTF8CSE, re, str, idx)
find(::Type{Fwd}, re::RegexTypes, str::MaybeSub{<:Str{_LatinCSE}}, idx::Integer) =
    _find(LatinCSE, re, str, idx)
find(::Type{Fwd}, re::RegexTypes, str::MaybeSub{String}, idx::Integer) =
    _find(UTF8CSE, re, str, idx)

find(::Type{First}, re::RegexTypes, str::MaybeSub{<:AbstractString}) =
    find(Fwd, re, str, 1)
find(::Type{First}, re::RegexTypes, str::MaybeSub{<:Str{C}}) where {C<:Regex_CSEs} =
    _find(C, re, str)
find(::Type{First}, re::RegexTypes, str::MaybeSub{<:Str{_LatinCSE}}) =
    _find(LatinCSE, re, str)
find(::Type{First}, re::RegexTypes, str::MaybeSub{String}) =
    _find(UTF8CSE, re, str)

struct StrRegexMatchIterator{R<:RegexTypes,T<:AbstractString}
    regex::R
    string::T
    overlap::Bool

    StrRegexMatchIterator(regex::RegexTypes, string::AbstractString, ovr::Bool=false) =
        new{typeof(regex),typeof(string)}(regex, string, ovr)
end
compile(itr::StrRegexMatchIterator) = (compile(itr.regex); itr)
eltype(::Type{StrRegexMatchIterator}) = StrRegexMatch
start(itr::StrRegexMatchIterator) = match(itr.regex, itr.string, 1, UInt32(0))
done(itr::StrRegexMatchIterator, prev_match) = (prev_match === nothing)
IteratorSize(::Type{StrRegexMatchIterator}) = SizeUnknown()

# Assumes prev_match is not nothing
function next(itr::StrRegexMatchIterator, prev_match)
    opts_nonempty = UInt32(PCRE.ANCHORED | PCRE.NOTEMPTY_ATSTART)
    if isempty(prev_match.match)
        offset = prev_match.offset + (itr.overlap ? 0 : ncodeunits(prev_match.match))
        while ((mat = match(itr.regex, itr.string, offset, opts_nonempty) === nothing) &&
               prevempty && offset <= sizeof(itr.string))
            offset = nextind(itr.string, offset)
            prevempty = false
        end
    else
        offset = (itr.overlap
                  ? nextind(itr.string, prev_match.offset)
                  : prev_match.offset + ncodeunits(prev_match.match))
        while ((mat = match(itr.regex, itr.string, offset, UInt32(0)) === nothing) &&
               prevempty && offset <= sizeof(itr.string))
            offset = nextind(itr.string, offset)
            prevempty = false
        end
    end
    (prev_match, mat)
end

eachmatch(re::StrRegex, str::AbstractString; overlap = false) =
    StrRegexMatchIterator(re, str, overlap)

## comparison ##

function ==(a::RegexTypes, b::RegexTypes)
    a.pattern == b.pattern &&
        a.compile_options == b.compile_options &&
        a.match_options == b.match_options
end

## hash ##
const hashre_seed = UInt === UInt64 ? 0x67e195eb8555e72d : 0xe32373e4
function hash(r::StrRegex, h::UInt)
    h += hashre_seed
    h = hash(r.pattern, h)
    h = hash(r.compile_options, h)
    h = hash(r.match_options, h)
end

_occurs_in(r::RegexTypes, s::AbstractString, off::Integer) =
    (compile(r) ; PCRE.exec(r.regex, UTF8Str(s), off, r.match_options, r.match_data))
_occurs_in(r::RegexTypes, s::MaybeSub{<:Str}, off::Integer) =
    (compile(r) ; PCRE.exec(r.regex, s, off, r.match_options, r.match_data))

occurs_in(needle::StrRegex, hay::AbstractString; off::Integer=0) = _occurs_in(needle, hay, off)
occurs_in(needle::Regex, hay::Str; off::Integer=0)               = _occurs_in(needle, hay, off)
occurs_in(needle::Regex, hay::SubString{<:Str}; off::Integer=0)  = _occurs_in(needle, hay, off)
