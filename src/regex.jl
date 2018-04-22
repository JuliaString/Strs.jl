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

const Binary_Regex_CSEs = Union{ASCIICSE,Latin_CSEs,Binary_CSEs}
const Regex_CSEs = Union{Binary_Regex_CSEs,UTF8CSE,UniPlusCSE}

# Default is to act as if validated UTF8
cvt_compile(::Type{<:CSE}, co) = UInt32(co) | PCRE.NO_UTF_CHECK | PCRE.UTF
cvt_match(::Type{<:CSE}, co)   = UInt32(co) | PCRE.NO_UTF_CHECK

cvt_compile(::Type{Binary_Regex_CSEs}, co) = UInt32(co) & ~PCRE.UTF

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

match_type(::Type{UTF8CSE})       = 1
match_type(::Type{UniPlusCSE})    = 1
match_type(::Type{UTF16CSE})      = 2
match_type(::Type{<:UTF32_CSEs})  = 3
match_type(::Type{ASCIICSE})      = 4
match_type(::Type{<:Binary_CSEs}) = 4
match_type(::Type{<:Latin_CSEs})  = 4
match_type(::Type{Text2CSE})      = 5
match_type(::Type{<:UCS2_CSEs})   = 5
match_type(::Type{Text4CSE})      = 6

function finalize! end

mutable struct StrRegex{T<:AbstractString}
    pattern::T
    compile_options::UInt32
    match_options::UInt32
    regex::Ptr{Cvoid} # Current compiled regex
    match_type::Int
    ovec::Vector{Csize_t}
    match_data::Ptr{Cvoid}

    # There are 6 combinations of CodeUnit size (8,16,32) and UTF/no-UTF
    # 1 - UTF8
    # 2 - UTF16
    # 3 - UTF32/_UTF32
    # 4 - Text1/ASCII/Binary - Latin/_Latin need (*UCP), but not (*UTF)
    # 5 - Text2 - UCS2/_UCS2 need (*UCP), but not (*UTF)
    # 6 - Text4
    table::NTuple{6, Ptr{Cvoid}}

    function StrRegex(pattern::T, compile_options::Integer, match_options::Integer
                      ) where {T<:AbstractString}
        re = compile(cse(T),
                     new{T}(pattern,
                            compile_opts(T, compile_options), match_opts(T, match_options),
                            C_NULL, 0, Csize_t[], C_NULL,
                            (C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL)))
        @static V6_COMPAT ? finalizer(re, finalize!) : finalizer(finalize!, re)
        re
    end
end

function update_table(t, re, n)
    if n < 4
        (n == 1 ? (re, t[2], t[3], t[4], t[5], t[6])
         : n == 2 ? (t[1], re, t[3], t[4], t[5], t[6]) : (t[1], t[2], re, t[4], t[5], t[6]))
    else
        (n == 4 ? (t[1], t[2], t[3], re, t[5], t[6])
         : n == 5 ? (t[1], t[2], t[3], t[4], re, t[6]) : (t[1], t[2], t[3], t[4], t[5], re))
    end
end

function finalize!(re)
    if re.regex != C_NULL
        for reg in re.table
            reg == C_NULL || PCRE.free_re(reg)
        end
        re.regex = C_NULL
        re.table = (C_NULL, C_NULL, C_NULL, C_NULL, C_NULL, C_NULL)
    end
    re.match_data == C_NULL || PCRE.free_match_data(re.match_data)
end

const RegexTypes = Union{Regex, StrRegex}

function _update_compile_opts(flags)
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

#=
Regex(pattern::MaybeSub{<:Str}, co, mo) = StrRegex(pattern, co, mo)
Regex(pattern::MaybeSub{<:Str}, flags::AbstractString) = StrRegex(pattern, flags)
Regex(pattern::MaybeSub{<:Str}) = StrRegex(pattern)
=#

# Yes, this is type piracy, but it is needed to make all string types work together easily
Regex(pattern::AbstractString, co::Integer, mo::Integer) = StrRegex(pattern, co, mo)
Regex(pattern::AbstractString, flags::AbstractString) = StrRegex(pattern, flags)
Regex(pattern::AbstractString) = StrRegex(pattern)

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

function compile(::Type{C}, regex::StrRegex) where {C<:Regex_CSEs}
    (nm = match_type(C)) == regex.match_type && return
    regex.match_type = nm
    if (re = regex.table[nm]) == C_NULL
        regex.compile_options = cvtcomp = cvt_compile(C, regex.compile_options)
        regex.regex = re = PCRE.compile(regex.pattern, cvtcomp)
        regex.table = update_table(regex.table, re, nm)
        PCRE.jit_compile(re)
    end
    if regex.match_data == C_NULL
        regex.match_data = PCRE.create_match_data(re)
        regex.ovec = PCRE.get_ovec(regex.match_data)
    end
    regex
end

function compile(::Type{C}, regex::Regex) where {C<:Regex_CSEs}
    re = regex.regex
    # ASCII is compatible with all (current) types, don't recompile
    C == ASCIICSE && re != C_NULL && return
    oldopt = regex.compile_options
    cvtcomp = cvt_compile(C, oldopt)
    if cvtcomp != oldopt
        regex.compile_options = cvtcomp
        re == C_NULL || (PCRE.free_re(re); regex.regex = re = C_NULL)
    end
    if re == C_NULL
        regex.regex = re = PCRE.compile(regex.pattern, cvtcomp)
        PCRE.jit_compile(re)
    end
    if regex.match_data == C_NULL
        regex.match_data = PCRE.create_match_data(re)
        regex.ovec = PCRE.get_ovec(regex.match_data)
    end
    regex
end

function _match(::Type{C}, re, str, idx, add_opts) where {C<:CSE}
    compile(C, re)
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
    compile(C, re)
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
find(::Type{Fwd}, re::RegexTypes, str::MaybeSub{<:Str{_LatinCSE}}, idx::Integer) =
    _find(LatinCSE, re, str, idx)
find(::Type{Fwd}, re::RegexTypes, str::MaybeSub{String}, idx::Integer) =
    _find(UniPlusCSE, re, str, idx)

find(::Type{First}, re::RegexTypes, str::MaybeSub{<:AbstractString}) =
    find(Fwd, re, str, 1)
find(::Type{First}, re::RegexTypes, str::MaybeSub{<:Str{C}}) where {C<:Regex_CSEs} =
    _find(C, re, str)
find(::Type{First}, re::RegexTypes, str::MaybeSub{<:Str{_LatinCSE}}) =
    _find(LatinCSE, re, str)
find(::Type{First}, re::RegexTypes, str::MaybeSub{String}) =
    _find(UniPlusCSE, re, str)

struct StrRegexMatchIterator{R<:RegexTypes,T<:AbstractString}
    regex::R
    string::T
    overlap::Bool

    StrRegexMatchIterator{R,T}(regex::R, string::T, ovr::Bool=false
                               ) where {R<:RegexTypes,T<:AbstractString} =
        new{R,T}(regex, string, ovr)
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

==(a::RegexTypes, b::RegexTypes) =
    a.pattern == b.pattern &&
    a.compile_options == b.compile_options &&
    a.match_options == b.match_options

## hash ##
hash(r::StrRegex, h::UInt) =
    hash(r.match_options,
         hash(r.compile_options,
              hash(r.pattern, h + UInt === UInt64 ? 0x67e195eb8555e72d : 0xe32373e4)))

_occurs_in(r::RegexTypes, s::AbstractString, off::Integer) =
    (compile(UTF8CSE, r) ; PCRE.exec(r.regex, UTF8Str(s), off, r.match_options, r.match_data))
_occurs_in(r::RegexTypes, s::MaybeSub{<:Str{C}}, off::Integer) where {C<:Regex_CSEs} =
    (compile(C, r) ; PCRE.exec(r.regex, s, off, r.match_options, r.match_data))

occurs_in(needle::StrRegex, hay::AbstractString; off::Integer=0) = _occurs_in(needle, hay, off)
occurs_in(needle::Regex, hay::Str; off::Integer=0)               = _occurs_in(needle, hay, off)
occurs_in(needle::Regex, hay::SubString{<:Str}; off::Integer=0)  = _occurs_in(needle, hay, off)
