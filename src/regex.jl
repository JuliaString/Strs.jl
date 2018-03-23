#=
Regex functions for Str strings

Copyright 2018 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based in part on julia/base/regex.jl
=#

using Base.PCRE

using Base: DEFAULT_COMPILER_OPTS, DEFAULT_MATCH_OPTS
import Base: Regex, match

const Regex_CSEs = Union{ASCIICSE,Latin_CSEs,Binary_CSEs}

# Default is to act as if validated UTF8
cvt_compile(::Type{<:CSE}, co) = co | PCRE.NO_UTF_CHECK | PCRE.UTF
cvt_match(::Type{<:CSE}, co)   = co | PCRE.NO_UTF_CHECK

cvt_compile(::Type{Regex_CSEs}, co) = co & ~PCRE.UTF
cvt_match(::Type{Regex_CSEs}, co)   = co & ~PCRE.NO_UTF_CHECK

cvt_compile(::Type{<:Str{C}}, co) where {C<:CSE} = cvt_compile(C, co)
cvt_match(::Type{<:Str{C}}, co) where {C<:CSE}   = cvt_match(C, co)

# String type is not validate
cvt_compile(::Type{String}, co) = (co & ~PCRE.NO_UTF_CHECK) | PCRE.UTF
cvt_match(::Type{String}, co)   = (co & ~PCRE.NO_UTF_CHECK)

Regex(pattern::Str{C}, co, mo) where {C<:Byte_CSEs} =
    Regex(String(pattern), cvt_compile(C, co), mo)
Regex(pattern::Str{C}) where {C<:Byte_CSEs} =
    Regex(String(pattern),
          cvt_compile(C, DEFAULT_COMPILER_OPTS), cvt_match(C, DEFAULT_MATCH_OPTS))

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

Regex(pattern::Str{C}, flags::AbstractString) where {C<:Byte_CSEs} =
    Regex(pattern, cvt_compile(C, _update_compile_opts(flags)), cvt_match(C, DEFAULT_MATCH_OPTS))

struct RegexMatchStr{T<:AbstractString}
    match::T
    captures::Vector{Union{Nothing,T}}
    offset::Int
    offsets::Vector{Int}
    regex::Regex
end

function show(io::IO, m::RegexMatchStr)
    print(io, "RegexMatchStr(")
    show(io, m.match)
    idx_to_capture_name = PCRE.capture_names(m.regex.regex)
    if !is_empty(m.captures)
        print(io, ", ")
        for i = 1:length(m.captures)
            # If the capture group is named, show the name.
            # Otherwise show its index.
            capture_name = get(idx_to_capture_name, i, i)
            print(io, capture_name, "=")
            show(io, m.captures[i])
            if i < length(m.captures)
                print(io, ", ")
            end
        end
    end
    print(io, ")")
end

# Capture group extraction
getindex(m::RegexMatchStr, idx::Integer) = m.captures[idx]
function getindex(m::RegexMatchStr, name::Symbol)
    idx = PCRE.substring_number_from_name(m.regex.regex, name)
    idx <= 0 && error("no capture group named $name found in regex")
    m[idx]
end

getindex(m::RegexMatchStr, name::AbstractString) = m[Symbol(name)]

"""
Check if the compile flags match the current search

If not, free up old compilation, and recompile
For better performance, the Regex object should hold spots for 5 compiled regexes,
for 8-bit, UTF-8, 16-bit, UTF-16, and 32-bit code units
"""
function check_compile(::Type{C}, regex::Regex) where {C<:Union{CSE,String}}
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
    RegexMatchStr(mat, cap, Int(ovec[1]+1), off, re)
end

match(re::Regex, str::Str{C}, idx::Integer, add_opts::UInt32=UInt32(0)) where {C<:CSE} =
    error("Regex not supported yet on strings with codeunit == UInt16 or UInt32")
match(re::Regex, str::Str{C}, idx::Integer, add_opts::UInt32=UInt32(0)) where {C<:Regex_CSEs} =
    _match(C, re, str, Int(idx), add_opts)
match(re::Regex, str::Str{UTF8CSE}, idx::Integer, add_opts::UInt32=UInt32(0)) =
    _match(UTF8CSE, re, str, Int(idx), add_opts)
match(re::Regex, str::Str{_LatinCSE}, idx::Integer, add_opts::UInt32=UInt32(0)) =
    _match(LatinCSE, re, Str{LatinCSE}(str), Int(idx), add_opts)


const StrOrSubStr{T} = Union{T,SubString{<:T}}

function _find(::Type{C}, re, str, idx) where {C}
    if idx > ncodeunits(str)
        @boundscheck boundserr(str, idx)
        return _not_found
    end
    check_compile(C, re)
    (PCRE.exec(re.regex, str, idx-1, cvt_match(C, re.match_options), re.match_data)
     ? ((Int(re.ovec[1])+1):prevind(str,Int(re.ovec[2])+1)) : _not_found)
end

find(::Type{Fwd}, re::Regex, str::StrOrSubStr{AbstractString}, idx::Integer) =
    throw(ArgumentError("regex search is only available for the String or Str types with " *
                        "codeunit == UInt8, or substrings of those types, use UTF8Str to convert"))

find(::Type{Fwd}, re::Regex, str::StrOrSubStr{Str{C}}, idx::Integer) where {C<:Regex_CSEs} =
    _find(C, re, str, idx)
find(::Type{Fwd}, re::Regex, str::StrOrSubStr{Str{C}}, idx::Integer) where {C<:UTF8CSE} =
    _find(C, re, str, idx)
find(::Type{Fwd}, re::Regex, str::StrOrSubStr{Str{C}}, idx::Integer) where {C<:_LatinCSE} =
    _find(LatinCSE, re, str, idx)
find(::Type{Fwd}, re::Regex, str::StrOrSubStr{String}, idx::Integer) =
    _find(String, re, str, idx)
