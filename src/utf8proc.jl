# This file includes code originally part of Julia.n
# Licensed under MIT License, see LICENSE.md

# The plan is to rewrite all of the functionality to not use the utf8proc library,
# and to use tables loaded up on initialization, as with StringLiterals.jl

# Currently, for characters outside the Latin1 subset, this depends on the following C calls:
# utf8proc_decompose
# utf8proc_reencode
# utf8proc_charwidth
# utf8proc_category
# utf8proc_category_string
# utf8proc_tolower
# utf8proc_toupper
# utf8proc_totitle

# For grapheme segmentation, this currently depends on the following 2 C calls:
# utf8proc_grapheme_break
# utf8proc_grapheme_break_stateful

# Unicode category constants
module Uni
const CN = 0
const LU = 1
const LL = 2
const LT = 3
const LM = 4
const LO = 5
const MN = 6
const MC = 7
const ME = 8
const ND = 9
const NL = 10
const NO = 11
const PC = 12
const PD = 13
const PS = 14
const PE = 15
const PI = 16
const PF = 17
const PO = 18
const SM = 19
const SC = 20
const SK = 21
const SO = 22
const ZS = 23
const ZL = 24
const ZP = 25
const CC = 26
const CF = 27
const CS = 28
const CO = 29

# strings corresponding to the category constants
const catstrings = [
    "Other, not assigned",
    "Letter, uppercase",
    "Letter, lowercase",
    "Letter, titlecase",
    "Letter, modifier",
    "Letter, other",
    "Mark, nonspacing",
    "Mark, spacing combining",
    "Mark, enclosing",
    "Number, decimal digit",
    "Number, letter",
    "Number, other",
    "Punctuation, connector",
    "Punctuation, dash",
    "Punctuation, open",
    "Punctuation, close",
    "Punctuation, initial quote",
    "Punctuation, final quote",
    "Punctuation, other",
    "Symbol, math",
    "Symbol, currency",
    "Symbol, modifier",
    "Symbol, other",
    "Separator, space",
    "Separator, line",
    "Separator, paragraph",
    "Other, control",
    "Other, format",
    "Other, surrogate",
    "Other, private use",
    "Invalid, too high",
    "Malformed, bad data",
]

const STABLE    = 1<<1
const COMPAT    = 1<<2
const COMPOSE   = 1<<3
const DECOMPOSE = 1<<4
const IGNORE    = 1<<5
const REJECTNA  = 1<<6
const NLF2LS    = 1<<7
const NLF2PS    = 1<<8
const NLF2LF    = NLF2LS | NLF2PS
const STRIPCC   = 1<<9
const CASEFOLD  = 1<<10
const CHARBOUND = 1<<11
const LUMP      = 1<<12
const STRIPMARK = 1<<13
end

############################################################################

utf8proc_error(result) =
    error(unsafe_string(ccall(:utf8proc_errmsg, Cstring, (Cssize_t,), result)))

function utf8proc_map(::Type{T}, str::MaybeSub{T}, options::Integer) where {T<:Str}
    nwords = ccall(:utf8proc_decompose, Int, (Ptr{UInt8}, Int, Ptr{UInt8}, Int, Cint),
                   str, sizeof(str), C_NULL, 0, options)
    nwords < 0 && utf8proc_error(nwords)
    buffer = Base.StringVector(nwords*4)
    nwords = ccall(:utf8proc_decompose, Int, (Ptr{UInt8}, Int, Ptr{UInt8}, Int, Cint),
                   str, sizeof(str), buffer, nwords, options)
    nwords < 0 && utf8proc_error(nwords)
    nbytes = ccall(:utf8proc_reencode, Int, (Ptr{UInt8}, Int, Cint), buffer, nwords, options)
    nbytes < 0 && utf8proc_error(nbytes)
    Str(cse(T), String(resize!(buffer, nbytes)))
end

utf8proc_charwidth(ch) = Int(ccall(:utf8proc_charwidth, Cint, (UInt32,), ch))

utf8proc_map(str::MaybeSub{T}, options::Integer) where {T<:Str} =
    utf8proc_map(T, convert(UTF8Str, str), options)
utf8proc_map(str::MaybeSub{<:Str{UTF8CSE}}, options::Integer) =
    utf8proc_map(UTF8Str, str, options)

@inline utf8proc_cat(ch) = ccall(:utf8proc_category, Cint, (UInt32,), ch)
@inline utf8proc_cat_abbr(ch) =
    unsafe_string(ccall(:utf8proc_category_string, Cstring, (UInt32,), ch))

@inline _lowercase_u(ch) = ccall(:utf8proc_tolower, UInt32, (UInt32,), ch)
@inline _uppercase_u(ch) = ccall(:utf8proc_toupper, UInt32, (UInt32,), ch)
@inline _titlecase_u(ch) = ccall(:utf8proc_totitle, UInt32, (UInt32,), ch)

############################################################################

function _normalize(::Type{T}, str::AbstractString;
                   stable::Bool      = false,
                   compat::Bool      = false,
                   compose::Bool     = true,
                   decompose::Bool   = false,
                   stripignore::Bool = false,
                   rejectna::Bool    = false,
                   newline2ls::Bool  = false,
                   newline2ps::Bool  = false,
                   newline2lf::Bool  = false,
                   stripcc::Bool     = false,
                   casefold::Bool    = false,
                   lump::Bool        = false,
                   stripmark::Bool   = false,
                   ) where {T<:Str}
    (compose & decompose) && unierror(UTF_ERR_DECOMPOSE_COMPOSE)
    (!(compat | stripmark) & (compat | stripmark)) && unierror(UTF_ERR_COMPAT_STRIPMARK)
    newline2ls + newline2ps + newline2lf > 1 && unierror(UTF_ERR_NL_CONVERSION)
    flags =
        ifelse(stable,      Uni.STABLE, 0) |
        ifelse(compat,      Uni.COMPAT, 0) |
        ifelse(decompose,   Uni.DECOMPOSE, 0) |
        ifelse(compose,     Uni.COMPOSE, 0) |
        ifelse(stripignore, Uni.IGNORE, 0) |
        ifelse(rejectna,    Uni.REJECTNA, 0) |
        ifelse(newline2ls,  Uni.NLF2LS, 0) |
        ifelse(newline2ps,  Uni.NLF2PS, 0) |
        ifelse(newline2lf,  Uni.NLF2LF, 0) |
        ifelse(stripcc,     Uni.STRIPCC, 0) |
        ifelse(casefold,    Uni.CASEFOLD, 0) |
        ifelse(lump,        Uni.LUMP, 0) |
        ifelse(stripmark,   Uni.STRIPMARK, 0)
    T(utf8proc_map(str, flags))
end

normalize(str::T, options::Integer) where {T<:Str} = _normalize(T, UTF8Str(str), options)
normalize(str::Str{UTF8CSE}, options::Integer) = _normalize(UTF8Str, str, options)

normalize(str::Str, nf::Symbol) =
    utf8proc_map(str, nf == :NFC ? (Uni.STABLE | Uni.COMPOSE) :
                 nf == :NFD ? (Uni.STABLE | Uni.DECOMPOSE) :
                 nf == :NFKC ? (Uni.STABLE | Uni.COMPOSE | Uni.COMPAT) :
                 nf == :NFKD ? (Uni.STABLE | Uni.DECOMPOSE | Uni.COMPAT) :
                 unierror(UTF_ERR_NORMALIZE, nf))

############################################################################

# iterators for grapheme segmentation

is_grapheme_break(c1::CodeUnitTypes, c2::CodeUnitTypes) =
    !(c1 <= 0x10ffff && c2 <= 0x10ffff) ||
    ccall(:utf8proc_grapheme_break, Bool, (UInt32, UInt32), c1, c2)

# Stateful grapheme break required by Unicode-9 rules: the string
# must be processed in sequence, with state initialized to Ref{Int32}(0).
# Requires utf8proc v2.0 or later.
is_grapheme_break!(state::Ref{Int32}, c1::CodeUnitTypes, c2::CodeUnitTypes) =
    ((c1 <= 0x10ffff && c2 <= 0x10ffff)
     ? ccall(:utf8proc_grapheme_break_stateful, Bool, (UInt32, UInt32, Ref{Int32}), c1, c2, state)
     : (state[] = 0; true))

is_grapheme_break(c1::Chr, c2::Chr) = is_grapheme_break(codepoint(c1), codepoint(c2))
is_grapheme_break(state::Ref{UInt32}, c1::Chr, c2::Chr) =
    is_grapheme_break(state, codepoint(c1), codepoint(c2))
