# This file includes code from Julia.
# License is MIT: http://julialang.org/license

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

function utf8proc_map(::Type{T}, str, options::Integer) where {T<:Str}
    nwords = ccall(:utf8proc_decompose, Int, (Ptr{UInt8}, Int, Ptr{UInt8}, Int, Cint),
                   str, sizeof(str), C_NULL, 0, options)
    nwords < 0 && utf8proc_error(nwords)
    buffer = _allocate(nwords*4)
    nwords = ccall(:utf8proc_decompose, Int, (Ptr{UInt8}, Int, Ptr{UInt8}, Int, Cint),
                   str, sizeof(str), buffer, nwords, options)
    nwords < 0 && utf8proc_error(nwords)
    nbytes = ccall(:utf8proc_reencode, Int, (Ptr{UInt8}, Int, Cint), buffer, nwords, options)
    nbytes < 0 && utf8proc_error(nbytes)
    T(resize!(buffer, nbytes))
end

utf8proc_map(str::T, options::Integer) where {T<:Str} = utf8proc_map(T, UTF8Str(str), options)
utf8proc_map(str::UTF8Str, options::Integer) = utf8proc_map(UTF8Str, str, options)

############################################################################

import Base: isalpha, isdigit, isxdigit, isalnum, iscntrl, ispunct, isspace, isprint, isgraph,
             islower, isupper, lowercase, uppercase, titlecase, lcfirst, ucfirst, isascii

@condimport isnumeric
@condimport textwidth

@static if isdefined(Base, :Unicode) && isdefined(Base.Unicode, :graphemes)
    import Base.Unicode: graphemes
else
    import Base: graphemes
end

export isgraphemebreak

@static if isdefined(Base, :isnumber)
    import Base: isnumber
    isnumber(val::CodePoint) = isnumeric(val)
end
@static if VERSION < v"0.7.0-DEV"
    import Base: is_assigned_char, isassigned, normalize_string
    is_assigned_char(ch::CodePoint) = isassigned(ch)
    normalize_string(::Type{T}, str::T; kwargs...) where {T<:Str} = normalize(T, str; kwargs...)
else
    import Base.Unicode: isassigned, normalize
end

############################################################################

function normalize(::Type{T}, str::T;
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

normalize(str::T, options::Integer) where {T<:Str} = normalize(T, UTF8Str(str), options)
normalize(str::UTF8Str, options::Integer) = normalize(UTF8Str, str, options)

normalize(str::Str, nf::Symbol) =
    utf8proc_map(str, nf == :NFC ? (Uni.STABLE | Uni.COMPOSE) :
                 nf == :NFD ? (Uni.STABLE | Uni.DECOMPOSE) :
                 nf == :NFKC ? (Uni.STABLE | Uni.COMPOSE | Uni.COMPAT) :
                 nf == :NFKD ? (Uni.STABLE | Uni.DECOMPOSE | Uni.COMPAT) :
                 unierror(UTF_ERR_NORMALIZE, nf))

############################################################################

## character column width function ##

textwidth(ch::CodePointTypes) = Int(ccall(:utf8proc_charwidth, Cint, (UInt32,), ch))
textwidth(ch::ASCIIChr) = 1
textwidth(ch::LatinChars) = 1

textwidth(str::Str) = mapreduce(textwidth, +, 0, str)
textwidth(str::ASCIIStr) = length(str)
textwidth(str::LatinStrings) = length(str)

############################################################################

@inline _cat(ch::CodePointTypes) = ccall(:utf8proc_category, Cint, (UInt32,), tobase(ch))
@inline _cat(ch::Union{UInt32, Text4Chr}) = ch <= '\U10ffff' ? _cat(ch%UTF32Chr) : Cint(30)

# returns code in 0:30 giving Unicode category
@inline category_code(ch::CodePointTypes) = _cat(ch)

@inline _cat_abbr(ch::CodePointTypes) =
    unsafe_string(ccall(:utf8proc_category_string, Cstring, (UInt32,), tobase(ch)))

@inline _lowercase_u(ch) = ccall(:utf8proc_tolower, UInt32, (UInt32,), ch)
@inline _uppercase_u(ch) = ccall(:utf8proc_toupper, UInt32, (UInt32,), ch)
@inline _titlecase_u(ch) = ccall(:utf8proc_totitle, UInt32, (UInt32,), ch)

# more human-readable representations of the category code
@inline category_abbrev(ch::CodePointTypes) = _cat_abbr(ch)
@inline category_abbrev(ch::Union{UInt32, Text4Chr}) = ch <= '\U10ffff' ? _cat_abbr(ch) : "In"

category_string(ch::CodePointTypes) = category_strings[category_code(ch) + 1]

isassigned(ch::CodePointTypes) = category_code(ch) != Uni.CN

_cat_mask(a) = a
@inline _cat_mask(a, b) = (1 << a%UInt) | (1 << b%UInt)
@inline _cat_mask(rng::UnitRange) = ((2 << rng.stop%UInt) - 1) & ~((1 << rng.start%UInt) - 1)

@inline _check_mask(ch, mask) = ((1%UInt << _cat(ch)%UInt) & mask) != 0

## libc character class predicates ##

# 0xb5, 0xdf, and 0xff cannot be uppercased in LatinStr, although they are lowercase
@inline _can_upper_l(c) = (0xe0 <= c <= 0xfe) & (c != 0xf7)
@inline _can_upper(c) = _islower_a(c) | _can_upper_l(c)

@inline _iscntrl(ch) = (ch <= 0x1f) | (0x7f <= ch <= 0x9f)
@inline _isdigit(ch) = (ch - '0'%UInt8) <= 9
@inline _isxdigit(ch) = _isdigit(ch) | (ch - 'A'%UInt8 < 6) | (ch - 'a'%UInt8 < 6)

const _isupper_mask   = _cat_mask(Uni.LU, Uni.LT)
const _isalpha_mask   = _cat_mask(Uni.LU : Uni.LO)
const _isnumeric_mask = _cat_mask(Uni.ND : Uni.NO)
const _ispunct_mask   = _cat_mask(Uni.PC : Uni.PO)
const _isprint_mask   = _cat_mask(Uni.LU : Uni.ZS)
const _isgraph_mask   = _cat_mask(Uni.LU : Uni.SO)
const _isalnum_mask   = _isnumeric_mask | _isalpha_mask

############################################################################
# Definitions for characters in the ASCII subset of Unicode

const _isnumeric_a = _isdigit
@inline _ispunct_a(ch) = ((1%UInt128 << ch) & 0x2800_0000_b800_0001_8c00_f7ee_0000_0000) != 0
@inline _isspace_a(ch) = (ch == 32) | (9 <= ch <= 13)
@inline _islower_a(ch) = (ch - 'a'%UInt8) < 26
@inline _isupper_a(ch) = (ch - 'A'%UInt8) < 26
@inline _isalpha_a(ch) = _islower_a(ch) | _isupper_a(ch)
@inline _isalnum_a(ch) = _isdigit(ch) | _isalpha_a(ch)
@inline _isprint_a(ch) = 0x20 <= ch < 0x7f
@inline _isgraph_a(ch) = 0x20 < ch < 0x7f

############################################################################
# Definitions for characters in the Latin1 subset of Unicode, but not in the ASCII subset

@inline _isnumeric_l(ch) = (ch <= 0xbe && ((1<<(ch-0xb2)) & 0x1c83) != 0)
@inline _ispunct_l(ch)   = ((1 << (ch-0x80)) & 0x88c0_0882_0000_0000) != 0
@inline _isspace_l(ch)   = (ch == 0x85) | (ch == 0xa0)
@inline _islower_l(c)    = ((0xdf <= c <= 0xff) & (c != 0xf7)) | (c == 0xb5)
@inline _isupper_l(c)    = (0xc0 <= c%UInt8 <= 0xde) & (c != 0xd7)
@inline _isalpha_l(c)    = ((0xc0 <= c <= 0xff) & (c != 0xf7) & (c != 0xd7)) | (c == 0xb5)
@inline _isalnum_l(c)    = _isalpha_l(c) || _isnumeric_l(c)
@inline _isprint_l(ch)   = ((0xa0 <= ch <= 0xff) & (ch != 0xad))
@inline _isgraph_l(ch)   = ((0xa0 < ch <= 0xff) & (ch != 0xad))

############################################################################
# Definitions for any Unicode codepoint (requires call to utf8proc) (only used for non-Latin1)

@inline _isnumeric_u(ch) = _check_mask(ch, _isnumeric_mask)
@inline _ispunct_u(ch)   = _check_mask(ch, _ispunct_mask)
@inline _isspace_u(ch)   = _cat(ch) == Uni.ZS
@inline _islower_u(ch)   = _cat(ch) == Uni.LL
@inline _isupper_u(ch)   = _check_mask(ch, _isupper_mask)
@inline _isalpha_u(ch)   = _check_mask(ch, _isalpha_mask)
@inline _isalnum_u(ch)   = _check_mask(ch, _isalnum_mask)
@inline _isprint_u(ch)   = _check_mask(ch, _isprint_mask)
@inline _isgraph_u(ch)   = _check_mask(ch, _isgraph_mask)

############################################################################
# Fallback definitions for all CodePoint types

@inline iscntrl(ch::CodePointTypes)  = _iscntrl(tobase(ch))
@inline isdigit(ch::CodePointTypes)  = _isdigit(tobase(ch))
@inline isxdigit(ch::CodePointTypes) = _isxdigit(tobase(ch))

@inline isascii(ch::Unsigned)    = ch <= 0x7f
@inline isascii(ch::CodePoint)   = isascii(tobase(ch))
@inline isascii(ch::ASCIIChr)    = true

@inline islatin(ch::Unsigned)    = ch <= 0xff
@inline islatin(ch::CodePoint)   = islatin(tobase(ch))

@inline isbmp(ch::Unsigned)      = ch <= 0xffff && !is_surrogate_codeunit(ch)
@inline isbmp(ch::UInt8)         = true
@inline isbmp(ch::CodePoint)     = isbmp(tobase(ch))

@inline isunicode(ch::Unsigned)  = ch <= 0x10ffff && !is_surrogate_codeunit(ch)
@inline isunicode(ch::UInt8)     = true
@inline isunicode(ch::CodePoint) = isunicode(tobase(ch))

const _catfuns = (:numeric, :punct, :space, :lower, :upper, :alpha, :alnum, :print, :graph)

for fnam in _catfuns
    isfnam  = Symbol(string("is", fnam))
    namroot  = string("_is", fnam)
    fnam_a  = Symbol(string(namroot, "_a"))
    fnam_al = Symbol(string(namroot, "_al"))
    fnam_ch = Symbol(string(namroot, "_ch"))
        
    @eval $(fnam_al)(ch) = isascii(ch) ? $(fnam_a)(ch) : $(Symbol(string(namroot, "_l")))(ch)
    @eval $(fnam_ch)(ch) = islatin(ch) ? $(fnam_al)(ch) : $(Symbol(string(namroot, "_u")))(ch)
    @eval $(isfnam)(ch::CodePointTypes) = $(fnam_ch)(tobase(ch))
    @eval $(isfnam)(ch::ASCIIChr)      = $(fnam_a)(tobase(ch))
    @eval $(isfnam)(ch::LatinChars)    = $(fnam_al)(tobase(ch))
end

############################################################################
# iterators for grapheme segmentation

isgraphemebreak(c1::CodePointTypes, c2::CodePointTypes) =
    !(c1 <= 0x10ffff && c2 <= 0x10ffff) ||
    ccall(:utf8proc_grapheme_break, Bool, (UInt32, UInt32), c1, c2)

# Stateful grapheme break required by Unicode-9 rules: the string
# must be processed in sequence, with state initialized to Ref{Int32}(0).
# Requires utf8proc v2.0 or later.
isgraphemebreak!(state::Ref{Int32}, c1::CodePointTypes, c2::CodePointTypes) =
    ((c1 <= 0x10ffff && c2 <= 0x10ffff)
     ? ccall(:utf8proc_grapheme_break_stateful, Bool, (UInt32, UInt32, Ref{Int32}), c1, c2, state)
     : (state[] = 0; true))

@static if isdefined(Base, :ismalformed)
    Base.ismalformed(ch::CodePoint) = false
    Base.isoverlong(ch::CodePoint) = false
end
