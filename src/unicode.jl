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
    buffer = Base.StringVector(nwords*4)
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

const _mod = @static isdefined(Base, :Unicode) ? "Base.Unicode" : "Base"
const _list =
    "normalize, graphemes, isassigned, islower, isupper, isalpha, isdigit, " *
    "isxdigit, isalnum, iscntrl, ispunct, isspace, isprint, isgraph, " *
    "lowercase, uppercase, titlecase, lcfirst, ucfirst, isascii"

eval(Meta.parse("import $_mod: $_list"))
eval(Meta.parse("export $_list"))

@static if _mod == "Base"
    import Base: isnumber
else
    import Base.Unicode: isnumeric, textwidth
end
export isnumeric, textwidth

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
    (compose & decompose) &&
        throw(ArgumentError("only one of decompose or compose may be true"))
    (!(compat | stripmark) & (compat | stripmark)) &&
        throw(ArgumentError("compat or stripmark true requires compose or decompose true"))
    newline2ls + newline2ps + newline2lf > 1 &&
        throw(ArgumentError("only one newline conversion may be specified"))
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
                 throw(ArgumentError(":$nf is not one of :NFC, :NFD, :NFKC, :NFKD")))

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
@inline _cat(ch::Union{UInt32, RawChar}) = ch <= '\U10ffff' ? _cat(ch%UTF32Chr) : Cint(30)

# returns code in 0:30 giving Unicode category
@inline category_code(ch::CodePointTypes) = _cat(ch)

@inline _cat_abbr(ch::CodePointTypes) =
    unsafe_string(ccall(:utf8proc_category_string, Cstring, (UInt32,), tobase(ch)))

# more human-readable representations of the category code
@inline category_abbrev(ch::CodePointTypes) = _cat_abbr(ch)
@inline category_abbrev(ch::Union{UInt32, RawChar}) = ch <= '\U10ffff' ? _cat_abbr(ch) : "In"

category_string(ch::CodePointTypes) = category_strings[category_code(ch) + 1]

isassigned(ch::CodePointTypes) = category_code(ch) != Uni.CN

_cat_mask(a) = a
@inline _cat_mask(a, b) = (1 << a%UInt) | (1 << b%UInt)
@inline _cat_mask(rng::UnitRange) = ((2 << rng.stop%UInt) - 1) & ~((1 << rng.start%UInt) - 1)

@inline _check_mask(ch, mask) = ((1%UInt << _cat(ch)%UInt) & mask) != 0
@inline _check_mask(ch, rng::UnitRange) = _check_mask(ch, _cat_mask(rng))
@inline _check_mask(ch, a, b) = _check_mask(ch, _cat_mask(a, b))

## libc character class predicates ##

const _isalpha_mask   = _cat_mask(Uni.LU : Uni.LO)
const _isnumeric_mask = _cat_mask(Uni.ND : Uni.NO)

@inline _isasciilower(c) = 'a'%UInt8 <= c <= 'z'%UInt8
@inline _isasciiupper(c) = 'A'%UInt8 <= c <= 'Z'%UInt8

@inline _islatinlower(c) = (0xdf <= c <= 0xfe & !(c == 0xf7))
@inline _islatinupper(c::LatinChr) = (0xc0 <= c%UInt8 <= 0xde & !(c%UInt8 == 0xd7))
@inline _islatinupper(c) = _islatinupper(c%LatinChr) | (c == 0xb5 || c == 0xff)

@inline _iscntrl(ch) = (ch <= 0x1f) | (0x7f <= ch <= 0x9f)
@inline _isdigit(ch) = '0'%UInt8 <= ch <= '9'%UInt8
@inline _isxdigit(ch) =
    _isdigit(ch) | ('A'%UInt8 <= ch <= 'F'%UInt8) | ('a'%UInt8 <= ch <= 'f'%UInt)

const _punct_00 = 0x8c00f7ee00000000
const _punct_40 = 0x28000000b8000001
const _punct_80 = 0x88c0088200000000
@inline _ispunct(ch) =
    (ch <= 0x3f ? ((1 << ch) & _punct_00) : ((1 << (ch - 0x40)) & _punct_40)) != 0
@inline _ispunctlatin(ch) = ch <= 0x7f ? _ispunct(ch) : (((1 << (ch-0x80)) & _punct_80) != 0)

@inline _isspace(ch) =
    (ch == 32 | 9 <= ch <= 13) || (ch == 0x85 | ch == 0xa0) || (ch > 0xff && _cat(ch) == Uni.ZS)

@inline _isprintascii(ch) = 0x20 <= ch < 0x7f
@inline _isprintlatin(ch) = _isprintascii(ch) | ((0xa0 <= ch <= 0xff) & (ch != 0xad))
@inline _isgraphascii(ch) = 0x20 < ch < 0x7f
@inline _isgraphlatin(ch) = _isgraphascii(ch) | ((0xa0 < ch <= 0xff) & (ch != 0xad))

@inline _isnumericlatin(ch) = _isdigit(ch) || (ch <= 0xbe && ((1<<(ch-0xb2)) & 0x1c83) != 0)

@inline iscntrl(ch::CodePointTypes) = _iscntrl(tobase(ch))
@inline isdigit(ch::CodePointTypes) = _isdigit(tobase(ch))
@inline isxdigit(ch::CodePointTypes) = _isxdigit(tobase(ch))

@inline isascii(ch::CodePointTypes) = tobase(ch) <= 0x7f
@inline isascii(ch::ASCIIChr) = true

@inline islatin(ch::CodePointTypes) = tobase(ch) <= 0xff
@inline islatin(ch::ASCIIChr)   = true
@inline islatin(ch::LatinChars) = true

@inline islower(ch::CodePointTypes) = islatin(ch) ? islower(ch%LatinUChr) : (_cat(ch) == Uni.LL)
@inline islower(ch::ASCIIChr)   = _isasciilower(tobase(ch))
@inline islower(ch::LatinChars) = _isasciilower(tobase(ch)) | _islatinlower(tobase(ch))

@inline isupper(ch::CodePointTypes) =
    islatin(ch) ? isupper(ch%LatinUChr) : _check_mask(ch, Uni.LU, Uni.LT)
@inline isupper(ch::ASCIIChr)   = _isasciiupper(ch)
@inline isupper(ch::LatinChars) = _isasciiupper(ch) | _islatinupper(ch)

@inline isalpha(ch::CodePointTypes) =
    islatin(ch) ? isalpha(ch%LatinUChr) : _check_mask(ch, _isalpha_mask)
@inline isalpha(ch::ASCIIChr) = _isasciilower(ch) | _isasciiupper(ch)
@inline isalpha(ch::LatinChars) = isalpha(ch%ASCIIChr) | _islatinlower(ch) | _islatinupper(ch)

@inline isnumeric(ch::CodePointTypes) =
    islatin(ch) ? _isnumericlatin(tobase(ch)) : _check_mask(ch, _isnumeric_mask)
@inline isnumeric(ch::ASCIIChr) = _isdigit(tobase(ch))
@inline isnumeric(ch::LatinChars) = _isnumericlatin(tobase(ch))

@inline isalnum(ch::CodePointTypes) =
    islatin(ch) ? isalnum(ch%LatinUChr) : _check_mask(ch, _isnumeric_mask | _isalpha_mask)
@inline isalnum(ch::ASCIIChr) = isdigit(ch) | isalpha(ch)
@inline isalnum(ch::LatinChars) = isalpha(ch) | isnumeric(ch)

@inline ispunct(ch::CodePointTypes) =
    islatin(ch) ? _ispunctlatin(tobase(ch)) : _check_mask(ch, Uni.PC : Uni.PO)
@inline ispunct(ch::ASCIIChr) = _ispunct(tobase(ch))
@inline ispunct(ch::LatinChars) = _ispunctlatin(tobase(ch))

@inline isspace(ch::CodePointTypes) = _isspace(tobase(ch))
@inline isspace(ch::ASCIIChr) = ch == 32 | 9 <= ch <= 13
@inline isspace(ch::LatinChars) = ch == 32 | 9 <= ch <= 13 | ch == 0x85 | ch == 0xa0

@inline isprint(ch::CodePointTypes) =
    islatin(ch) ? _isprintlatin(tobase(ch)) : _check_mask(ch, Uni.LU : Uni.ZS)
@inline isprint(ch::ASCIIChr) = _isprintascii(tobase(ch))
@inline isprint(ch::LatinChars) = _isprintlatin(tobase(ch))

@inline isgraph(ch::CodePointTypes) =
    islatin(ch) ? _isgraphlatin(tobase(ch)) : _check_mask(ch, Uni.LU : Uni.SO)
@inline isgraph(ch::ASCIIChr) = _isgraphascii(tobase(ch))
@inline isgraph(ch::LatinChars) = _isgraphlatin(tobase(ch))

_lowercase(ch) = islatin(ch) ? _lowercase(ch) : ccall(:utf8proc_tolower, UInt32, (UInt32,), ch)
_uppercase(ch) = islatin(ch) ? _uppercase(ch) : ccall(:utf8proc_tolower, UInt32, (UInt32,), ch)
_titlecase(ch) = islatin(ch) ? _uppercase(ch) : ccall(:utf8proc_totitle, UInt32, (UInt32,), ch)

lowercase(ch::T) where {T<:CodePointTypes} = T(_lowercase(tobase(ch)))
uppercase(ch::T) where {T<:CodePointTypes} = T(_uppercase(tobase(ch)))
titlecase(ch::T) where {T<:CodePointTypes} = T(_titlecase(tobase(ch)))

lowercase(ch::ASCIIChr) = ifelse(isupper(ch), ASCIIChr(ch + 0x20), ch)
uppercase(ch::ASCIIChr) = ifelse(islower(ch), ASCIIChr(ch - 0x20), ch)
titlecase(ch::ASCIIChr) = uppercase(ch)

lowercase(ch::T) where {T<:LatinChars} = ifelse(isupper(ch), T(ch + 0x20), ch)
uppercase(ch::LatinChr) = ifelse(islower(ch), T(ch - 0x20), ch)

# Special handling for case where this is just an optimization of the first 256 bytes of Unicode,
# and not the 8-bit ISO 8859-1 character set
function uppercase(ch::LatinUChr)
    islower(ch%LatinChr) && return LatinUChr(ch + 0x20)
    ch == 0xb5 ? UCS2Chr(0x39c) : (ch == 0xff ? UCS2Chr(0x178) : ch)
end
titlecase(ch::LatinChars) = uppercase(ch)

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
