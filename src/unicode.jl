#=
Character and string classification functions

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones,
Licensed under MIT License, see LICENSE.md
=#

# Recommended by deprecate
@static if V6_COMPAT
    text_width(str::AbstractString) = strwidth(str)
    text_width(ch::Char) = charwidth(ch)

    import Base: is_assigned_char, normalize_string, isnumber
    Base.is_assigned_char(ch::CodePoint) = is_assigned(ch)
    Base.normalize_string(str::Str, opt::Symbol) = normalize(str, opt)
    Base.strwidth(str::Str) = text_width(str)
    Base.charwidth(ch::CodePoint) = text_width(ch)
    isnumber(val::CodePoint) = is_numeric(val)
else
    Base.Unicode.normalize(str::Str, opt::Symbol) = normalize(str, opt)
    Base.Unicode.isassigned(ch::CodePoint) = is_assigned(ch)
    is_graphic(ch::Char) = is_graphic(codepoint(ch))
    is_alphanumeric(ch::Char) = is_alphanumeric(codepoint(ch))
end

############################################################################

## character column width function ##

text_width(ch::UInt8)     = Int(ifelse(ch < 0x7f, ch > 0x1f, ch > 0x9f & ch != 0xad))
text_width(ch::UInt16)    = utf8proc_charwidth(ch)
text_width(ch::UInt32)    = utf8proc_charwidth(ch)
text_width(ch::CodePoint) = text_width(codepoint(ch))
text_width(ch::ASCIIChr)  = Int(32 <= codepoint(ch) <= 126)

text_width(str::Str) = mapreduce(text_width, +, 0, str)
text_width(str::Str{Union{ASCIICSE,Latin_CSEs}}) = length(str)

############################################################################

@inline _cat(ch::CodeUnitTypes) = ch <= 0x10ffff ? utf8proc_cat(ch) : Cint(30)
@inline _cat(ch::CodePoint)     = _cat(codepoint(ch))

# returns code in 0:30 giving Unicode category
@inline category_code(ch::Union{CodePoint,CodeUnitTypes}) = _cat(ch)

# more human-readable representations of the category code
@inline category_abbrev(ch::CodeUnitTypes) = ch <= 0x10ffff ? utf8proc_cat_abbr(ch) : "In"
@inline category_abbrev(ch::CodePoint)     = category_abbrev(codepoint(ch))

category_string(ch::CodeUnitTypes) = category_strings[_cat(ch) + 1]
category_string(ch::CodePoint)     = category_string(codepoint(ch))

is_assigned(ch::CodeUnitTypes) = category_code(ch) != Uni.CN
is_assigned(ch::CodePoint) = is_assigned(codepoint(ch))

_cat_mask(a) = a
@inline _cat_mask(a, b) = (1%UInt << a%UInt) | (1%UInt << b%UInt)
@inline _cat_mask(rng::UnitRange) =
    ((2%UInt << rng.stop%UInt) - 1) & ~((1%UInt << rng.start%UInt) - 1)

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
@inline _ispunct_l(ch)   = ((1%UInt64 << (ch-0x80)) & 0x88c0_0882_0000_0000) != 0
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

@inline is_control(ch::CodeUnitTypes)   = _iscntrl(ch)
@inline is_digit(ch::CodeUnitTypes)     = _isdigit(ch)
@inline is_hex_digit(ch::CodeUnitTypes) = _isxdigit(ch)

@inline is_control(ch::CodePoint)   = is_control(codepoint(ch))
@inline is_digit(ch::CodePoint)     = is_digit(codepoint(ch))
@inline is_hex_digit(ch::CodePoint) = is_hex_digit(codepoint(ch))

@inline is_ascii(ch::Unsigned)    = ch <= 0x7f
@inline is_ascii(ch::CodePoint)   = is_ascii(codepoint(ch))
@inline is_ascii(ch::ASCIIChr)    = true

@inline is_latin(ch::Unsigned)    = ch <= 0xff
@inline is_latin(ch::CodePoint)   = is_latin(codepoint(ch))

@inline is_bmp(ch::Unsigned)      = ch <= 0xffff && !is_surrogate_codeunit(ch)
@inline is_bmp(ch::UInt8)         = true
@inline is_bmp(ch::CodePoint)     = is_bmp(codepoint(ch))

@inline is_unicode(ch::Unsigned)  = ch <= 0x10ffff && !is_surrogate_codeunit(ch)
@inline is_unicode(ch::UInt8)     = true
@inline is_unicode(ch::CodePoint) = is_unicode(codepoint(ch))

const _catfuns =
    ((:numeric,      :numeric),
     (:punctuation,  :punct),
     (:space,        :space),
     (:lowercase,    :lower),
     (:uppercase,    :upper),
     (:alpha,        :alpha),
     (:alphanumeric, :alnum),
     (:printable,    :print),
     (:graphic,      :graph))

for (nnam, fnam) in _catfuns
    isnam  = Symbol(string("is_", nnam))
    namroot  = string("_is", fnam)
    fnam_a  = Symbol(string(namroot, "_a"))
    fnam_al = Symbol(string(namroot, "_al"))
    fnam_ch = Symbol(string(namroot, "_ch"))
        
    @eval $(fnam_al)(ch) = is_ascii(ch) ? $(fnam_a)(ch) : $(Symbol(string(namroot, "_l")))(ch)
    @eval $(fnam_ch)(ch) = is_latin(ch) ? $(fnam_al)(ch) : $(Symbol(string(namroot, "_u")))(ch)
    @eval $(isnam)(ch::CodeUnitTypes)  = $(fnam_ch)(ch)
    @eval $(isnam)(ch::CodePoint)      = $(fnam_ch)(codepoint(ch))
    @eval $(isnam)(ch::ASCIIChr)       = $(fnam_a)(codepoint(ch))
    @eval $(isnam)(ch::LatinChars)     = $(fnam_al)(codepoint(ch))
end

############################################################################

@static if isdefined(Base, :ismalformed)
    Base.ismalformed(ch::CodePoint) = false
    Base.isoverlong(ch::CodePoint) = false
    is_malformed(ch) = ismalformed(ch)
    is_overlong(ch) = isoverlong(ch)
else
    is_malformed(ch) = false
    is_overlong(ch) = false
end
