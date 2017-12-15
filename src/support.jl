# This file includes code that was formerly a part of Julia. License is MIT: http://julialang.org/license

## Error messages for Unicode / UTF support

const UTF_ERR_SHORT             = "invalid UTF-8 sequence starting at index <<1>> (0x<<2>> missing one or more continuation bytes)"
const UTF_ERR_CONT              = "invalid UTF-8 sequence starting at index <<1>> (0x<<2>> is not a continuation byte)"
const UTF_ERR_LONG              = "invalid UTF-8 sequence, overlong encoding starting at index <<1>> (0x<<2>>)"
const UTF_ERR_NOT_LEAD          = "not a leading Unicode surrogate code unit at index <<1>> (0x<<2>>)"
const UTF_ERR_NOT_TRAIL         = "not a trailing Unicode surrogate code unit at index <<1>> (0x<<2>>)"
const UTF_ERR_NOT_SURROGATE     = "not a valid Unicode surrogate code unit at index <<1>> (0x<<2>>)"
const UTF_ERR_MISSING_SURROGATE = "missing trailing Unicode surrogate code unit after index <<1>> (0x<<2>>)"
const UTF_ERR_INVALID           = "invalid Unicode character starting at index <<1>> (0x<<2>> > 0x10ffff)"
const UTF_ERR_SURROGATE         = "surrogate encoding not allowed in UTF-8 or UTF-32, at index <<1>> (0x<<2>>)"
const UTF_ERR_ODD_BYTES_16      = "UTF16String can't have odd number of bytes <<1>>"
const UTF_ERR_ODD_BYTES_32      = "UTF32String must have multiple of 4 bytes <<1>>"
const UTF_ERR_INVALID_CHAR      = "invalid Unicode character (0x<<2>> > 0x10ffff)"
const UTF_ERR_INVALID_8         = "invalid UTF-8 data"
const UTF_ERR_INVALID_16        = "invalid UTF-16 data"
const UTF_ERR_INVALID_UCS2      = "invalid UCS-2 character (surrogate present)"
const UTF_ERR_INVALID_LATIN1    = "invalid Latin1 character (> 0xff present)"
const UTF_ERR_INVALID_INDEX     = "invalid character index"
const UTF_ERR_MAP_CHAR          = "map(f,s::AbstractString) requires f to return Char; try map(f,collect(s)) or a comprehension instead"

if !isdefined(Base, :UnicodeError)
struct UnicodeError <: Exception
    errmsg::AbstractString   ##< A UTF_ERR_ message
    errpos::Int32            ##< Position of invalid character
    errchr::UInt32           ##< Invalid character
end

show(io::IO, exc::UnicodeError) =
    print(io, replace(replace(string("UnicodeError: ",exc.errmsg),
                              "<<1>>",string(exc.errpos)),"<<2>>",hex(exc.errchr)))
end

## Functions to check validity of UTF-8, UTF-16, and UTF-32 encoded strings,
#  and also to return information necessary to convert to other encodings

is_surrogate_lead(c::Unsigned) = ((c & ~0x003ff) == 0xd800)
is_surrogate_trail(c::Unsigned) = ((c & ~0x003ff) == 0xdc00)
is_surrogate_codeunit(c::Unsigned) = ((c & ~0x007ff) == 0xd800)
is_valid_continuation(c) = ((c & 0xc0) == 0x80)

## Return flags for check_string function

const UTF_LONG      =  1  ##< Long encodings are present
const UTF_LATIN1    =  2  ##< characters in range 0x80-0xFF present
const UTF_UNICODE2  =  4  ##< characters in range 0x100-0x7ff present
const UTF_UNICODE3  =  8  ##< characters in range 0x800-0xd7ff, 0xe000-0xffff
const UTF_UNICODE4  = 16  ##< non-BMP characters present
const UTF_SURROGATE = 32  ##< surrogate pairs present
const UTF_INVALID   = 64  ##< surrogate pairs present

# Get a UTF-8 continuation byte, give error if invalid, return updated character value
function check_continuation(dat, pos, ch, flag)
    byt = get_codeunit(dat, pos)
    pos += 1
    if is_valid_continuation(byt)
        flag = false
    elseif !flag
        throw(UnicodeError(UTF_ERR_CONT, pos, byt))
    end
    (ch << 6) | (byt & 0x3f), pos, flag
end

"""
Validates and calculates number of characters in a UTF-8,UTF-16 or UTF-32 encoded vector/string

Warning: this function does not check the bounds of the start or end positions
Use `checkstring` to make sure the bounds are checked

Input Arguments:

* `dat`    UTF-8 (`Vector{UInt8}`), UTF-16 (`Vector{UInt16}`) or UTF-32 (`Vector{UInt32}`, `AbstractString`) encoded string
* `pos`    start position
* `endpos` end position

Keyword Arguments:

* `accept_long_null`  = `true`  # Modified UTF-8 (`\\0` represented as `b\"\\xc0\\x80\"`)
* `accept_surrogates` = `true`  # `CESU-8`
* `accept_long_char`  = `false` # Accept arbitrary long encodings
* `accept_invalids`   = `false` # Accept invalid sequences (to be replaced on conversion)

Returns:

* (total characters, flags, 4-byte, 3-byte, 2-byte)

Throws:

* `UnicodeError`
"""
function unsafe_checkstring end

function unsafe_checkstring(dat::Union{AbstractVector{UInt8}, Ptr{UInt8}}, pos, endpos;
                            accept_long_null  = true,
                            accept_surrogates = true,
                            accept_long_char  = false,
                            accept_invalids   = false)
    local byt::UInt8, ch::UInt32, surr::UInt32
    flags::UInt = 0
    totalchar = latin1byte = num2byte = num3byte = num4byte = invalids = 0
    @inbounds while pos <= endpos
        ch = get_codeunit(dat, pos)
        pos += 1
        totalchar += 1
        if ch > 0x7f
            # Check UTF-8 encoding
            if ch < 0xe0
                # 2-byte UTF-8 sequence (i.e. characters 0x80-0x7ff)
                if pos > endpos
                    accept_invalids || throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
                    invalids += 1
                    break
                end
                ch, pos, flg = check_continuation(dat, pos, ch & 0x3f, accept_invalids)
                flg && (invalids += 1 ; continue)
                if ch > 0xff
                    num2byte += 1
                elseif ch > 0x7f
                    latin1byte += 1
                elseif accept_long_char
                    flags |= UTF_LONG
                elseif (ch == 0) && accept_long_null
                    flags |= UTF_LONG
                elseif accept_invalids
                    invalids += 1
                else
                    throw(UnicodeError(UTF_ERR_LONG, pos, ch))
                end
             elseif ch < 0xf0
                # 3-byte UTF-8 sequence (i.e. characters 0x800-0xffff)
                if pos + 1 > endpos
                    accept_invalids || throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
                    invalids += 1
                    break
                end
                ch, pos, flg = check_continuation(dat, pos, ch & 0x0f, accept_invalids)
                flg && (invalids += 1 ; continue)
                ch, pos, flg = check_continuation(dat, pos, ch, accept_invalids)
                flg && (invalids += 1 ; continue)
                # check for surrogate pairs, make sure correct
                if is_surrogate_codeunit(ch)
                    if !is_surrogate_lead(ch)
                        accept_invalids || throw(UnicodeError(UTF_ERR_NOT_LEAD, pos-2, ch))
                        invalids += 1
                        continue
                    end
                    # next character *must* be a trailing surrogate character
                    if pos + 2 > endpos
                        accept_invalids ||
                            throw(UnicodeError(UTF_ERR_MISSING_SURROGATE, pos-2, ch))
                        invalids += 1
                        break
                    end
                    byt = get_codeunit(dat, pos)
                    pos += 1
                    if byt != 0xed
                        accept_invalids || throw(UnicodeError(UTF_ERR_NOT_TRAIL, pos, byt))
                        invalids += 1
                        continue
                    end
                    surr, pos, flg = check_continuation(dat, pos, 0x0000d, accept_invalids)
                    flg && (invalids += 1 ; continue)
                    surr, pos, flg = check_continuation(dat, pos, surr, accept_invalids)
                    flg && (invalids += 1 ; continue)
                    if !is_surrogate_trail(surr)
                        accept_invalids || throw(UnicodeError(UTF_ERR_NOT_TRAIL, pos-2, surr))
                        invalids += 1
                    elseif !accept_surrogates
                        accept_invalids || throw(UnicodeError(UTF_ERR_SURROGATE, pos-2, surr))
                        invalids += 1
                    else
                        flags |= UTF_SURROGATE
                        num4byte += 1
                    end
                elseif ch > 0x07ff
                    num3byte += 1
                elseif accept_long_char
                    flags |= UTF_LONG
                    num2byte += 1
                elseif accept_invalids
                    invalids += 1
                else
                    throw(UnicodeError(UTF_ERR_LONG, pos-2, ch))
                end
            elseif ch < 0xf5
                # 4-byte UTF-8 sequence (i.e. characters > 0xffff)
                if pos + 2 > endpos
                    accept_invalids || throw(UnicodeError(UTF_ERR_SHORT, pos, ch))
                    invalids += 1
                    break
                end
                ch, pos, flg = check_continuation(dat, pos, ch & 0x07, accept_invalids)
                flg && (invalids += 1 ; continue)
                ch, pos, flg = check_continuation(dat, pos, ch, accept_invalids)
                flg && (invalids += 1 ; continue)
                ch, pos, flg = check_continuation(dat, pos, ch, accept_invalids)
                flg && (invalids += 1 ; continue)
                if ch > 0x10ffff
                    accept_invalids || throw(UnicodeError(UTF_ERR_INVALID, pos-3, ch))
                    invalids += 1
                elseif ch > 0xffff
                    num4byte += 1
                elseif is_surrogate_codeunit(ch)
                    accept_invalids || throw(UnicodeError(UTF_ERR_SURROGATE, pos-3, ch))
                    invalids += 1
                elseif accept_long_char
                    # This is an overly long encoded character
                    flags |= UTF_LONG
                    if ch > 0x7ff
                        num3byte += 1
                    elseif ch > 0x7f
                        num2byte += 1
                    end
                elseif accept_invalids
                    invalids += 1
                else
                    throw(UnicodeError(UTF_ERR_LONG, pos-2, ch))
                end
            elseif accept_invalids
                invalids += 1
            else
                throw(UnicodeError(UTF_ERR_INVALID, pos, ch))
            end
        end
    end
    latin1byte != 0 && (flags |= UTF_LATIN1)
    num2byte != 0 && (flags |= UTF_UNICODE2)
    num3byte != 0 && (flags |= UTF_UNICODE3)
    num4byte != 0 && (flags |= UTF_UNICODE4)
    invalids != 0 && (flags |= UTF_INVALIDS)
    totalchar, flags, num4byte, num3byte, num2byte + latin1byte, invalids
end

const AbstractString1632{Tel<:Union{UInt16,UInt32}} =
    Union{AbstractVector{Tel}, AbstractString, Ptr{Tel}}

function unsafe_checkstring(dat::AbstractString1632, pos, endpos;
                            accept_long_null  = true,
                            accept_surrogates = true,
                            accept_long_char  = false,
                            accept_invalids   = false)
    local ch::UInt32
    flags::UInt = 0
    totalchar = latin1byte = num2byte = num3byte = num4byte = invalids = 0
    @inbounds while pos <= endpos
        ch = get_codeunit(dat, pos)
        pos += 1
        totalchar += 1
        if ch > 0x7f
            if ch < 0x100
                latin1byte += 1
            elseif ch < 0x800
                num2byte += 1
            elseif ch > 0x0ffff
                if (ch > 0x10ffff)
                    accept_invalids || throw(UnicodeError(UTF_ERR_INVALID, pos, ch))
                    invalids += 1
                else
                    num4byte += 1
                end
            elseif !is_surrogate_codeunit(ch)
                num3byte += 1
            elseif is_surrogate_lead(ch)
                if pos > endpos
                    accept_invalids || throw(UnicodeError(UTF_ERR_MISSING_SURROGATE, pos, ch))
                    invalids += 1
                    break
                end
                # next character *must* be a trailing surrogate character
                ch = get_codeunit(dat, pos)
                pos += 1
                if !is_surrogate_trail(ch)
                    accept_invalids || throw(UnicodeError(UTF_ERR_NOT_TRAIL, pos, ch))
                    invalids += 1
                elseif typeof(dat) <: AbstractVector{UInt16} # fix this test!
                    num4byte += 1
                elseif accept_surrogates
                    flags |= UTF_SURROGATE
                    num4byte += 1
                elseif accept_invalids
                    invalids += 1
                else
                    throw(UnicodeError(UTF_ERR_SURROGATE, pos, ch))
                end
            elseif accept_invalids
                invalids += 1
            else
                throw(UnicodeError(UTF_ERR_NOT_LEAD, pos, ch))
            end
        end
    end
    latin1byte != 0 && (flags |= UTF_LATIN1)
    num2byte != 0 && (flags |= UTF_UNICODE2)
    num3byte != 0 && (flags |= UTF_UNICODE3)
    num4byte != 0 && (flags |= UTF_UNICODE4)
    invalids != 0 && (flags |= UTF_INVALIDS)
    return totalchar, flags, num4byte, num3byte, num2byte + latin1byte, invalids
end

"""
Validates and calculates number of characters in a UTF-8,UTF-16 or UTF-32 encoded vector/string

This function checks the bounds of the start and end positions
Use `unsafe_checkstring` to avoid that overhead if the bounds have already been checked

Input Arguments:

* `dat`    UTF-8 (`Vector{UInt8}`), UTF-16 (`Vector{UInt16}`) or UTF-32 (`Vector{UInt32}`, `AbstractString`) encoded string

Optional Input Arguments:

* `startpos` start position (defaults to 1)
* `endpos`   end position   (defaults to `endof(dat)`)

Keyword Arguments:

* `accept_long_null`  = `true`  # Modified UTF-8 (`\\0` represented as `b\"\\xc0\\x80\"`)
* `accept_surrogates` = `true`  # `CESU-8`
* `accept_long_char`  = `false` # Accept arbitrary long encodings

Returns:

* (total characters, flags, 4-byte, 3-byte, 2-byte)

Throws:

* `UnicodeError`
"""
function checkstring end

# No need to check bounds if using defaults
checkstring(dat; kwargs...) = unsafe_checkstring(dat, 1, endof(dat); kwargs...)

# Make sure that beginning and end positions are bounds checked
function checkstring(dat, startpos, endpos = endof(dat); kwargs...)
    checkbounds(dat, startpos)
    checkbounds(dat, endpos)
    endpos < startpos &&
        throw(ArgumentError("End position ($endpos) is less than start position ($startpos)"))
    unsafe_checkstring(dat, startpos, endpos; kwargs...)
end

byte_string_classify(data::Vector{UInt8}) =
    ccall(:u8_isvalid, Int32, (Ptr{UInt8}, Int), data, length(data))
byte_string_classify(s::ByteStr) = byte_string_classify(_data(s))
    # 0: neither valid ASCII nor UTF-8
    # 1: valid ASCII
    # 2: valid UTF-8

isvalid(::Type{ASCIIStr}, s::Vector{UInt8}) = byte_string_classify(s) == 1
isvalid(::Type{UTF8Str}, s::Vector{UInt8}) = byte_string_classify(s) != 0
isvalid(::Type{LatinStr}, s::Vector{UInt8}) = true

bytestring() = ASCIIStr("")

function bytestring(s::AbstractString...)
    str = Base.print_to_string(s...)
    data = Vector{UInt8}(str)
    # This needs to validate the Unicode also
    isvalid(ASCIIStr, data) ? ASCIIStr(data) : UTF8Str(data)
end
bytestring(s::Vector{UInt8}) = bytestring(String(s))
bytestring(p::Union{Ptr{UInt8}, Ptr{Int8}, Cstring}) = unsafe_string(p)
bytestring(p::Union{Ptr{UInt8}, Ptr{Int8}}, len::Integer) = unsafe_string(p,len)
