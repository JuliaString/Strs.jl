#=
IO functions for Str and Chr types

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#
@inline _write_utf8_2(io, ch) = write(io, get_utf8_2(ch)...)
@inline _write_utf8_3(io, ch) = write(io, get_utf8_3(ch)...)
@inline _write_utf8_4(io, ch) = write(io, get_utf8_4(ch)...)

@inline _write_ucs2(io, ch) =
    ch <= 0x7f ? write(io, ch%UInt8) : ch <= 0x7ff ? _write_utf8_2(io, ch) : _write_utf8_3(io, ch)

@inline write_utf8(io, ch) = ch <= 0xffff ? _write_ucs2(io, ch) : _write_utf8_4(io, ch)
@inline write_utf16(io, ch) = ch <= 0xffff ? write(io, ch%UInt16) : write(io, get_utf16(ch)...)

@inline print(io::IO, ch::UCS2Chr)  = _write_ucs2(io, codepoint(ch))
@inline print(io::IO, ch::UTF32Chr) = write_utf8(io, codepoint(ch))

## outputting Str strings and Chr characters ##

write(io::IO, ch::Chr) = write(io, codepoint(ch))

write(io::IO, str::MaybeSub{<:Str{<:CSE}}) =
    @preserve str unsafe_write(io, pointer(str), reinterpret(UInt, sizeof(str)))

# optimized methods to avoid iterating over chars
print(io::IO, str::MaybeSub{T}) where {T<:Str{<:Union{Text1CSE,BinaryCSE,ASCIICSE,UTF8CSE}}} =
    (write(io, str); nothing)

## outputting Latin 1 strings ##

function print(io::IO, str::MaybeSub{<:Str{<:Latin_CSEs}})
    @preserve str begin
        pnt = pointer(str)
        fin = pnt + sizeof(str)
        # Skip and write out ASCII sequences together
        while pnt < fin
            # Skip to first non-ASCII sequence
            # Todo: Optimize this to look at chunks at a time to find first non-ASCII
            beg = pnt
            ch = 0x00
            while (ch = get_codeunit(pnt)) < 0x80 && (pnt += 1) < fin ; end
            # Now we have from beg to < pnt that are ASCII
            unsafe_write(io, beg, pnt - beg)
            pnt < fin || break
            # Todo: Optimize sequences of more than one character > 0x7f
            # Write out two bytes of Latin1 character encoded as UTF-8
            _write_utf8_2(io, ch)
            pnt += 1
        end
    end
    nothing
end

_print(io, ch::UInt8) = ch <= 0x7f ? write(io, ch) : _write_utf8_2(io, ch)
print(io::IO, ch::LatinChars) = _print(io, ch%UInt8)

write(io::IO, ch::LatinChars) = write(io, ch%UInt8)

## outputting UCS2 strings as UTF-8 ##

function print(io::IO, str::MaybeSub{<:Str{<:UCS2_CSEs}})
    @preserve str begin
        len = ncodeunits(str)
        pnt = pointer(str)
        fin = bytoff(pnt, len)
        while pnt < fin
            _write_ucs2(io, get_codeunit(pnt))
            pnt += 2
        end
    end
    nothing
end

## output UTF-16 string ##

function print(io::IO, str::MaybeSub{<:Str{UTF16CSE}})
    @preserve str begin
        siz = ncodeunits(str)
        pnt = pointer(str)
        # Skip and write out ASCII sequences together
        fin = pnt + siz
        while pnt < fin
            ch = get_codeunit(pnt)
            # Handle 0x80-0x7ff
            if ch <= 0x7f
                write(io, ch%UInt8)
            elseif ch <= 0x7ff
                _write_utf8_2(io, ch)
            elseif is_surrogate_lead(ch)
                _write_utf8_4(io, get_supplementary(ch, get_codeunit(pnt += 2)))
            else
                _write_utf8_3(io, ch)
            end
            pnt += 2
        end
    end
    nothing
end

## outputting UTF32 strings as UTF-8 ##

function print(io::IO, str::MaybeSub{<:Str{<:UTF32_CSEs}})
    @preserve str begin
        len = ncodeunits(str)
        pnt = pointer(str)
        fin = bytoff(pnt, len)
        while pnt < fin
            write_utf8(io, get_codeunit(pnt))
            pnt += 4
        end
    end
    nothing
end

