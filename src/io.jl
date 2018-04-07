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

@inline _fastwrite(io, str) =
    @preserve str unsafe_write(io, pointer(str), reinterpret(UInt, sizeof(str)))

## outputting Str strings and Chr characters ##

@inline _write(::Type{<:CSE}, io, ch::AbsChar)     = write(io, codepoint(ch))
@inline _write(::Type{UTF8CSE}, io, ch::AbsChar)   = write_utf8(io, codepoint(ch))
@inline _write(::Type{UTF16CSE}, io, ch::AbsChar) = write_utf16(io, codepoint(ch))

@inline write(io::IO, ch::Chr) = write(io, codepoint(ch))

# Todo: make this more common with print code

@inline _write(::Type{C}, io, str::MaybeSub{<:Str{C}}) where {C<:CSE} =
    _fastwrite(io, str)

@inline _write(::Type{LatinCSE}, io,
               str::MaybeSub{<:Str{<:Union{ASCIICSE,Binary_CSEs,_LatinCSE}}}) =
    _fastwrite(io, str)
@inline _write(::Type{_LatinCSE}, io,
               str::MaybeSub{<:Str{<:Union{ASCIICSE,Binary_CSEs,LatinCSE}}}) =
    _fastwrite(io, str)

@inline _write(::Type{UCS2CSE}, io, str::MaybeSub{<:Str{<:Union{_UCS2CSE,Text2CSE}}}) =
    _fastwrite(io, str)
@inline _write(::Type{_UCS2CSE}, io, str::MaybeSub{<:Str{<:Union{UCS2CSE,Text2CSE}}}) =
    _fastwrite(io, str)

@inline _write(::Type{UTF32CSE}, io, str::MaybeSub{<:Str{<:Union{_UTF32CSE,Text4CSE}}}) =
    _fastwrite(io, str)
@inline _write(::Type{_UTF32CSE}, io, str::MaybeSub{<:Str{<:Union{UTF32CSE,Text4CSE}}}) =
    _fastwrite(io, str)

@inline _write(::Type{UTF8CSE}, io, str::MaybeSub{<:Str{ASCIICSE}}) =
    _fastwrite(io, str)
@inline _write(::Type{UTF16CSE}, io, str::MaybeSub{<:Str{<:UCS2_CSEs}}) =
    _fastwrite(io, str)

function _write(::Type{UTF8CSE}, io, str::MaybeSub{<:Str{<:Union{Latin_CSEs}}})
    @preserve str begin
        pnt = pointer(str)
        fin = pnt + sizeof(str)
        # Skip and write out ASCII sequences together
        cnt = 0
        while pnt < fin
            # Skip to first non-ASCII sequence
            # Todo: Optimize this to look at chunks at a time to find first non-ASCII
            beg = pnt
            ch = 0x00
            while (ch = get_codeunit(pnt)) < 0x80 && (pnt += 1) < fin ; end
            # Now we have from beg to < pnt that are ASCII
            unsafe_write(io, beg, pnt - beg)
            cnt += (pnt - beg)
            pnt < fin || break
            # Todo: Optimize sequences of more than one character > 0x7f
            # Write out two bytes of Latin1 character encoded as UTF-8
            _write_utf8_2(io, ch)
            cnt += 2
            pnt += 1
        end
        cnt
    end
end

function _write(::Type{UTF8CSE}, io, str::MaybeSub{<:Str{<:UCS2_CSEs}})
    @preserve str begin
        pnt = pointer(str)
        fin = pnt + sizeof(str)
        cnt = 0
        while pnt < fin
            ch = get_codeunit(pnt)
            pnt += 2
            if ch <= 0x7f
                write(io, ch%UInt8)
                cnt += 1
            elseif ch <= 0x7ff
                _write_utf8_2(io, ch)
                cnt += 2
            else
                _write_utf8_3(io, ch)
                cnt += 3
            end
        end
        cnt
    end
end

function _write(::Type{UTF8CSE}, io, str::MaybeSub{<:Str{UTF16CSE}})
    @preserve str begin
        pnt = pointer(str)
        # Skip and write out ASCII sequences together
        fin = pnt + sizeof(str)
        cnt = 0
        while pnt < fin
            ch = get_codeunit(pnt)
            # Handle 0x80-0x7ff
            if ch <= 0x7f
                write(io, ch%UInt8)
                cnt += 1
            elseif ch <= 0x7ff
                _write_utf8_2(io, ch)
                cnt += 2
            elseif is_surrogate_lead(ch)
                _write_utf8_4(io, get_supplementary(ch, get_codeunit(pnt += 2)))
                cnt += 4
            else
                _write_utf8_3(io, ch)
                cnt += 3
            end
            pnt += 2
        end
        cnt
    end
end


function _write(::Type{UTF8CSE}, io, str::MaybeSub{<:Str{<:UTF32_CSEs}})
    @preserve str begin
        pnt = pointer(str)
        fin = pnt + sizeof(str)
        cnt = 0
        while pnt < fin
            ch = get_codeunit(pnt)
            # Handle 0x80-0x7ff
            if ch <= 0x7f
                write(io, ch%UInt8)
                cnt += 1
            elseif ch <= 0x7ff
                _write_utf8_2(io, ch)
                cnt += 2
            elseif ch <= 0xffff
                _write_utf8_3(io, ch)
                cnt += 3
            else
                _write_utf8_4(io, ch)
                cnt += 4
            end
            pnt += 4
        end
        cnt
    end
end

function _write(::Type{UTF16CSE}, io, str::MaybeSub{<:Str{<:CSE}})
    cnt = 0
    @inbounds for ch in str
        cnt += write_utf16(io, ch)
    end
    cnt
end

function _write(::Type{T}, io, str::MaybeSub{<:Str{C}}
                ) where {T<:Union{UTF16CSE,UCS2_CSEs,UTF32_CSEs,Text2CSE,Text4CSE},
                         C<:Union{ASCIICSE,Latin_CSEs,Binary_CSEs}}
    cnt = 0
    @preserve str begin
        pnt = pointer(str)
        fin = pnt + sizeof(str)
        while pnt < fin
            cnt += write(io, get_codeunit(pnt)%codeunit(T))
            pnt += sizeof(codeunit(C))
        end
    end
    cnt
end

function _write(::Type{UTF32CSE}, io, str::MaybeSub{<:Str{<:CSE}})
    cnt = 0
    @inbounds for ch in str
        write(io, ch%UInt32)
        cnt += 4
    end
    cnt
end

function _write(::Type{<:UTF32_CSEs}, io, str::MaybeSub{<:Str{C}}
                ) where {C<:Union{UCS2_CSEs,Text2CSE}}
    @preserve str begin
        pnt = pointer(str)
        fin = pnt + sizeof(str)
        while pnt < fin
            write(io, get_codeunit(pnt)%UInt32)
            pnt += sizeof(codeunit(C))
        end
    end
    sizeof(str)*2
end

function _write(::Type{<:UTF32_CSEs}, io,
                str::MaybeSub{<:Str{C}}) where {C<:Union{UTF8CSE,UTF16CSE}}
    cnt = 0
    @preserve str begin
        pnt = pointer(str)
        fin = pnt + sizeof(str)
        while pnt < fin
            ch, pnt = _nextcp(C, pnt)
            write(io, ch)
            cnt += 4
        end
    end
    cnt
end

@inline write(io::IO, str::MaybeSub{<:Str{C}}) where {C<:CSE} = _write(C, io, str)

# Printing bytes
_print(io, ch::UInt8) = ch <= 0x7f ? write(io, ch) : _write_utf8_2(io, ch)
print(io::IO, ch::LatinChars) = (_print(io, ch%UInt8) ; nothing)

print(io::IO, str::MaybeSub{<:Str{<:CSE}}) = (_write(UTF8CSE, io, str) ; nothing)

# optimized methods to avoid iterating over chars
print(io::IO, str::MaybeSub{T}) where {T<:Str{<:Union{Text1CSE,BinaryCSE,ASCIICSE,UTF8CSE}}} =
    (_fastwrite(io, str); nothing)

function sprint(f::Function, ::Type{T}, args...;
                context=nothing, sizehint::Integer=0) where {T<:Union{String, Str}}
    s = IOBuffer(sizehint=sizehint)
    if context !== nothing
        f(IOContext(s, context), args...)
        S = get(context, :type, T)
        S(resize!(s.data, s.size))
    else
        f(s, args...)
        T(resize!(s.data, s.size))
    end
end

read(io::IO, ::Type{C}) where {CS,T,C<:Chr{CS,T}} = C(read(io, T))

read(io::IO, ::Type{T}) where {C<:CSE,T<:Str{C}} = Str(C, read(io, String))

IOBuffer(str::T) where {T<:Str} =
    IOContext(IOBuffer(unsafe_wrap(Vector{UInt8}, str.data)), (:type => T))
IOBuffer(s::SubString{T}) where {T<:Str} =
    IOContext(IOBuffer(view(unsafe_wrap(Vector{UInt8}, s.string.data),
                            s.offset + 1 : s.offset + sizeof(s))), (:type => T))

function _joinio(io, ::Type{C}, strings) where {C}
    @inbounds for str in strings
        _write(C, io, str)
    end
    io
end

function _joinio(io, ::Type{C}, strings, delim::T) where {C,T}
    # Could speed this up in the common case where delim === 
    delbuf = (T <: AbstractString && cse(delim) === C) ? delim : convert(Str{C}, delim)
    @inbounds for str in strings
        _write(C, io, str)
        (num -= 1) == 0 || _write(C, io, delbuf)
    end
    io
end

function _joinio(io, ::Type{C}, strings, delim::T, last::S) where {C,T,S}
    # Could speed this up in the common case where delim ===
    delbuf = (T <: AbstractString && cse(delim) === C) ? delim : convert(Str{C}, delim)
    lastbuff = (S <: AbstractString && cse(last) === C) ? last : convert(Str{C}, last)
    @inbounds for str in strings
        _write(C, io, str)
        (num -= 1) > 1 ? _write(C, io, delbuf) : (num == 0 || _write(C, io, lastbuff))
    end
    io
end

function _joincvt(::Type{C}, strings) where {C}
    (num = length(strings)) < 2 && return num == 0 ? empty_str(C) : convert(Str{C}, strings[1])
    Str(C, String(take!(_joinio(IOBuffer(), C, strings))))
end

function _joincvt(::Type{C}, strings, delim) where {C}
    (num = length(strings)) < 2 && return num == 0 ? empty_str(C) : convert(Str{C}, strings[1])
    Str(C, String(take!(_joinio(IOBuffer(), C, strings, delim))))
end

function _joincvt(::Type{C}, strings, delim, last) where {C}
    (num = length(strings)) < 2 && return num == 0 ? empty_str(C) : convert(Str{C}, strings[1])
    Str(C, String(take!(_joinio(IOBuffer(), C, strings, delim, last))))
end

function _join(::Type{C}, strings) where {C}
    (num = length(strings)) < 2 && return num == 0 ? empty_str(C) : convert(Str{C}, strings[1])
    len = 0
    @inbounds for str in strings
        len += ncodeunits(str)
    end
    buf, pnt = _allocate(codeunit(C), len)
    @inbounds for str in strings
        len = ncodeunits(str)
        _memcpy(pnt, pointer(str), len)
        pnt += len
    end
    Str(C, buf)
end

function _join(::Type{C}, strings, delim::T) where {C,T}
    (num = length(strings)) < 2 && return num == 0 ? empty_str(C) : convert(Str{C}, strings[1])

    @preserve delim begin
        # Could speed this up in the common case where delim === 
        delbuf = (T <: AbstractString && cse(delim) === C) ? delim : convert(Str{C}, delim)
        delpnt = pointer(delbuf)
        dellen = ncodeunits(delbuf)
        len = 0
        @inbounds for str in strings
            len += ncodeunits(str) + dellen
        end
        len -= dellen

        buf, pnt = _allocate(codeunit(C), len)
        @inbounds for str in strings
            len = ncodeunits(str)
            _memcpy(pnt, pointer(str), len)
            pnt += len
            if (num -= 1) != 0
                _memcpy(pnt, delpnt, dellen)
                pnt += dellen
            end
        end
        Str(C, buf)
    end
end

function _join(::Type{C}, strings, delim::T, last::S) where {C,T,S}
    (num = length(strings)) < 2 && return num == 0 ? empty_str(C) : convert(Str{C}, strings[1])

    @preserve delim begin
        # Could speed this up in the common case where delim === 
        delbuf = (T <: AbstractString && cse(delim) === C) ? delim : convert(Str{C}, delim)
        delpnt = pointer(delbuf)
        dellen = ncodeunits(delbuf)
        lastbuff = (S <: AbstractString && cse(last) === C) ? last : convert(Str{C}, last)
        lastpnt = pointer(lastbuff)
        lastlen = ncodeunits(lastbuff)
        len = 0
        @inbounds for str in strings
            len += ncodeunits(str) + dellen
        end
        len = len - 2*dellen + lastlen

        buf, pnt = _allocate(codeunit(C), len)
        @inbounds for str in strings
            len = ncodeunits(str)
            _memcpy(pnt, pointer(str), len)
            pnt += len
            if (num -= 1) > 1
                _memcpy(pnt, delpnt, dellen)
                pnt += dellen
            elseif num != 0
                _memcpy(pnt, lastpnt, lastlen)
                pnt += lastlen
            end
        end
        Str(C, buf)
    end
end

@inline function calc_type(strings)
    C = Union{}
    for str in strings
        C = promote_type(C, cse(str))
    end
    C
end

join(io::IO, strings::AbstractVector{<:MaybeSub{<:Str}}) =
    (_joinio(io, calc_type(strings), strings) ; nothing)
join(io::IO, strings::AbstractVector{<:MaybeSub{<:Str}}, delim) =
    (_joinio(io, calc_type(strings), strings, delim) ; nothing)
join(io::IO, strings::AbstractVector{<:MaybeSub{<:Str}}, delim, last) =
    (_joinio(io, calc_type(strings), strings, delim, last) ; nothing)

join(strings::AbstractVector{<:MaybeSub{<:Str}}) =
    _joincvt(calc_type(strings), strings)
join(strings::AbstractVector{<:MaybeSub{T}}) where {C<:Union{ASCIICSE, Latin_CSEs},T<:Str{C}} =
    _join(C, strings)
join(strings::AbstractVector{<:MaybeSub{T}}) where {C<:Word_CSEs,T<:Str{C}} =
    _join(C, strings)
join(strings::AbstractVector{<:MaybeSub{T}}) where {C<:Quad_CSEs,T<:Str{C}} =
    _join(C, strings)

join(strings::AbstractVector{<:MaybeSub{<:Str}}, delim) =
    _joincvt(_calc_type(strings), strings, delim)
join(strings::AbstractVector{<:MaybeSub{T}},
     delim) where {C<:Union{Text1CSE, BinaryCSE, ASCIICSE, Latin_CSEs},T<:Str{C}} =
    _join(C, strings, delim)
join(strings::AbstractVector{<:MaybeSub{T}}, d) where {C<:Word_CSEs,T<:Str{C}} =
    _join(C, strings, delim)
join(strings::AbstractVector{<:MaybeSub{T}}, d) where {C<:Quad_CSEs,T<:Str{C}} =
    _join(C, strings, delim)

join(strings::AbstractVector{<:MaybeSub{<:Str}}, delim, last) =
    _joincvt(calc_type(strings), strings, delim, last)
join(strings::AbstractVector{<:MaybeSub{T}},
     delim, last) where {C<:Union{Text1CSE, BinaryCSE, ASCIICSE, Latin_CSEs},T<:Str{C}} =
         _join(C, strings, delim, last)
join(strings::AbstractVector{<:MaybeSub{T}},
     delim, last) where {C<:Word_CSEs,T<:Str{C}} =
         _join(C, strings, delim, last)
join(strings::AbstractVector{<:MaybeSub{T}},
     delim, last) where {C<:Quad_CSEs,T<:Str{C}} =
         _join(C, strings, delim, last)
