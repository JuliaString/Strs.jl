#=
Optimized search functions for UTF32Str

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

function search(str::UTF32Strings, ch::UInt32, pos::Integer)
    len, pnt = _lenpnt(str)
    pos == len + 1 && return 0
    @boundscheck 1 <= pos <= len && boundserr(str, pos)
    (ch <= 0x10ffff && !is_surrogate_codeunit(ch)) || return 0
    @inbounds while pos <= len
        get_codeunit(pnt, pos) == ch && return pos
        pos += 1
    end
    0
end

function rsearch(str::Str{CSE_T}, ch::UInt32, pos::Integer) where {CSE_T<:Union{UTF32CSE,_UTF32CSE}}
    len, pnt = _lenpnt(str)
    pos == len + 1 && return 0
    @boundscheck 1 <= pos <= len && boundserr(str, pos)
    (ch <= 0x10ffff && !is_surrogate_codeunit(ch)) || return 0
    @inbounds while pos > 0
        get_codeunit(pnt, pos) == ch && return pos
        pos -= 1
    end
    0
end

