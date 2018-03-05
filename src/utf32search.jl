#=
Optimized search functions for UTF32Str

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

@inline function _fwd_srch_codeunit(beg::Ptr{T}, cu::T, pos, len) where {T<:UInt32}
    @static if sizeof(Cwchar_t) == sizeof(T)
        pnt = _fwd_memchr(bytoff(beg, pos - 1), cu, len - pos + 1)
        pnt == C_NULL ? 0 : chrdiff(pnt, beg) + 1
    else
        fin = bytoff(beg, len)
        pnt = bytoff(beg, pos - 1)
        while pnt < fin
            get_codeunit(pnt) == ch && return chrdiff(pnt, beg) + 1
            pnt += sizeof(T)
        end
        0
    end
end

@inline function _rev_srch_codeunit(beg::Ptr{T}, ch::T, pos) where {T<:UInt32}
    pnt = bytoff(beg, pos)
    while (pnt -= sizeof(T)) >= beg && get_codeunit(pnt) != ch ; end
    chrdiff(pnt, beg) + 1
end
