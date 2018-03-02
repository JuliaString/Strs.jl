#=
Optimized search functions for UTF32Str

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

@inline function _fwd_srch_codeunit(beg::Ptr{UInt32}, cu::UInt32, pos, len)
    @static if sizeof(Cwchar_t) == 4
        pnt = _fwd_memchr(beg + (pos - 1)<<2, cu, len - pos + 1)
        pnt == C_NULL ? 0 : Int((pnt - beg)>>2 + 1)
    else
        fin = beg + (len << 2)
        pnt = beg + (pos - 1) << 2
        while pnt < fin
            get_codeunit(pnt) == ch && return Int((pnt - beg)>>2 + 1)
            pnt += sizeof(UInt32)
        end
        0
    end
end

@inline function _rev_srch_codeunit(beg::Ptr{UInt32}, ch::UInt32, pos)
    pnt = beg + (pos << 2)
    while (pnt -= sizeof(UInt32)) >= beg && get_codeunit(pnt) != ch ; end
    Int((pnt + 4 - beg)>>2)
end
