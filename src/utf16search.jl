#=
Optimized search functions for UTF16Str

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#

function _srch_utf16_fwd(beg, ch, pnt, fin)
    lead, trail = get_utf16(ch)
    while pnt < fin
        # This checks for the trailing surrogate, since they are much less like to match
        # (there are only a few leading surrogates for the assigned areas in the non-BMP planes)
        get_codeunit(pnt) == trail && get_codeunit(pnt - 2) == lead &&
            return chrdiff(pnt, beg)
        pnt += sizeof(UInt16)
    end
    0
end

function _srch_utf16_rev(beg, ch, pnt)
    lead, trail = get_utf16(ch)
    beg += sizeof(T) # Need to have room for lead
    while pnt > beg
        get_codeunit(pnt -= 2) == trail && get_codeunit(pnt -= 2) == lead &&
            return chrdiff(pnt, beg) + 1
    end
    0
end

function _srch_cp(::Fwd, str::Str{UTF16CSE}, cp::AbsChar, pos, len)
    beg = _pnt(str)
    ((ch = tobase(cp)) <= 0x0ffff
     ? _srch_codeunit(Fwd(), beg, ch%UInt16, pos, len)
     : _srch_utf16_fwd(beg, ch, bytoff(beg, pos), bytoff(beg, len)))
end

function _srch_cp(::Rev, str::Str{UTF16CSE}, cp::AbsChar, pos, len)
    beg = _pnt(str)
    ((ch = tobase(cp)) <= 0x0ffff
     ? _srch_codeunit(Rev(), beg, ch%UInt16, pos)
     : _srch_utf16_rev(beg, ch, bytoff(beg, pos)))
end
