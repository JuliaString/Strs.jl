#=
Statistics for Unicode character/string types

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md
=#
struct LineCounts
    lines::Int
    chars::Int
    bytes::Int

    LineCounts(lines::Vector) = new(length(lines), sum(length, lines), sum(sizeof, lines))
    LineCounts(l::Integer, c::Integer) = new(Int(l), Int(c), 0)
    LineCounts(l::Integer, c::Integer, b::Integer) = new(Int(l), Int(c), Int(b))
end

LineCounts(old::LineCounts, lines::Vector) = LineCounts(old.lines, old.chars, sum(sizeof, lines))

struct CharTypes
    ascii::Int
    latin::Int
    utf2byte::Int
    ucs2::Int
    utf32::Int
    surr::Int
    invalid::Int
end

struct CharStat
    num::Int
    len::Int
    empty::Int
    minlen::Int
    maxlen::Int
    maxtyp::Int
    chars::CharTypes
    lines::CharTypes
end

maxbit(flags) = sizeof(flags)<<3 - leading_zeros(flags)

function calcstats(lines)
    numlines = length(lines)
    lenlines = sum(length, lines)
    charstat = [0,0,0,0,0,0,0]
    linestat = [0,0,0,0,0,0,0]
    emptylines = 0
    minlen = typemax(Int)
    maxlen = 0
    maxtyp = 0
    for l in lines
        len = length(l)
        minlen = min(minlen, len)
        maxlen = max(maxlen, len)
        flags = 0
        for chr in l
            ch = chr%UInt32
            t = ch <= 0x7f ? 1 :
                ch <= 0xff ? 2 :
                ch <= 0x7ff ? 3 :
                0xd800 <= ch <= 0xdfff ? 6 :
                ch <= 0xffff ? 4 :
                ch <= 0x10ffff ? 5 : 7
            charstat[t] += 1
            flags |= 1<<(t-1)
        end
        if flags == 0
            emptylines += 1
        else
            linestat[maxbit(flags & 0x1f)] += 1
            (flags & (1<<5)) != 0 && (linestat[6] += 1)
            (flags & (1<<6)) != 0 && (linestat[7] += 1)
        end
        maxtyp |= flags
    end
    CharStat(numlines, lenlines, emptylines, minlen, maxlen, maxbit(maxtyp & 0x1f),
             CharTypes(charstat...), CharTypes(linestat...))
end
