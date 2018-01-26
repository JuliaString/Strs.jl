#=
Benchmarking routines for characters and strings

Copyright 2017 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md

THIS IS STILL VERY WIP AND HARDCODED!

The variable userdir controls where to save different files downloaded from the internet,
and store the renamed copies (stripped of ASCII lines) for later processing
(I may change that part so that the it is done automatically, and the files are just renamed,
or the files to be processed are picked up from a configuration file)

books = load_books()     # This downloads and processes the books in the gutenbergbooks list
save_books(books)        # This saves them in an output directory (samples) for later benchmarking
tst = checktests()       # Runs a bunch of tests, compares results to the results from String
res = benchdir()         # Runs the benchmark functions on all the documents found in the directory
save_results(fname, res) # Saves the results to the given file

res = load_results(fname) # Loads the results from the given file
dispbench(res)            # Displays the results in a pretty format
=#

using BenchmarkTools

const test_legacy = false

@static test_legacy && (using LegacyStrings)

using StringLiterals
using Strs
import Strs: LineCounts, CharTypes, CharStat, calcstats
import Strs: _LatinStr, _UCS2Str, _UTF32Str, _LatinChr

uninit(T, len)  = @static VERSION < v"0.7.0-DEV" ? T(len) : T(uninitialized, len)
create_vector(T, len) = uninit(Vector{T}, len)

import Base: show

const userdir = "/Users/scott/"

const dpath   = joinpath(userdir, "textsamples")
const gutpath = joinpath(userdir, "gutenberg")
const outpath = joinpath(userdir, "samples")
const defsampledir = outpath

const gutenbergbooks =
    (("files/2600/2600-0",        "English"), # War & Peace, some other languages in quotes
     ("files/1400/1400-0",        "English"), # Great Expectations, uses Unicode quotes
     ("files/42286/42286-0",      "Hungarian"),
     ("files/32941/32941-0",      "Japanese"),
     ("files/24264/24264-0",      "Chinese"),
     ("files/40687/40687-0",      "Telugu"), # Third most spoken in India, official
     ("files/50513/50513-0",      "French"),
#     ("files/43007/43007-0",      "Arabic"),
     ("cache/epub/38496/pg38496", "Portuguese"),
     ("cache/epub/2000/pg2000",   "Spanish"), # Don Quijote
     ("cache/epub/48750/pg48750", "Swedish"),
     ("cache/epub/48322/pg48322", "German"),
     )

const downloadedbooks =
    (("LYSAIa GORA DIeVICh'Ia - SIeRGIeI GOLOVAChIoV.txt", "Russian"),
     )

#Load books from Project Gutenberg site, removing lines added at beginning and end that
# are not part of the book, as much as possible
function load_gutenberg!(books, list, dict)
    for (nam, lang) in list
        cnt = get(dict, lang, 0)
        dict[lang] = cnt + 1
        outnam = cnt == 0 ? "$lang.txt" : "$lang-$cnt.txt"
        lname = joinpath(gutpath, outnam)
        download(joinpath("http://www.gutenberg.org/", nam * ".txt"), lname)
        println("Saved to: ", lname)
        lines = readlines(lname)
        # Eliminate initial lines, empty lines, trailing lines
        pos = 0
        len = length(lines)
        while (pos += 1) <= len
            l = lines[pos]
            sizeof(l) > 41 && startswith(l, "***") && endswith(l, "***") &&
                contains(l, "START OF TH") && contains(l, " PROJECT GUTENBERG EBOOK") && break
        end
        out = Vector{String}()
        while (pos += 1) <= len
            l = lines[pos]
            sizeof(l) > 41 && startswith(l, "***") && endswith(l, "***") &&
                contains(l, "END OF TH") && contains(l, " PROJECT GUTENBERG EBOOK") && break
            isempty(l) || push!(out, l)
        end
        push!(books, (outnam, out))
    end
    books
end

function load_books()
    dict = Dict{String,Int}()
    books = Vector{Tuple{String, Vector{String}}}()
    for (nam, lang) in downloadedbooks
        cnt = get(dict, lang, 0)
        dict[lang] = cnt + 1
        outnam = cnt == 0 ? "$lang.txt" : "$lang-$cnt.txt"
        lines = readlines(joinpath(dpath, nam))
        # Eliminate empty lines
        pos = 0
        len = length(lines)
        out = Vector{String}()
        while (pos += 1) <= len
            l = lines[pos]
            isempty(l) || push!(out, l)
        end
        push!(books, (outnam, out))
    end
    load_gutenberg!(books, gutenbergbooks, dict)
end

function save_books(books)
    for (nam, book) in books
        outnam = joinpath(outpath, nam)
        open(outnam, "w") do io
            for lin in book
                println(io, lin)
            end
            println("Saved $nam")
        end
    end
end

show(io::IO, cnt::LineCounts) =
    pr"\(io)\%10d(cnt.bytes)\%12.3f(cnt.bytes/cnt.chars)"

function show(io::IO, s::CharTypes)
    pr"\(io)\%10d(s.ascii)\%10d(s.latin)\%10d(s.utf2byte)\%10d(s.ucs2)"
    pr"\(io)\%10d(s.utf32)\%10d(s.surr)\%10d(s.invalid)"
end

function show(io::IO, v::Tuple{String,CharStat})
    s = v[2]
    pr_ul(io, "File name              Lines     Chars   Avg C/L     Empty")
    pr_ul(io, "       Min       Max   MaxType\n")
    pr"\(io)\%-18s(v[1])\%10d(s.num)\%10d(s.len)\%10.3f(s.len/s.num)\%10d(s.empty)"
    pr"\(io)\%10d(s.minlen)\%10d(s.maxlen)\%10s(string(enctyp(s.maxtyp)))\n\n"
    pr_ul(io, "Character Types:       ")
    pr_ul(io, "ASCII    Latin1    2-Byte      UCS2     UTF32 Surrogate   Invalid\n")
    pr"\(io)Total characters: \(s.chars)\n"
    pr"\(io)Lines with > 0:   \(s.lines)\n"
end

const pwc = print_with_color
pwc(c, l) = pwc(c, STDOUT, l)

pr_ul(io, l) = pwc(:underline, io, l)
pr_ul(l)     = pwc(:underline, l)

print_size_ratio(io, val) =
    pwc(val < .95 ? :green : val > 1.05 ? :red : :normal, io, f"\%6.3f(val)")
print_time_ratio(io, val) =
    pwc(val < .95 ? :red : val > 1.05 ? :green : :normal, io, f"\%10.3f(val)")

print_size_ratio(val) = print_size_ratio(STDOUT, val)
print_time_ratio(val) = print_time_ratio(STDOUT, val)

print_ratio(val) = pwc(val < .95 ? :red : val > 1.05 ? :green : :normal, f"\%12.3f(val)")
print_ratio_rev(val) = pwc(val < .95 ? :green : val > 1.05 ? :red : :normal, f"\%12.3f(val)")

function dispres(io, xres)
    # (fname, stats, sizes, res)
    (fname, stats, sizes, res) = xres
    show(io, (fname, stats))
    numchars = stats.len
    maxlen = 0
    pos = 0
    for i = 1:length(res)
        len = length(res[i][3])
        len > maxlen && (maxlen = len; pos = i)
    end
    rs = res[pos][3]
    nam1 = []
    nam2 = []
    for result in rs
        names = split(result[1], '\n')
        push!(nam1, names[1])
        push!(nam2, length(names) > 1 ? names[2] : "")
    end
    pr"\(io)\nType        B/Char"
    for nam in nam1 ; pr"\(io)\%10s(nam)" ; end
    pr_ul(io, "\n                  ")
    for nam in nam2 ; pr_ul(io, f"\%10s(nam)") ; end
    pr"\nResults:          "
    for results in rs
        pr"\(io)\%10d(results[2])"
    end
    r1 = res[1]
    t1 = r1[3]
    pr"\(io)\n\n\%-12.12s(r1[1])\%6.3f(sizes[1]/numchars)"
    for tim in t1
        pr"\(io)\%10.3f(tim[3]/numchars)"
    end
    for i = 2:length(res)
        rn = res[i]
        pr"\(io)\n\%-12.12s(rn[1])\%6.3f(sizes[i]/numchars)"
        #print_size_ratio(sizes[i]/numchars)
        tn = rn[3]
        minres = min(length(t1), length(tn))
        for i = 1:minres
            print_time_ratio(io, t1[i][3]/tn[i][3])
        end
        for i = minres+1:length(tn)
            pwc(:yellow, io, f"\%10.3f(tn[i][3]/numchars)")
        end
    end
    println(io)
end

@static VERSION < v"0.7.0-DEV" && (Base.repeat(x::Char, cnt::Int) = x^cnt)
const divline = string(repeat('#', 100),'\n','\f')

function dispbench(io, totres)
    for res in totres[1]
        dispres(io, res)
        print(io, divline)
    end
end

dispbench(totres) = dispbench(STDOUT, totres)

function countlength(lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        cnt += length(text)
    end
    cnt
end

function dolowercase(lines::Vector{<:AbstractString})
    cnt = 0
    for (i, text) in enumerate(lines)
        val = lowercase(text)
        cnt += (val == text)
    end
    cnt
end

function douppercase(lines::Vector{<:AbstractString})
    cnt = 0
    for (i, text) in enumerate(lines)
        val = uppercase(text)
        cnt += (val == text)
    end
    cnt
end

function iteratechars(text::AbstractString)
    cnt = 0
    for ch in text
        cnt += '0' <= ch <= '9'
    end
    cnt
end

function iteratechars(text::Str)
    cnt = 0
    @inbounds for i=1:length(text)
        cnt += '0' <= text[i] <= '9'
    end
    cnt
end

function iteratecps(text::AbstractString)
    cnt = 0
    for ch in codepoints(text)
        cnt += '0' <= ch <= '9'
    end
    cnt
end

function iteratecus(text::AbstractString)
    cnt = 0
    for ch in codeunits(text)
        cnt += '0' <= ch <= '9'
    end
    cnt
end

function countchars(lines::Vector{T}) where {T<:AbstractString}
    cnt = 0
    for text in lines
        cnt += iteratechars(text)
    end
    cnt
end

function countcps(lines::Vector{T}) where {T<:AbstractString}
    cnt = 0
    for text in lines
        cnt += iteratecps(text)
    end
    cnt
end

function countsize(lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        cnt += sizeof(text)
    end
    cnt
end

function countcodeunits(text::AbstractString)
    cnt = 0
    @inbounds for cu in codeunits(text)
        cnt += cu
    end
    cnt
end

function countsize2(lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        cnt += countcodeunits(text)
    end
    cnt
end

function checkstr(fun, lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        cnt += fun(text)
    end
    cnt
end

function checktext(fun, lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        isempty(text) || fun(text)
        cnt += 1
    end
    cnt
end

function iteratenextind(text)
    cnt = 0
    len = ncodeunits(text)
    pos = 1
    while pos <= len
        pos = nextind(text, pos)
        cnt += 1
    end
    cnt
end

function iteratefunchars(fun, text)
    cnt = 0
    for ch in text
        cnt += fun(ch)
    end
    cnt
end

function iteratefuncps(fun, text)
    cnt = 0
    for ch in codepoints(text)
        cnt += fun(ch)
    end
    cnt
end

function iteratefuncus(fun, text)
    cnt = 0
    for ch in codeunits(text)
        cnt += fun(ch)
    end
    cnt
end

function checkchars(fun, lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        cnt += iteratefunchars(fun, text)
    end
    cnt
end

function checkcp(fun, lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        cnt += iteratefuncps(fun, text)
    end
    cnt
end

function sumchars(text)
    t = 0
    for ch in text
        t += UInt32(ch)
    end
    t
end
function sumcp(text)
    t = 0
    for ch in codepoints(text)
        t += UInt32(ch)
    end
    t
end
function sumcu(text)
    t = 0
    for ch in codeunits(text)
        t += UInt32(ch)
    end
    t
end

function sumcharvals(lines::Vector{<:AbstractString})
    t = 0
    for text in lines
        t += sumchars(text)
    end
    t
end
function sumcodepnts(lines::Vector{<:AbstractString})
    t = 0
    for text in lines
        t += sumcp(text)
    end
    t
end
function sumcodeunits(lines::Vector{<:AbstractString})
    t = 0
    for text in lines
        t += sumcu(text)
    end
    t
end

searchres(x::UnitRange) = x.start
searchres(x::Int) = x

function searchlines(lines, v)
    t = 0
    for text in lines
        t += searchres(search(text, v))
    end
    t
end

function searchlines(lines::Vector{UniStr}, v)
    t = 0
    for text in lines
        if typeof(text) == ASCIIStr
            t += searchres(search(text::ASCIIStr, v))
        elseif typeof(text) == _LatinStr
            t += searchres(search(text::_LatinStr, v))
        elseif typeof(text) == _UCS2Str
            t += searchres(search(text::_UCS2Str, v))
        else
            t += searchres(search(text::_UTF32Str, v))
        end
    end
    t
end

# maybe change all this to be table driven and use @eval!

searchchar(lines::Vector{String}) =
    searchlines(lines, '\ufffd')
searchchar(lines::Vector{T}) where {T} =
    searchlines(lines, codepoint(T)('\ufffd'))
searchchar(lines::Vector{T}) where {T<:Union{ASCIIStr,LatinStr,_LatinStr}} =
    searchlines(lines, ASCIIChr(0x1a))

searchstr(lines::Vector{String})      = searchlines(lines, "thy")
searchstr(lines::Vector{T}) where {T} = searchlines(lines, T("thy"))

# normalize
# textwidth
# isassigned

sklength(str) = length(str)
@inline function _length(s, i::Int, n::Int, c::Int)
    i < n || return c
    @inbounds b = codeunit(s, i)
    @inbounds while true
        while true
            (i += 1) ≤ n || return c
            0xc0 ≤ b ≤ 0xf7 && break
            b = codeunit(s, i)
        end
        l = b
        b = codeunit(s, i) # cont byte 1
        c -= (x = b & 0xc0 == 0x80)
        x & (l ≥ 0xe0) || continue

        (i += 1) ≤ n || return c
        b = codeunit(s, i) # cont byte 2
        c -= (x = b & 0xc0 == 0x80)
        x & (l ≥ 0xf0) || continue

        (i += 1) ≤ n || return c
        b = codeunit(s, i) # cont byte 3
        c -= (b & 0xc0 == 0x80)
    end
end
sklength(s::T) where {T<:AbstractString} = _length(s, 1, ncodeunits(s), ncodeunits(s))

_is_surrogate_trail(c::Unsigned) = ((c & ~0x003ff) == 0xdc00)
_is_valid_continuation(c) = ((c & 0xc0) == 0x80)

function oldlength(str::T) where {T<:AbstractString}
    dat = Vector{UInt8}(str)
    cnt = 0
    for i = 1:sizeof(dat)
        @inbounds cnt += !_is_valid_continuation(dat[i])
    end
    cnt
end
function oldlength(s::UTF16Str)
    dat = reinterpret(UInt16, s.data)
    len = length(dat)
    len == 0 && return 0
    cnt = 0
    for i = 1:len
        @inbounds cnt += !_is_surrogate_trail(dat[i])
    end
    cnt
end

repeat1(str)  = repeat(str, 1)
repeat10(str) = repeat(str, 10)
repeat1c(str)  = repeat(str[1], 1)
repeat10c(str) = repeat(str[1], 10)

countsklength(l)  = checkstr(sklength, l)
countoldlength(l) = checkstr(oldlength, l)

checkrepeat1(l)   = checktext(repeat1, l)
checkrepeat10(l)  = checktext(repeat10, l)
checkrepeat1c(l)  = checktext(repeat1c, l)
checkrepeat10c(l) = checktext(repeat10c, l)
checkreverse(l)   = checktext(reverse, l)

checknextind(l) = checkstr(iteratenextind, l)
countchars(l)   = checkstr(iteratechars, l)
countcps(l)     = checkstr(iteratecps, l)
countcus(l)     = checkstr(iteratecus, l)

validstr(l)     = checkstr(isvalid, l)
asciistr(l)     = checkstr(isascii, l)

validchars(l)   = checkchars(isvalid, l)
asciichars(l)   = checkchars(isascii, l)

checkvalid(l)   = checkcp(isvalid,   l)
checkascii(l)   = checkcp(isascii,   l)
checkcntrl(l)   = checkcp(iscntrl,   l)
checkdigit(l)   = checkcp(isdigit,   l)
checkxdigit(l)  = checkcp(isxdigit,  l)
checklower(l)   = checkcp(islower,   l)
checkupper(l)   = checkcp(isupper,   l)
checkalpha(l)   = checkcp(isalpha,   l)
checknumeric(l) = checkcp(isnumeric, l)
checkspace(l)   = checkcp(isspace,   l)
checkalnum(l)   = checkcp(isalnum,   l)
checkprint(l)   = checkcp(isprint,   l)
checkpunct(l)   = checkcp(ispunct,   l)
checkgraph(l)   = checkcp(isgraph,   l)

function wrap(f, lines, io, cnts::LineCounts, t, msg, basetime=0%UInt)
    tst = ""
    try
        res = f(lines)
        raw = @belapsed ($f)($lines)
        tim = round(UInt64, raw*1e9)
        push!(t, (msg, res, tim))
        pr"\(io)\%-22s(replace(msg, '\n' => ' ')*':') \%12d(res)"
        pr"\(io) \%12.3f(tim/1000000)"
        pr"\(io) \%12.3f(tim/cnts.lines)"
        pr"\(io) \%12.3f(tim/cnts.chars)"
        pr"\(io) \%12.3f(tim/cnts.bytes)"
        basetime != 0 && print_ratio(basetime/tim)
        pr"\(io)\n"
    catch ex
        typeof(ex) == InterruptException && rethrow()
        push!(t, (msg, 0, 0%UInt))
        println(io, sprint(showerror, ex, catch_backtrace()))
    end
end

# For each set of lines: save number of characters (should be the same for all)
# For each test (currently just counting via Char iteration, length)

const tests =
    (
     (countsize,   "sizeof"),
     (countlength, "length"),
     (checknextind, "nextind\nchars"),
     (checkreverse, "reverse"),
     (checkrepeat1,  "repeat 1\nstring"),
     (checkrepeat10,  "repeat 10\nstring"),
     (checkrepeat1c,  "repeat 1\nchar"),
     (checkrepeat10c,  "repeat 10\nchar"),
#    (countsklength,  "length\nSK"),
#    (countoldlength, "length\nOld"),
     (countchars,   "iteration\nChar"),
     (sumcharvals,  "sum\nchar vals"),
     (asciistr,     "isascii\nstring"),
     (validstr,     "isvalid\nstring"),
     (checkascii,   "isascii\nchars"),
     (checkvalid,   "isvalid\nchars"),
     (checklower,   "islower\nchars"),
     #=
     (checkcntrl,   "iscntrl\nchars"),
     (checkupper,   "isupper\nchars"),
     (checkalpha,   "isalpha\nchars"),
     (checkalnum,   "isalnum\nchars"),
     (checkspace,   "isspace\nchars"),
     (checkprint,   "isprint\nchars"),
     (checkpunct,   "ispunct\nchars"),
     (checkgraph,   "isgraph\nchars"),
     (checkdigit,   "isdigit\nchars"),
     (checkxdigit,  "isxdigit\nchars"),
     =#
     (dolowercase,  "lowercase\nstring"),
     (douppercase,  "uppercase\nstring"),
#    (dotitlecase,  "titlecase\nstring"),
    )

function testperf(lines::Vector{T}, io, cnts, docnam, basetime) where {T<:AbstractString}
    # Test performance
    pr_ul(io, f"""\%-22s(docnam) \%12s("Result") \%12s("ms total") """)
    pr_ul(io, f"""\%12s("ns/line") \%12s("ns/char") \%12s("ns/byte")\n""")
    t = []
    # Just run everything once
    for (tst, nam) in tests
        try
            tst(lines)
        catch ex
            typeof(ex) == InterruptException && rethrow()
            str = f"Failed test \(nam): \(sprint(showerror, ex, catch_backtrace()))"
            println("File: ", docnam, ": ", str)
            return docnam, 0, t, str
        end
    end
    pos = 0
    for (tst, nam) in tests
        wrap(tst, lines, io, cnts, t, nam, basetime == nothing ? 0%UInt : basetime[pos += 1][3])
    end
    #=
    if T != String
        wrap(countsize2,  lines, io, cnts, t, "iteration\ncodeunits")
        #wrap(countcps,    lines, io, cnts, t, "iteration\ncp")
        wrap(sumcodepnts, lines, io, cnts, t, "sum\ncp")
    end
    =#
    pr"\n"
    docnam, cnts.bytes, t, ""
end

function sumsizes1(lines)
    cnt = 0
    for text in lines
        cnt += sizeof(text)
    end
    cnt
end

sumsizes2(lines) = sum(sizeof, lines)

function benchsizes(lines)
    f = sumsizes1
    t1 = @belapsed sumsizes1($lines)
    t2 = @belapsed sumsizes2($lines)
    t3 = @belapsed ($f)($lines)
    t4 = @belapsed sum(sizeof, $lines)
    t1, t2, t3, t4
end

enctyp(t) = t == 1 ? ASCIIStr : t == 2 ? LatinStr : t < 5 ? UCS2Str : UTF32Str
          
function encode_lines(list, lines)
    numlines = length(lines)
    enc = []
    push!(enc, lines)
    for i = 2:length(list)
        T = list[i]
        vec = create_vector(T, numlines)
        @inbounds for j = 1:numlines
            vec[j] = convert(T, lines[j])
        end
        push!(enc, vec)
    end
    enc
end

function checkboolchar(fun, lines)
    res = []
    for text in lines
        len = length(text)
        bv = uninit(BitVector, len)
        for (i, ch) in enumerate(text)
            bv[i] = fun(ch)
        end
        push!(res, bv)
    end
    res
end

function checkline(::Type{Bool}, fun, lines)
    len = length(lines)
    bv = uninit(BitVector, len)
    for (i, text) in enumerate(lines)
        bv[i] = fun(text)
    end
    bv
end

function checkline(::Type{T}, fun, lines) where {T}
    res = T[]
    for text in lines
        push!(res, fun(text))
    end
    res
end

function runcheckline(::Type{T}, lines, list) where {T}
    totresults = []
    for fun in list
        push!(totresults, checkline(T, fun, lines))
    end
    totresults
end

function runcheckchar(lines, list)
    totresults = []
    for fun in list
        push!(totresults, checkboolchar(fun, lines))
    end
    totresults
end

function runcheckcu(lines, list)
    totresults = []
    for fun in list
        res = []
        for text in lines
            push!(res, [fun(cu) for cu in codeunits(text)])
        end
        push!(totresults, res)
    end
    totresults
end

striplist(list) = replace(string(list), "Base.Unicode." => "")
function comparetestline(lines, results, list, displist)
    pr"  Lines:     \(displist)"
    diff = []
    for (i, fun) in enumerate(list)
        fundiff = []
        funres = results[i]
        try
            for (j, text) in enumerate(lines)
                res = fun(text)
                res == funres[j] ||
                    push!(fundiff, typeof(res) == Bool ? (j, text) : (j, text, res, funres[j]))
            end
            isempty(fundiff) || push!(diff, (i, fun, fundiff))
        catch ex
            typeof(ex) == InterruptException && rethrow()
            pr"Failed test \(fun): \(sprint(showerror, ex, catch_backtrace()))"
        end
    end
    if isempty(diff)
        pwc(:green, "\r\u2714\n")
    else
        pwc(:red, "\e[s\rX\e[u")
        io = IOBuffer()
        print(io, " => ", diff)
        str = String(take!(io))
        println(str[1:200])
    end
    diff
end

function comparetestchar(lines, results, list, displist)
    pr"  Chars:     \(displist)"
    diff = []
    for (i, fun) in enumerate(list)
        fundiff = []
        lineres = results[i]
        try
            for (j, text) in enumerate(lines)
                chrdiff = []
                chrres = lineres[j]
                for (k, ch) in enumerate(text)
                    res = fun(ch)
                    res == chrres[k] ||
                        push!(chrdiff, typeof(res) == Bool ? (k, ch) : (k, ch, res, chrres[k]))
                end
                isempty(chrdiff) || push!(fundiff, (j, text, chrdiff))
            end
            isempty(fundiff) || push!(diff, (i, fun, fundiff))
        catch ex
            typeof(ex) == InterruptException && rethrow()
            pr"Failed test \(fun): \(sprint(showerror, ex, catch_backtrace()))"
        end
    end
    if isempty(diff)
        pwc(:green, "\r\u2714\n")
    else
        pwc(:red, "\e[s\rX\e[u")
        io = IOBuffer()
        print(io, " => ", diff)
        str = String(take!(io))
        println(str[1:200])
    end
    diff
end

function comparetestcu(lines, results, list, displist)
    pr"  CodeUnits: \(displist)"
    diff = []
    for (i, fun) in enumerate(list)
        fundiff = []
        lineres = results[i]
        try
            for (j, text) in enumerate(lines)
                chrdiff = []
                chrres = lineres[j]
                for (k, ch) in enumerate(codeunits(text))
                    res = fun(ch)
                    res == chrres[k] ||
                        push!(chrdiff, typeof(res) == Bool ? (k, ch) : (k, ch, res, chrres[k]))
                end
                isempty(chrdiff) || push!(fundiff, (j, text, chrdiff))
            end
            isempty(fundiff) || push!(diff, (i, fun, fundiff))
        catch ex
            typeof(ex) == InterruptException && rethrow()
            pr"Failed test \(fun): \(sprint(showerror, ex, catch_backtrace()))"
        end
    end
    if isempty(diff)
        pwc(:green, "\r\u2714\n")
    else
        pwc(:red, "\e[s\rX\e[u")
        io = IOBuffer()
        print(io, " => ", diff)
        str = String(take!(io))
        println(str[1:200])
    end
    diff
end

const testlist =
    (((length, ), "length"),
     ((isascii, isvalid), "isascii, isvalid"),
     #((lowercase, uppercase, reverse), "lowercase, uppercase, reverse"),
     ((lowercase,), "lowercase,"),
     ((isascii, isvalid, iscntrl, islower, isupper, isalpha,
       isalnum, isspace, isprint, ispunct, isgraph, isdigit, isxdigit),
      "is(ascii|valid|cntrl|lower|upper|alpha|alnum|space|print|punct|graph|digit|xdigit)"),
     ((UInt32, ), "UInt32"),
     ((sizeof, ), "sizeof"))

# Use String to calculate a baseline (note, sizeof needs to be checked separately, as only
# UTF8Str should be the same).

function compareall(io, lines, res)
    pr"\(io)\(eltype(lines))\n"
    (comparetestline(lines, res[1], testlist[1]...),
     comparetestline(lines, res[2], testlist[2]...),
     comparetestline(lines, res[3], testlist[3]...),
     comparetestchar(lines, res[4], testlist[4]...),
     eltype(lines) == UTF8Str ? comparetestcu(lines, res[5], testlist[5]...) : [],
     eltype(lines) == UTF8Str ? comparetestline(lines, res[6], testlist[6]...) : [])
end

function checktests(io = STDOUT, sampledir = defsampledir)
    totres = []
    totcmp = []
    for fname in readdir(sampledir)
        lines = readlines(joinpath(sampledir, fname))
        stats = calcstats(lines)
        list = [String, UTF8Str, UTF16Str, UTF32Str, UniStr]
        MT = enctyp(stats.maxtyp)
        push!(list, MT)
        isdefined(Main, :UTF8String) && push!(list, UTF8String, UTF16String, UTF32String)
        enc = encode_lines(list, lines)
        res = (runcheckline(Int, lines, testlist[1][1]),
               runcheckline(Bool, lines, testlist[2][1]),
               runcheckline(Any, lines, testlist[3][1]),
               runcheckchar(lines, testlist[4][1]),
               runcheckcu(lines,   testlist[5][1]),
               runcheckline(Int, lines, testlist[6][1]))
        push!(totres, (fname, res))
        cmp = []
        pr"\(io)Checking \(fname):\n"
        for i = 2:length(list)
            push!(cmp, compareall(io, enc[i], res))
        end
        push!(totres, (fname, res))
        push!(totcmp, (fname, cmp))
    end
    totres, totcmp
end

function benchdir(io = STDOUT, sampledir = defsampledir)
    totres = []
    totlines = []
    totnames = []
    totsizes = []
    for fname in readdir(sampledir)
        lines = readlines(joinpath(sampledir, fname))
        stats = calcstats(lines)
        show(io, (fname, stats))
        list = [String, UTF8Str, UTF16Str, UTF32Str, UniStr]
        MT = enctyp(stats.maxtyp)
        push!(list, MT)
        isdefined(Main, :UTF8String) && push!(list, UTF8String, UTF16String, UTF32String)
        numchars = stats.len
        numlines = stats.num
        enc = encode_lines(list, lines)
        # Now calculate and display the size statistics for each
        sizes = [sum(sizeof, enclines) for enclines in enc]
        basesize = sizes[1]
        names = String[string(typ) for typ in list]

        pr_ul(io, "\nTypes:      ")
        for nam in names ; pr_ul(io, f"\%12s(nam)") ; end
        pr"\(io)\nBytes:      "
        for siz in sizes ; pr"\(io)\%12d(siz)" ; end
        pr"\(io)\nBytes/Char: "
        for siz in sizes ; pr"\(io)\%12.3f(siz/numchars)" ; end
        pr"\(io)\nRelative:               "
        for siz in sizes[2:end] ; print_ratio_rev(siz/basesize) ; end
        pr"\n\n"

        # Now test the performance for each
        res = create_vector(Any, length(list))
        res[1] = testperf(enc[1], io, LineCounts(numlines, numchars, sizes[1]), names[1], nothing)
        basetime = res[1][3]
        for i = 2:length(list)
            res[i] = testperf(enc[i], io, LineCounts(numlines, numchars, sizes[i]), names[i],
                              basetime)
        end
        push!(totres, (fname, stats, sizes, res))

        push!(totnames, names)
        push!(totsizes, sizes)
        push!(totlines, enc)

        print(divline)
    end
    totres, totlines, totnames, totsizes
end

function save_results(fname, res)
    open(fname, "w") do io
        serialize(io, res)
    end
end

function load_results(fname)
    open(fname) do io
        deserialize(io)
    end
end

