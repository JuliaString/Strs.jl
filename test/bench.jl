#=
Benchmarking routines for characters and strings

Copyright 2017-2018 Gandalf Software, Inc., Scott P. Jones
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

isdefined(Main, :STRS_SETUP) || include("setup.jl")
@static V6_COMPAT || (using Serialization)

@static V6_COMPAT || (Base.iterate(it::Union{CodePoints,CodeUnits}) = iterate(it, 1))


const inppath = "textsamples"
const gutpath = "gutenberg"
const smppath = "samples"

const gutenbergbooks =
    (("files/2600/2600-0",        "English"), # War & Peace, some other languages in quotes
     ("files/1400/1400-0",        "English"), # Great Expectations, uses Unicode quotes
     ("files/42286/42286-0",      "Hungarian"),
     #("files/8119/8119-0",        "Polish"), # couldn't get this to load correctly
     ("files/31536/31536-0",      "Polish"),
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

const downloadedbooks = (
#                         ("LYSAIa GORA DIeVICh'Ia - SIeRGIeI GOLOVAChIoV.txt", "Russian"),
                         )

getdefdir(dir)::String = dir === nothing ? homedir() : dir

function filter_lines(lines)
    out = Vector{String}()
    sizehint!(out, length(lines))
    # Eliminate initial lines, empty lines, trailing lines
    checkbeg = true
    for l in lines
        if sizeof(l) > 41 && starts_with(l, "***") && ends_with(l, "***") &&
            occurs_in(" PROJECT GUTENBERG EBOOK", l) &&
            occurs_in(checkbeg ? "START OF TH" : "END OF TH", l)
            checkbeg || break # Found "end of"
            checkbeg = false
        else
            push!(out, l)
        end
    end
    out
end

"""
Load books from Project Gutenberg site, removing lines added at beginning and end that
are not part of the book, as much as possible
"""
function load_gutenberg!(books, list, dict, gutenbergdir)
    mkpath(gutenbergdir)
    for (nam, lang) in list
        cnt = get(dict, lang, 0)
        dict[lang] = cnt + 1
        outnam = cnt == 0 ? "$lang.txt" : "$lang-$cnt.txt"
        lname = joinpath(gutenbergdir, outnam)
        download(joinpath("http://www.gutenberg.org/", nam * ".txt"), lname)
        println("Saved to: ", lname)
        push!(books, (outnam, filter_lines(readlines(lname))))
    end
    books
end

function remove_empty(lines)
    # Eliminate empty lines
    len = length(lines)
    out = Vector{String}()
    sizehint!(out, len)
    for l in lines
        is_empty(l) || push!(out, l)
    end
    out
end

"""
load_books(; dir=nothing)

Loads a set of books from a local directory, and downloads a set of books from Project Gutenberg
Returns them as a dictionary with names -> vectors of strings
"""
function load_books(; dir::Any=nothing)
    defdir = getdefdir(dir)
    inputdir = joinpath(defdir, inppath)
    dict = Dict{String,Int}()
    books = Vector{Tuple{String, Vector{String}}}()
    for (nam, lang) in downloadedbooks
        cnt = get(dict, lang, 0)
        dict[lang] = cnt + 1
        outnam = cnt == 0 ? "$lang.txt" : "$lang-$cnt.txt"
        push!(books, (outnam, readlines(joinpath(inputdir, nam))))
    end
    load_gutenberg!(books, gutenbergbooks, dict, joinpath(defdir, gutpath))
end

"""
save_books(books; dir=nothing)

Saves the collection of downloaded books into the given directory, in a "samples" subdirectory.
If the directory is not set, it will default to the user's home directory
"""
function save_books(books; dir::Any=nothing)
    sampledir = joinpath(getdefdir(dir), smppath)
    mkpath(sampledir)
    for (nam, book) in books
        outnam = joinpath(sampledir, nam)
        open(outnam, "w") do io
            for lin in book
                println(io, lin)
            end
            println("Saved $nam")
        end
    end
end

Base.show(io::IO, cnt::LineCounts) =
    pr"\(io)\%10d(cnt.bytes)\%12.3f(cnt.bytes/cnt.chars)"

function Base.show(io::IO, s::CharTypes)
    pr"\(io)\%10d(s.ascii)\%10d(s.latin)\%10d(s.utf2byte)\%10d(s.ucs2)"
    pr"\(io)\%10d(s.utf32)\%10d(s.surr)\%10d(s.invalid)"
end

function Base.show(io::IO, v::Tuple{String,CharStat})
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

print_size_ratio(io, val) =
    pwc(val < .95 ? :green : val > 1.05 ? :red : :normal, io, f"\%6.3f(val)")
print_time_ratio(io, val) =
    pwc(val < .95 ? :red : val > 1.05 ? :green : :normal, io, f"\%10.3f(val)")

print_size_ratio(val) = print_size_ratio(_stdout(), val)
print_time_ratio(val) = print_time_ratio(_stdout(), val)

print_ratio(val) = pwc(val < .95 ? :red : val > 1.05 ? :green : :normal, f"\%12.3f(val)")
print_ratio_rev(val) = pwc(val < .95 ? :green : val > 1.05 ? :red : :normal, f"\%12.3f(val)")

function display_results(io, xres)
    # (fname, stats, sizes, res)
    (fname, stats, sizes, selstat, selsiz, res) = xres
    show(io, (fname, stats))
    selstat == stats || show(io, (fname, selstat))
    t1 = res[1][3]
    numchars = selstat.len
    pr"\(io)\n\n\%-12.12s(res[1][1])\%6.3f(sizes[1]/stats.len)"
    for rn in res[i]
        pr"\(io)\n\%-12.12s(rn[1])\%6.3f(sizes[i]/stats.len)"
    end
    println(io)
    for i = 1:length(res)
        pr"\(io)\%10.3f(t1[i][3]/numchars)"
        for rn in res[i]
            print_time_ratio(io, t1[i][3]/rn[3])
        end
    end
    println(io)
    # pwc(:yellow, io, f"\%10.3f(rn[3]/numchars)")
end

function dispres(io, xres)
    # (fname, stats, sizes, res)
    (fname, stats, sizes, selstat, selsiz, res) = xres
    show(io, (fname, stats))
    selstat == stats || show(io, (fname, selstat))
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
    numchars = selstat.len
    pr"\(io)\n\n\%-12.12s(r1[1])\%6.3f(sizes[1]/stats.len)"
    for tim in t1
        pr"\(io)\%10.3f(tim[3]/numchars)"
    end
    for i = 2:length(res)
        rn = res[i]
        rn[1] == "UniStr" && continue
        pr"\(io)\n\%-12.12s(rn[1])\%6.3f(sizes[i]/stats.len)"
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

const divline = string(repeat('#', 100),'\n','\f')

function dispbench(io, totres)
    for res in totres[1]
        dispres(io, res)
        print(io, divline)
    end
end

dispbench(totres) = dispbench(_stdout(), totres)

function display_benchmark(io, totres)
    for res in totres[1]
        display_results(io, res)
        print(io, divline)
    end
end

display_benchmark(totres) = displaybench(_stdout(), totres)

function calchash(lines::Vector{<:AbstractString})
    hashval = 0%UInt
    for text in lines
        hashval = hash(text, hashval)
    end
    hashval%UInt32 # Just show bottom 32-bits, for displaying better
end

function countlength(lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        cnt += length(text)
    end
    cnt
end

function dolowercase(lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        cnt += lowercase(text) !== text
    end
    cnt
end

function douppercase(lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        cnt += uppercase(text) !== text
    end
    cnt
end

function countchars(lines::Vector{T}) where {T<:AbstractString}
    cnt = 0
    @inbounds for text in lines, ch in text
        cnt += 1
    end
    cnt
end

function countcps(lines::Vector{T}) where {T<:AbstractString}
    cnt = 0
    for text in lines, ch in codepoints(text)
        cnt += 1
    end
    cnt
end

function countcus(lines::Vector{T}) where {T<:AbstractString}
    cnt = 0
    @inbounds for text in lines, cu in codeunits(text)
        cnt += 1
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

function checkstr(fun, lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        cnt += fun(text)
    end
    cnt
end

function checkuni(lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        cnt += sizeof(UniStr(text))
    end
    cnt
end

function checkcvt(::Type{T}, lines) where {T}
    cnt = 0
    for text in lines
        cnt += sizeof(convert(T, text))
    end
    cnt
end

function checktrans(::Type{T}, lines) where {T}
    #println("checktrans($T, $(eltype(lines))")
    cnt = 0
    for text in lines
        cnt += sizeof(transcode(T, text))
    end
    cnt
end

function checktext(fun, lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines
        is_empty(text) || fun(text)
        cnt += 1
    end
    cnt
end

@inline function iteratenextind(text)
    cnt = 0
    len = ncodeunits(text)
    pos = 1
    while pos <= len
        pos = nextind(text, pos)
        cnt += 1
    end
    cnt
end

function checkcus(fun, lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines, cu in codeunits(text)
        cnt += fun(cu)
    end
    cnt
end

function checkcp(fun, lines::Vector{<:AbstractString})
    cnt = 0
    for text in lines, cp in text
        cnt += fun(cp)
    end
    cnt
end

checkjoin(lines) = sizeof(join(lines))

function sumcharvals(lines::Vector{<:AbstractString})
    t = 0
    for text in lines, ch in text
        t += UInt32(ch)
    end
    t
end
function sumcodeunits(lines::Vector{<:AbstractString})
    t = 0
    for text in lines, cu in codeunits(text)
        t += UInt32(cu)
    end
    t
end

searchres(x::UnitRange) = x.start
searchres(x::Int) = x

function searchlines(lines, v, rev=false)
    t = 0
    if rev
    for text in lines
        t += searchres(fnd(Last, v, text))
    end
    else
    for text in lines
        t += searchres(fnd(First, v, text))
    end
    end
    t
end

function searchlinescvt(lines, v, rev=false)
    t = 0
    if rev
    for text in lines
        t += searchres(fnd(Last, v, utf8(text)))
    end
    else
    for text in lines
        t += searchres(fnd(First, v, utf8(text)))
    end
    end
    t
end

function searchlines(lines::Vector{UniStr}, v, rev=false)
    t = 0
    if rev
    for text in lines
        if typeof(text) === ASCIIStr
            t += searchres(fnd(Last, v, text::ASCIIStr))
        elseif typeof(text) === _LatinStr
            t += searchres(fnd(Last, v, text::_LatinStr))
        elseif typeof(text) === _UCS2Str
            t += searchres(fnd(Last, v, text::_UCS2Str))
        else
            t += searchres(fnd(Last, v, text::_UTF32Str))
        end
    end
    else
    for text in lines
        if typeof(text) === ASCIIStr
            t += searchres(fnd(First, v, text::ASCIIStr))
        elseif typeof(text) === _LatinStr
            t += searchres(fnd(First, v, text::_LatinStr))
        elseif typeof(text) === _UCS2Str
            t += searchres(fnd(First, v, text::_UCS2Str))
        else
            t += searchres(fnd(First, v, text::_UTF32Str))
        end
    end
    end
    t
end

# maybe change all this to be table driven and use @eval!

searchchar(lines::Vector{String}, d=false) =
    searchlines(lines, '\ufffd', d)
searchchar(lines::Vector{T}, d=false) where {T} =
    searchlines(lines, eltype(T)(0xfffd), d)
searchchar(lines::Vector{T}, d=false) where {T<:Union{ASCIIStr,LatinStr,_LatinStr}} =
    searchlines(lines, eltype(T)(0x1a), d)

searchstr(lines::Vector{String}, d=false) = searchlines(lines, "this is a test", d)
searchstr(lines::Vector{T}, d=false) where {T} = searchlines(lines, T("this is a test"), d)

searchreg(lines::Vector{<:AbstractString}, d=false) =
    searchlinescvt(lines, R"this is a test", d)
searchreg(lines::Vector{T}, d=false) where {T<:Str{<:StrRegex.Regex_CSEs}} =
    searchlines(lines, R"this is a test", d)
searchreg(lines::Vector{String}, d=false) = searchlines(lines, r"this is a test", d)

rsearchchar(lines) = searchchar(lines, true)
rsearchstr(lines) = searchstr(lines, true)

# normalize
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

function checksplit(lines)
    cnt = 0
    for text in lines
        cnt += length(split(text, " "))
    end
    cnt
end

repeat1(str)   = repeat(str, 1)
repeat10(str)  = repeat(SubString(str, 1, 5), 10)
repeat1c(str)  = @inbounds repeat(str[1], 1)
repeat80c(str) = @inbounds repeat(str[1], 80)

countsklength(l)  = checkstr(sklength, l)
countoldlength(l) = checkstr(oldlength, l)
checktextwidth(l) = checkstr(text_width, l)

checkrepeat1(l)   = checktext(repeat1, l)
checkrepeat10(l)  = checktext(repeat10, l)
checkrepeat1c(l)  = checktext(repeat1c, l)
checkrepeat80c(l) = checktext(repeat80c, l)
checkreverse(l)   = checktext(reverse, l)

checknextind(l) = checkstr(iteratenextind, l)

validstr(l)     = checkstr(is_valid, l)
asciistr(l)     = checkstr(is_ascii, l)

checkvalid(l)   = checkcp(is_valid,     l)
checkascii(l)   = checkcp(is_ascii,     l)
checkcntrl(l)   = checkcp(is_control,   l)
checkdigit(l)   = checkcp(is_digit,     l)
checkxdigit(l)  = checkcp(is_hex_digit, l)
checklower(l)   = checkcp(is_lowercase, l)
checkupper(l)   = checkcp(is_uppercase, l)
checkletter(l)  = checkcp(is_letter,    l)
checknumeric(l) = checkcp(is_numeric,   l)
checkspace(l)   = checkcp(is_space,     l)
checkalnum(l)   = checkcp(is_alphanumeric, l)
checkprint(l)   = checkcp(is_printable,    l)
checkpunct(l)   = checkcp(is_punctuation,  l)
checkgraph(l)   = checkcp(is_graphic,      l)

function mintime(f, lines)
    m = typemax(UInt)
    for i=1:10
        ns = time_ns()
        f(lines)
        t = time_ns() - ns
        t < m && (m = t)
    end
    m
end

function wrap(f, lines, io, cnts::LineCounts, t, msg, fast=true, basetime=0%UInt)
    tst = ""
    try
        res = f(lines)
        tim = fast ? mintime(f, lines) : round(UInt64, 1e9 * @belapsed ($f)($lines))
        push!(t, (msg, res, tim))
        pr"\(io)\%-22s(replace(msg, '\n' => ' ')*':') \%12d(res)"
        pr"\(io) \%12.3f(tim/1000)"
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

function mintime(::Type{T}, f, lines) where {T}
    m = typemax(UInt)
    for i=1:10
        ns = time_ns()
        f(T, lines)
        t = time_ns() - ns
        t < m && (m = t)
    end
    m
end

function wrap(::Type{T}, f, lines, io, cnts::LineCounts, t, msg,
              fast=true, basetime=0%UInt) where {T}
    tst = ""
    try
        res = f(T, lines)
        tim = fast ? mintime(T, f, lines) : round(UInt64, 1e9 * @belapsed ($f)($lines))
        push!(t, (msg, res, tim))
        pr"\(io)\%-22s(replace(msg, '\n' => ' ')*':') \%12d(res)"
        pr"\(io) \%12.3f(tim/1000)"
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
     (calchash,     "hash\nstring"),
     (checknextind, "nextind\nchars"),
     (checkjoin,    "join\nlines"),
     (checksplit,   "split\nline"),
     (checkreverse, "reverse"),
     (checkrepeat1,  "repeat 1\nstring"),
#     (checkrepeat10,  "repeat 10\nstring"),
     (searchstr,    "search\nstring"),
     (searchchar,    "search\nchar"),
     (searchreg,     "search\nregex"),
#     (rsearchstr,    "rsearch\nstring"),
#     (rsearchchar,    "rsearch\nchar"),
#     (checkrepeat1c,  "repeat 1\nchar"),
#     (checkrepeat80c,  "repeat 80\nchar"),
#    (countsklength,  "length\nSK"),
#    (countoldlength, "length\nOld"),
     (countchars,   "iteration\nchar"),
     (sumcharvals,  "sum\nchar vals"),
     (asciistr,     "isascii\nstring"),
     (validstr,     "isvalid\nstring"),
     (checkascii,   "isascii\nchars"),
     (checkvalid,   "isvalid\nchars"),
     (checklower,   "islower\nchars"),
     (checkalnum,   "isalnum\nchars"),
     #=
     (checkcntrl,   "iscntrl\nchars"),
     (checkupper,   "isupper\nchars"),
     (checkletter,  "isletter\nchars"),
     (checkspace,   "isspace\nchars"),
     (checkprint,   "isprint\nchars"),
     (checkpunct,   "ispunct\nchars"),
     (checkgraph,   "isgraph\nchars"),
     (checkdigit,   "isdigit\nchars"),
     (checkxdigit,  "isxdigit\nchars"),
     (dolowercase,  "lowercase\nstring"),
     =#
     (douppercase,  "uppercase\nstring"),
     (checktextwidth, "textwidth\nstring"),
     #    (dotitlecase,  "titlecase\nstring"),
    )

function select_lines(lines::Vector{<:AbstractString}; num=1000)
    out = Vector{Int}()
    sizehint!(out, num)
    len = length(lines)
    i = len>>1
    j = i-1
    while length(out) < num
        is_empty(lines[i]) || push!(out, i)
        is_empty(lines[j]) || push!(out, j)
        (i += 1) > len && break
        (j -= 1) < 1   && break
    end
    out
end

const mu_total = f"\<mu>s total"

function testperf(lines::Vector{T}, io, cnts, docnam, basetime, fast) where {T<:AbstractString}
    # Test performance
    pr_ul(io, f"""\%-22s(docnam) \%12s("Result") \%12s(mu_total) """)
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
        wrap(tst, lines, io, cnts, t, nam, fast,
             basetime == nothing ? 0%UInt : basetime[pos += 1][3])
    end
    pr"\n"
    docnam, cnts.bytes, t, ""
end

function testcvt(::Type{S}, vec, io, numlines, numchars, siz, docnam,
                 basetime) where {S}
    tr = checktrans
    cv = checkcvt
    #println(typeof(vec))

    # Test performance
    pr_ul(io, f"""\%-22s(docnam) \%12s("Result") \%12s(mu_total) """)
    pr_ul(io, f"""\%12s("ns/line") \%12s("ns/char") \%12s("ns/byte")\n""")
    t = []
    # Just run everything once
    try
        if S === String
            tr(UInt16, vec[1])
            tr(UInt32, vec[1])
            tr(String, vec[2])
            tr(String, vec[3])
        else
            cv(S, vec[1])
            cv(S, vec[4])
            cv(S, vec[5])
        end
    catch ex
        typeof(ex) == InterruptException && rethrow()
        str = f"Failed test: \(sprint(showerror, ex, catch_backtrace()))"
        println("File: ", docnam, ": ", str)
        return docnam, 0, t, str
    end
    if S === String
        wrap(UInt16, tr, vec[1], io, LineCounts(numlines, numchars, siz[1]), t,
             "transcode to UTF16", true, 0%UInt)
        wrap(UInt32, tr, vec[1], io, LineCounts(numlines, numchars, siz[1]), t,
             "transcode to UTF32", true, 0%UInt)
        wrap(String, tr, vec[2], io, LineCounts(numlines, numchars, siz[2]), t,
             "transcode UInt16", true, 0%UInt)
        wrap(String, tr, vec[3], io, LineCounts(numlines, numchars, siz[3]), t,
             "transcode UInt32", true, 0%UInt)
    else
        wrap(S, cv, vec[1], io, LineCounts(numlines, numchars, siz[1]), t,
             "convert String", true, basetime[1 + (codeunit(S) === UInt32)][3])
        wrap(S, cv, vec[4], io, LineCounts(numlines, numchars, siz[2]), t,
             "convert UInt16", true, basetime[3][3])
        wrap(S, cv, vec[5], io, LineCounts(numlines, numchars, siz[3]), t,
             "convert UInt32", true, basetime[4][3])
    end
    pr"\n"
    docnam, siz, t, ""
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
        bv = _uninit(BitVector, len)
        for (i, ch) in enumerate(text)
            bv[i] = fun(ch)
        end
        push!(res, bv)
    end
    res
end

function checkline(::Type{Bool}, fun, lines)
    len = length(lines)
    bv = _uninit(BitVector, len)
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
        push!(totresults, checkline(T === Integer ? typeof(fun(lines[1])) : T, fun, lines))
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

function comparetest(fun, typ, n, test, lines, res)
    results = res[n]
    list = testlist[n][1]
    local diff
    msg = f"  \%11s(string(typ,':')) \(testlist[n][2])"
    if test
        @testset "$msg" begin
            diff = fun(test, lines, results, list)
        end
        return diff
    end
    println(msg)
    diff = fun(test, lines, results, list)
    if is_empty(diff)
        pwc(:green, "\r\u2714\n")
    else
        pwc(:red, "\e[s\rX\e[u")
        io = IOBuffer()
        try
            print(io, " => ", diff)
        catch ex
            typeof(ex) == InterruptException && rethrow()
            pr"Can't display diffs \(diff[end][2]): \(sprint(showerror, ex, catch_backtrace()))"
        end
        str = String(take!(io))
        cnt = 0
        max = min(length(str), 80)
        for ch in str
            (cnt += 1) > max && break
            print(ch)
        end
        println()
    end
    diff
end

function testline(test, lines, results, list)
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
            test && !is_empty(fundiff) && pr"Failed test \(fun): \(fundiff)"
            test && @test is_empty(fundiff)
            is_empty(fundiff) || push!(diff, (i, fun, fundiff))
        catch ex
            typeof(ex) == InterruptException && rethrow()
            pr"Failed test \(fun): \(sprint(showerror, ex, catch_backtrace()))"
        end
    end
    diff
end

testchar(args...) = testch(identity, args...)
testcu(args...)   = testch(codeunits, args...)

function testch(op, test, lines, results, list)
    diff = []
    for (i, fun) in enumerate(list)
        fundiff = []
        lineres = results[i]
        try
            for (j, text) in enumerate(lines)
                chrdiff = []
                chrres = lineres[j]
                for (k, ch) in enumerate(op(text))
                    res = fun(ch)
                    res == chrres[k] ||
                        push!(chrdiff, typeof(res) == Bool ? (k, ch) : (k, ch, res, chrres[k]))
                end
                is_empty(chrdiff) || push!(fundiff, (j, text, chrdiff))
            end
            test && @test is_empty(fundiff)
            is_empty(fundiff) || push!(diff, (i, fun, fundiff))
        catch ex
            typeof(ex) == InterruptException && rethrow()
            pr"Failed test \(fun): \(sprint(showerror, ex, catch_backtrace()))"
        end
    end
    diff
end

const testlist =
    (((length, text_width, hash), "length, text_width, hash"),
     ((is_ascii, is_valid), "is_ascii, is_valid"),
     ((lowercase, uppercase, reverse), "lowercase, uppercase, reverse"),
     ((is_ascii, is_valid, is_control, is_lowercase, is_uppercase, is_letter,
       is_alphanumeric, is_space, is_printable, is_punctuation, is_graphic,
       is_digit, is_hex_digit),
      "is_(ascii|valid|control|lowercase|uppercase|letter|alphanumeric|space|printable|" *
      "punctuation|graphic|digit|hex_digit)"),
     ((UInt32, ), "UInt32"),
     ((sizeof, ), "sizeof"))

# Use String to calculate a baseline (note, sizeof needs to be checked separately, as only
# UTF8Str should be the same).

comparetestline(n, test, lines, res) = comparetest(testline, "Lines", n, test, lines, res)

function compareall(io, lines, res, test)
    pr"\(io)\(eltype(lines))\n"
    (comparetestline(1, test, lines, res),
     comparetestline(2, test, lines, res),
     comparetestline(3, test, lines, res),
     comparetest(testchar, "Chars", 4, test, lines, res),
     eltype(lines) == UTF8Str ? comparetest(testcu, "CodeUnits", 5, test, lines, res) : [],
     eltype(lines) == UTF8Str ? comparetestline(6, test, lines, res) : [])
end

function checktests(io = _stdout(); dir::Any=nothing, test::Bool=false)
    totres = []
    totcmp = []
    sampledir = joinpath(getdefdir(dir), smppath)
    for fname in readdir(sampledir)
        lines = readlines(joinpath(sampledir, fname))
        stats = calcstats(lines)
        list = [String, UTF8Str, UTF16Str, UTF32Str, UniStr]
        MT = enctyp(stats.maxtyp)
        MT != UTF32Str && push!(list, MT)
        isdefined(Main, :UTF8String) && push!(list, UTF8String, UTF16String, UTF32String)
        enc = encode_lines(list, lines)
        res = (runcheckline(Integer, lines, testlist[1][1]),
               runcheckline(Bool, lines, testlist[2][1]),
               runcheckline(Any, lines, testlist[3][1]),
               runcheckchar(lines, testlist[4][1]),
               runcheckcu(lines,   testlist[5][1]),
               runcheckline(Int, lines, testlist[6][1]))
        push!(totres, (fname, res))
        cmp = []
        pr"\(io)Checking \(fname):\n"
        for i = 2:length(list)
            push!(cmp, compareall(io, enc[i], res, test))
        end
        push!(totres, (fname, res))
        push!(totcmp, (fname, cmp))
    end
    totres, totcmp
end

function benchdir(io = _stdout();  dir::Any=nothing, fast::Bool=true, select::Bool=false)
    totres = []
    totlines = []
    totnames = []
    totsizes = []
    sampledir = joinpath(getdefdir(dir), smppath)
    for fname in readdir(sampledir)
        lines = readlines(joinpath(sampledir, fname))
        stats = calcstats(lines)
        show(io, (fname, stats))
        list = [String, UTF8Str, UTF16Str, UTF32Str, UniStr]
        MT = enctyp(stats.maxtyp)
        MT != UTF32Str && push!(list, MT)
        isdefined(Main, :UTF8String) && push!(list, UTF8String, UTF16String, UTF32String)
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
        for siz in sizes ; pr"\(io)\%12.3f(siz/stats.len)" ; end
        pr"\(io)\nRelative:               "
        for siz in sizes[2:end] ; print_ratio_rev(siz/basesize) ; end
        pr"\n\n"

        # Select the middle 1000 non-empty lines of the file for benchmarking
        if select
            sellines = select_lines(lines)
            sel = [enclines[sellines] for enclines in enc]
            selsiz = [sum(sizeof, enclines) for enclines in sel]
            selstat = calcstats(sel[1])
        else
            sellines = 1:length(lines)
            sel = enc
            selstat = stats
            selsiz = sizes
        end
        numchars = selstat.len
        numlines = selstat.num

        # Now test the performance for each
        res = create_vector(Any, length(list))
        res[1] = testperf(sel[1], io, LineCounts(numlines, numchars, selsiz[1]),
                          names[1], nothing, fast)
        basetime = res[1][3]
        for i = 2:length(list)
            res[i] = testperf(sel[i], io, LineCounts(numlines, numchars, selsiz[i]),
                              names[i], basetime, fast)
        end

        # Now test the performance of conversions for each
        v16 = [transcode(UInt16, s) for s in sel[1]]
        v32 = [transcode(UInt32, s) for s in sel[1]]
        cvtsel = (sel[1], v16, v32,
                  [convert(Text2Str, v) for v in v16],
                  [convert(Text4Str, v) for v in v32])
        cvtsiz = [sum(sizeof, enclines) for enclines in cvtsel]

        cvtres = create_vector(Any, length(list))
        cvtres[1] = testcvt(list[1], cvtsel, io, numlines, numchars, cvtsiz, names[1], nothing)
        basetime = cvtres[1][3]
        for i = 2:length(list)
            cvtres[i] = testcvt(list[i], cvtsel, io, numlines, numchars, cvtsiz, names[i], basetime)
        end
        push!(totres, (fname, stats, sizes, selstat, selsiz, res, cvtres))

        push!(totnames, names)
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

