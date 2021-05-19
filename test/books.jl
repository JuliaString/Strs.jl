#=
Benchmarking routines for characters and strings

Copyright 2017-2020 Gandalf Software, Inc., Scott P. Jones
Licensed under MIT License, see LICENSE.md

Functions to load up sample books
=#

const inppath = "textsamples"
const gutpath = "gutenberg"
const smppath = "samples"

const gutenbergbooks =
    (("2600/2600-0",   "English"), # War & Peace, some other languages in quotes
     ("1400/1400-0",   "English"), # Great Expectations, uses Unicode quotes
     ("42286/42286-0", "Hungarian"),
     ("31536/31536-0", "Polish"),
     ("32941/32941-0", "Japanese"),
     ("24264/24264-0", "Chinese"),
     ("40687/40687-0", "Telugu"), # Third most spoken in India, official
     ("50513/50513-0", "French"),
     ("43007/43007-0", "Arabic"),
     ("38496/38496-8", "Portuguese"), # Latin1
     ("2000/2000-0",   "Spanish"), # Don Quijote
     ("48750/48750-8", "Swedish"), # Latin1
#     ("48322/48322-8", "German"),  # Latin1
     )

const downloadedbooks = (
#                         ("LYSAIa GORA DIeVICh'Ia - SIeRGIeI GOLOVAChIoV.txt", "Russian"),
                         )

getdefdir(dir)::String = dir === nothing ? homedir() : dir

function find_beg(lines)
    CSE = "Character set encoding: "
    csebeg = sizeof(CSE) + 1
    last = length(lines)
    cse = ""
    ln = 0
    while ln < last
        l = lines[ln += 1]
        if starts_with(l, CSE)
            cse = l[csebeg:end]
        elseif sizeof(l) > 41 && starts_with(l, "***") && ends_with(l, "***") &&
            occurs_in(" PROJECT GUTENBERG EBOOK", l) &&
            occurs_in("START OF TH", l)
            # Skip over empty lines
            while ln < last && (l = lines[ln += 1]) == "" ; end
            if starts_with(l, "Produced by ")
                # Skip over non-empty lines after "Produced by"
                while ln < last && lines[ln += 1] != "" ; end
                # Skip over empty lines after "Produced by"
                while ln < last && lines[ln += 1] == "" ; end
            end
            return (ln, cse)
        end
    end
    (1, cse)
end

function find_end(lines, beg)
    ln = last = length(lines)
    while ln > beg
        l = lines[ln]
        if sizeof(l) > 41 && starts_with(l, "***") && ends_with(l, "***") &&
            occurs_in(" PROJECT GUTENBERG EBOOK", l) &&
            occurs_in("END OF TH", l)
            #print("Found END OF at line $ln")
            while ln > beg && (l = lines[ln -= 1]) == "" ; end
            #println(" => $ln")
            if starts_with(l, "End of ") && occurs_in("Project Gutenberg", l)
                #print("Found End of at line $ln")
                while ln > beg && lines[ln -= 1] == "" ; end
                #println(" => $ln")
            end
            return ln
        end
        ln -= 1
    end
    last
end

# Try 5 times (because of frequent timeout failures)
function try_download(url, nam)
    for i = 1:5
        try
            download(url, lname)
            break
        catch ex
        end
    end
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
        url = string("http://www.gutenberg.org/files/", nam, ".txt")
        try_download(url, lname)
        println("Saved to: ", lname)
        lines = readlines(lname)
        (beg, cse) = find_beg(lines)
        lst = find_end(lines, beg)
        if cse == "ISO-8859-1"
            filt = [convert(String, Str(LatinCSE, lines[ln])) for ln in beg:lst]
        elseif cse == "UTF-8" || cse == ""
            filt = lines[beg:lst]
        else
            error("Unknown character set encoding: $cse")
        end
        push!(books, (outnam, filt))
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
