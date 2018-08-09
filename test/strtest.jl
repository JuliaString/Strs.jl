using ModuleInterfaceTools

const curmod = @static V6_COMPAT ? current_module() : @__MODULE__

isdefined(curmod, :STRS_SETUP) || include("setup.jl")

const pkgadd =
    ["ModuleInterfaceTools", "Format", "PCRE2",
     "StrTables", "LaTeX_Entities", "Emoji_Entities", "HTML_Entities", "Unicode_Entities"]

const pkglist =
    ["StrAPI", "CharSetEncodings", "ChrBase", "StrBase",
     "StrRegex", "StrLiterals", "StrEntities", "StrFormat", "Strs", "StrICU"]

function rmpkg(pkg)
    try
        Pkg.installed(pkg) == nothing || Pkg.free(pkg)
    catch ex
    end
    Pkg.rm(pkg)
    p = joinpath(pkgdir, pkg)
    run(`rm -rf $p`)
    j = joinpath(replace(pkgdir, ver, joinpath("lib", ver)) => string(pkg, ".ji"))
    run(`rm -rf $j`)
end

function loadall(loc=git)
    # Get rid of any old copies of the package
    for pkg in vcat(pkgadd, pkglist)
        rmpkg(pkg)
    end

    rmpkg("JSON")
    Pkg.clone(loc == git ? "https://github.com/ScottPJones/JSON.jl" : "/j/JSON.jl")
    Pkg.checkout("JSON", "spj/useptr")
    Pkg.build("JSON")
    loadpkg("LightXML"; loc="https://github.com/JuliaIO/")

    for pkg in pkgadd
        Pkg.add(pkg)
    end
    for pkg in pkglist
        Pkg.clone(joinpath(loc, string(pkg, ".jl")))
    end

    for pkg in pkglist
        Pkg.build(pkg)
    end
end

function str_chr(val)
    isempty(val) && return ("", 0, "")
    io = IOBuffer()
    for ch in val
        print(io, Strs.outhex(ch%UInt32,4), ':')
    end
    str = String(take!(io))[1:end-1]
    val, length(str), str
end

function testtable(m::Module, symtab, fun, f2)
    def = m.default
    get_old(k) = symtab[f2(k)]
    get_new(k) = lookupname(def, k)
    tab = collect(def.nam)
    juliatab = [fun(k) for k in keys(symtab)]
    juliaval = Dict{String, Vector{String}}()
    tabval   = Dict{String, Vector{String}}()
    for k in keys(symtab)
        val = symtab[k]
        mykey = fun(k)
        haskey(juliaval, val) ? push!(juliaval[val], mykey) : (juliaval[val] = [mykey])
    end
    for k in tab
        val = lookupname(def, k)
        haskey(tabval, val) ? push!(tabval[val], k) : (tabval[val] = [k])
    end
    prnt = false
    srtjv = sort!(collect(keys(juliaval)))
    outvec = Tuple{String, String, String, String}[]
    cnt = 0
    maxik = 0
    maxjk = 0
    maxkk = 0
    for val in srtjv
        keys = juliaval[val]
        haskey(tabval, val) || continue
        tabkeys = tabval[val]
        keys == tabkeys && continue
        cnt += 1
        ik = str_names(intersect(tabkeys, keys))
        jk = str_names(setdiff(keys, tabkeys))
        kk = str_names(setdiff(tabkeys, keys))
        maxik = max(maxik, length(ik))
        maxjk = max(maxjk, length(jk))
        maxkk = max(maxkk, length(kk))
        push!(outvec, (val, ik, jk, kk))
    end
    maxik += 1; maxjk += 1; maxkk += 1
    println()
    if !isempty(outvec)
        println("Names changed from old to new: ", cnt)
        str = """    $(rpad("Both", maxik))$(rpad("Removed", maxjk))$(rpad("Added", maxkk))"""
        pr_ul(str)
        println()
        for (v, ik, jk, kk) in outvec
            println("\e[s", v, "\e[u\e[4C", rpad(ik, maxik), rpad(jk, maxjk), kk)
        end
        println()
    end
    onlyjulia = sort!(collect(setdiff(juliatab, tab)))
    if !isempty(onlyjulia)
        tabjulia = [(key, str_chr(get_old(key))) for key in onlyjulia]
        maxjulia = mapreduce((v)->v[2][2], max, tabjulia)
        pr_ul("Entities present in Julia table, not present in this table: $(length(onlyjulia))\n")
        for (key, v) in sort(tabjulia)
            (val, len, str) = v
            println("\e[s", val, "\e[u\e[4C ", haskey(tabval, val) ? "  " : "- ",
                    rpad(str, maxjulia+1), key)
        end
        println()
    end
    onlytable = sort!(collect(setdiff(tab, juliatab)))
    if !isempty(onlytable)
        tabnew = [(key, str_chr(get_new(key))) for key in onlytable]
        maxnew = mapreduce((v)->v[2][2], max, tabnew)
        pr_ul("Entities present in this table, not present in Julia table: $(length(onlytable))\n")
        for (key, v) in tabnew
            (val, len, str) = v
            println("\e[s", val, "\e[u\e[4C ", haskey(juliaval, val) ? "  " : "+ ",
                    rpad(str, maxnew+1), key)
        end
        println()
    end
    bothtab = sort!(collect(intersect(juliatab, tab)))
    tabdiff = Tuple{String, Tuple{String, Int, String}, Tuple{String, Int, String}}[]
    for key in bothtab
        vold = get_old(key)
        vnew = get_new(key)
        vold == vnew || push!(tabdiff, (key, str_chr(vold), str_chr(vnew)))
    end
    if !isempty(tabdiff)
        maxdiff1 = mapreduce((v)->v[2][2], max, tabdiff)
        maxdiff2 = mapreduce((v)->v[3][2], max, tabdiff)
        pr_ul("Entities present in both tables, with different values: $(length(tabdiff))\n")
        for (key, v1, v2) in tabdiff
            (val, len, str) = v1
            print("\e[s", val, "\e[u\e[4C ", rpad(str, maxdiff1+3))
            (val, len, str) = v2
            println("\e[s", val, "\e[u\e[4C ", rpad(str, maxdiff2+3), key)
        end
        println()
    end
end

function showchanges(m::Module, symtab, fun)
    def = m.default
    tab = collect(def.nam)
    juliaval = Dict{String, Set{String}}()
    tabval   = Dict{String, Set{String}}()
    for k in keys(symtab)
        val = symtab[k]
        haskey(juliaval, val) || (juliaval[val] = Set{String}())
        push!(juliaval[val], fun(k))
    end
    for k in tab
        occurs_in('{', k) && continue
        val = lookupname(def, k)
        haskey(tabval, val) || (tabval[val] = Set{String}())
        push!(tabval[val], k)
    end
    prnt = false
    preftab = Dict{String, Set{String}}()
    for (val, keys) in juliaval
        haskey(tabval, val) || continue
        tabkeys = tabval[val]
        keys == tabkeys && continue
        prnt || (println("Names changed from old to new"); prnt = true)
        # See if there is a common suffix
        jkeys = collect(keys)
        tkeys = collect(tabkeys)
        onlyjulia = sort!(collect(setdiff(keys, tabkeys)))
        onlytable = sort!(collect(setdiff(tabkeys, keys)))
        bothtabs  = sort!(collect(intersect(keys, tabkeys)))
        if length(keys) == length(tabkeys) == 1
            jk = jkeys[1]
            tk = tkeys[1]
            jlen = sizeof(jk)
            tlen = sizeof(tk)
            mlen = min(jlen, tlen)
            lasteq = mlen
            for i = 0:mlen-1
                jk[end-i] == tk[end-i] || (lasteq = i ; break)
            end
            pref1 = tk[1:(end-lasteq)]
            pref2 = jk[1:(end-lasteq)]
            haskey(preftab, pref1) || (preftab[pref1] = Set{String}())
            push!(preftab[pref1], pref2)
            println("\e[s$pref1 -> $pref2\e[u\e[30C\e[s$val\e[u\e[3C$(rpad(jk,30))$(rpad(tk,30))")
        elseif isempty(onlyjulia)
            println("\e[s$bothtabs +\e[u\e[30C\e[s$val\e[u\e[3C$onlytable")
        elseif isempty(onlytable)
            println("\e[s$bothtabs -\e[u\e[30C\e[s$val\e[u\e[3C$onlyjulia")
        else
            println("\e[s$bothtabs\e[u\e[30C\e[s$val\e[u\e[3C$onlytable -> $onlyjulia")
        end
    end
    preftab
end

showlatex() = showchanges(LaTeX_Entities, RC.latex_symbols, (x)->x[2:end])

const StrSet = Set{String}
const TPSet = NTuple{6, StrSet}

function add_values!(tab::Dict, ind, symtab::Dict, fun)
    for (nam, val) in symtab
        haskey(tab, val) ||
            (tab[val] = (StrSet(), StrSet(), StrSet(), StrSet(), StrSet(), StrSet()))
        n = fun(nam)
        push!(tab[val][ind], n)
    end
end

function add_values!(tab::Dict, ind, m::Module)
    def = m.default
    for i = 1:length(def)
        nam = def[i]
        val = lookupname(def, nam)
        haskey(tab, val) ||
            (tab[val] = (StrSet(), StrSet(), StrSet(), StrSet(), StrSet(), StrSet()))
        push!(tab[val][ind], nam)
    end
end

function build_tables()
    tab = Dict{String, TPSet}()
    add_values!(tab, 1, RC.latex_symbols, (x)->x[2:end])
    add_values!(tab, 2, RC.emoji_symbols, (x)->x[3:end-1])
    add_values!(tab, 3, LaTeX_Entities)
    add_values!(tab, 4, Emoji_Entities)
    add_values!(tab, 5, HTML_Entities)
    #add_values!(tab, 6, Unicode_Entities)
    tab
end

function max_lengths(tab)
    lens = [0,0,0,0,0]
    for (val, nams) in tab, i = 1:length(lens)
        l = 0
        for n in nams[i]
            l += length(n) + 1
        end
        lens[i] = max(lens[i], l)
    end
    lens
end

function str_names(nameset)
    io = IOBuffer()
    allnames = sort!(collect(nameset))
    for n in allnames
        print(io, n, " ")
    end
    String(take!(io))
end

function display_table(tab)
    lens = max_lengths(tab)
    vt = collect(keys(tab))
    nt = collect(values(tab))
    srt = sortperm(vt)
    for loc in srt
        val, names = vt[loc], nt[loc]
        print("\e[s$val\e[u\e[4C\e[s$(str_chr(val)[3])\e[u\e[12C")
        for i=1:length(lens)
            print(rpad(str_names(names[i]), lens[i]))
        end
        println()
    end
end

function testall()
    # Compare tables against what's currently in Base (for this version of Julia)
    testtable(LaTeX_Entities, RC.latex_symbols, (x)->x[2:end],   (x)->string("\\", x))
    testtable(Emoji_Entities, RC.emoji_symbols, (x)->x[3:end-1], (x)->string("\\:", x, ':'))
    for pkg in pkglist
        try
            Pkg.test(pkg)
        catch ex
            println(pkg, " failed")
            println(sprint(showerror, ex, catch_backtrace()))
        end
    end
end

"""Load up a non-registered package"""
function loadpkg(pkg; loc=git, branch=nothing)
    rmpkg(pkg)
    Pkg.clone(joinpath(loc, string(pkg, ".jl")))
    branch == nothing || Pkg.checkout(pkg, branch)
    Pkg.build(pkg)
end

macro usestr()
    io = IOBuffer()
    print(io, "using ")
    for pkg in pkglist
        print(io, pkg, ", ")
    end
    :( $(parse(Expr, String(take!(io)[1:end-2]))) )
end

nothing
