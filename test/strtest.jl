@static VERSION < v"0.7.0-DEV" || (using Pkg, REPL)

const ver = "v0.$(VERSION.minor)"
const git = "https://github.com/JuliaString/"
const pkgdir = Pkg.dir()

const pkglist =
    ["StrTables", "LaTeX_Entities", "Emoji_Entities", "HTML_Entities", "Unicode_Entities",
     "Format", "StringLiterals", "Strs", "StrICU"]

const mparse = @static VERSION < v"0.7.0-DEV" ? parse : Meta.parse
_rep(str, a, b) = @static VERSION < v"0.7.0-DEV" ? replace(str, a, b) : replace(str, a => b)
const RC = @static VERSION < v"0.7.0-DEV" ? Base.REPLCompletions : REPL.REPLCompletions

function rmpkg(pkg)
    try
        Pkg.installed(pkg) == nothing || Pkg.free(pkg)
    catch ex
    end
    Pkg.rm(pkg)
    p = joinpath(pkgdir, pkg)
    run(`rm -rf $p`)
    j = joinpath(_rep(pkgdir, ver, joinpath("lib", ver)), string(pkg, ".ji"))
    run(`rm -rf $j`)
end

function loadall(loc=git)
    # Get rid of any old copies of the package
    for pkg in pkglist
        rmpkg(pkg)
    end


    rmpkg("LightXML")
    rmpkg("JSON")
    Pkg.clone("https://github.com/ScottPJones/JSON.jl")
    Pkg.checkout("JSON", "spj/useptr")
    Pkg.build("JSON")
    Pkg.clone("https://github.com/ScottPJones/LightXML.jl")
    Pkg.checkout("LightXML", "spj/v7update")
    Pkg.build("LightXML")

    for pkg in pkglist
        Pkg.clone(joinpath(loc, string(pkg, ".jl")))
    end

    for pkg in pkglist
        Pkg.build(pkg)
    end
end

print_chr(val) = isempty(val) || print("\e[s", val, "\e[u\e[2C")

function str_chr(val)
    isempty(val) && return ("", 0, "")
    io = IOBuffer()
    for ch in val
        print(io, hex(ch%UInt32,4), ':')
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
    onlyjulia = setdiff(juliatab, tab)
    if !isempty(onlyjulia)
        tabjulia = [(key, str_chr(get_old(key))) for key in onlyjulia]
        maxjulia = mapreduce((v)->v[2][2], max, tabjulia)
        println("Entities present in Julia table, not present in this table: ", length(onlyjulia))
        for (key, v) in tabjulia
            (val, len, str) = v
            println("\e[s", val, "\e[u\e[2C ", rpad(str, maxjulia), " ", key)
        end
        println()
    end
    onlytable = setdiff(tab, juliatab)
    if !isempty(onlytable)
        tabnew = [(key, str_chr(get_new(key))) for key in onlytable]
        maxnew = mapreduce((v)->v[2][2], max, tabnew)
        println("Entities present in this table, not present in Julia table: ", length(onlytable))
        for (key, v) in tabnew
            (val, len, str) = v
            println("\e[s", val, "\e[u\e[2C ", rpad(str, maxnew), " ", key)
        end
        println()
    end
    bothtab = intersect(juliatab, tab)
    tabdiff = []
    for key in bothtab
        vold = get_old(key)
        vnew = get_new(key)
        vold == vnew || push!(tabdiff, (key, str_chr(vold), str_chr(vnew)))
    end
    if !isempty(tabdiff)
        maxdiff1 = mapreduce((v)->v[2][2], max, tabdiff)
        maxdiff2 = mapreduce((v)->v[3][2], max, tabdiff)
        println("Entities present in both tables, with different values: ", length(tabdiff))
        for (key, v1, v2) in tabdiff
            (val, len, str) = v1
            print("\e[s", val, "\e[u\e[2C ", rpad(str, maxdiff1), "   ")
            (val, len, str) = v2
            println("\e[s", val, "\e[u\e[2C ", rpad(str, maxdiff2), "   ", key)
        end
        println()
    end
end

function testall()
    # Compare tables against what's currently in Base (for this version of Julia)
    testtable(LaTeX_Entities, RC.latex_symbols, (x)->x[2:end], (x)->"\\$x")
    testtable(Emoji_Entities, RC.emoji_symbols, (x)->x[3:end-1], (x)->"\\:$x:")
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
    :( $(mparse(String(take!(io)[1:end-2]))) )
end

nothing
