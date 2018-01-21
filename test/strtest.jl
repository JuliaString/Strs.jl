const ver = "v0.$(VERSION.minor)"
const git = "https://github.com/JuliaString/"
const pkgdir = Pkg.dir()

const pkglist =
    ["StrTables", "LaTeX_Entities", "Emoji_Entities", "HTML_Entities", "Unicode_Entities",
     "Format", "StringLiterals", "Strs", "StrICU"]

const mparse = @static VERSION < v"0.7.0-DEV" ? parse : Meta.parse
@static if VERSION < v"0.7.0-DEV"
    const _rep = replace
    const textwidth = strwidth
else
    _rep(str, a, b) = replace(str, a => b)
end

function loadall(loc=git)
    # Get rid of any old copies of the package
    for pkg in pkglist
        try
            Pkg.installed(pkg) == nothing || Pkg.free(pkg)
        catch ex
            println("$pkg is not registered")
        end
        Pkg.rm(pkg)
        p = joinpath(pkgdir, pkg)
        run(`rm -rf $p`)
    end

    Pkg.installed("LightXML") == nothing || Pkg.free("LightXML")
    Pkg.rm("LightXML")
    Pkg.clone(joinpath("https://github.com/ScottPJones/", "LightXML.jl"))
    Pkg.checkout("LightXML", "spj/v7update")

    for pkg in pkglist
        Pkg.clone(joinpath(loc, string(pkg, ".jl")))
    end

    for pkg in pkglist
        Pkg.build(pkg)
    end
end

function _dispchr(val)
    isempty(val) && return 0
    print("\e[s", val, "\e[u\e[2C")
    for ch in val
        print(lpad(hex(ch%UInt32, 4), 6))
    end
    2+6*length(val)
end

function testtable(m::Module, symtab, fun, f2)
    tab = collect(m._tab.nam)
    juliatab = [fun(k) for k in keys(symtab)]
    onlytable = setdiff(tab, juliatab)
    onlyjulia = setdiff(juliatab, tab)
    lookup = m.lookupname
    println("Entities present in Julia table, not present in this table: ", length(onlyjulia))
    for key in onlyjulia
        val = symtab[f2(key)]
        l = _dispchr(val)
        println(repeat(" ", 22 - l), key)
    end
    println()
    println("Entities present in this table, not present in Julia table: ", length(onlytable))
    for key in onlytable
        val = lookup(key)
        l = _dispchr(val)
        println(repeat(" ", 22 - l), key)
    end
    println()
    for (i, vold) in enumerate(symtab)
        key  = juliatab[i]
        vnew = lookup(key)
        if vold != vnew
            print(rpad(key, 28))
            l = _dispchr(vold)
            if !isempty(vnew)
                print(repeat(" ", 22 - l))
                _dispchr(vnew)
            end
            println()
        end
    end
end

function testall()
    # Compare tables against what's currently in Base (for this version of Julia)
    testtable(LaTeX_Entities, Base.REPLCompletions.latex_symbols,
              (x)->x[2:end], (x)->"\\$x")
    testtable(Emoji_Entities, Base.REPLCompletions.emoji_symbols,
              (x)->x[3:end-1], (x)->"\\:$x:")
    for pkg in pkglist
        Pkg.test(pkg)
    end
end

"""Load up a non-registered package"""
function loadpkg(pkg, loc=git, branch=nothing)
    try
        Pkg.installed(pkg) == nothing || Pkg.free(pkg)
    catch ex
        println("$pkg is not registered")
    end
    p = joinpath(pkgdir, pkg)
    run(`rm -rf $p`)
    Pkg.clone(joinpath(loc, string(pkg, ".jl")))
    j = joinpath(_rep(pkgdir, ver, joinpath("lib", ver)), string(pkg, ".ji"))
    run(`rm -rf $j`)
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
