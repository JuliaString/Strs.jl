const ver = "v0.$(VERSION.minor)"
const git = "https://github.com/JuliaString/"
const pkgdir = Pkg.dir()

const pkglist =
    ["StrTables", "LaTeX_Entities", "Emoji_Entities", "HTML_Entities", "Unicode_Entities",
     "Format", "StringLiterals", "Strs", "StrICU"]

const mparse = @static VERSION < v"0.7.0-DEV" ? parse : Meta.parse
@static if VERSION < v"0.7.0-DEV"
    const _rep = replace
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
    len = length(val)
    print("\t",len)
    len != 0 && print(":", hex(val[1]), ":", val)
end

function testtable(m::Module, symtab, fun)
    tab = collect(m._tab.nam)
    juliatab = [fun(k) for k in keys(symtab)]
    onlyjulia = setdiff(tab, juliatab)
    onlytable = setdiff(juliatab, tab)
    both = intersect(juliatab, tab)
    println("Entities present in Julia table, not present in this table: ", length(onlyjulia))
    isempty(onlyjulia) || println(onlyjulia)
    println()
    println("Entities present in this table, not present in Julia table: ", length(onlytable))
    isempty(onlytable) || println(onlytable)
    println()
    lookup = m.lookupname
    for (vkey, vold) in symtab
        key = fun(vkey)
        vnew = lookup(key)
        vold != vnew && println(_dispchr(vold), _dispchr(vnew), "\t", key)
    end
end

function testall()
    # Compare tables against what's currently in Base (for this version of Julia)
    testtable(LaTeX_Entities, Base.REPLCompletions.latex_symbols, (x)->x[2:end])
    testtable(Emoji_Entities, Base.REPLCompletions.emoji_symbols, (x)->x[3:end-1])
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
