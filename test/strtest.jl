const ver = "v0.$(VERSION.minor)"
const git = "https://github.com/JuliaString/"
const pkgdir = Pkg.dir()

const pkglist =
    ["StrTables", "LaTeX_Entities", "Emoji_Entities", "HTML_Entities", "Unicode_Entities",
     "Format", "StringLiterals", "Strs", "StrICU"]

const mparse = @static VERSION < v"0.7.0-DEV" ? parse : Meta.parse

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

function testlatex()
    le = LaTeX_Entities ; ln = le.lookupname ; mt = le.matches
    an1 = collect(keys(Base.REPLCompletions.latex_symbols))
    an2 = [x[2:end] for x in an1]
    vn = collect(values(Base.REPLCompletions.latex_symbols))
    mn = collect(le._tab.nam)
    println(setdiff(an2,mn))
    println(setdiff(mn,an2))
    for (i, key) in enumerate(an2)
        val = vn[i]
        vnew = ln(key)
        val != vnew &&
            println(i,"\t",length(val),":",hex(val[1]),":",val,"\t",length(vnew),
		    ":",hex(vnew[1]),":",vnew,"\t",key)
    end
end

function testemoji()
    ee = Emoji_Entities ; ln = ee.lookupname ; mt = ee.matches
    an1 = collect(keys(Base.REPLCompletions.emoji_symbols))
    an2 = [x[3:end-1] for x in an1]
    vn = collect(values(Base.REPLCompletions.emoji_symbols))
    mn = collect(ee._tab.nam)
    println(setdiff(an2,mn))
    println(setdiff(mn,an2))
    for (i, key) in enumerate(an2)
        val = vn[i]
        vnew = ln(key)
        val != vnew &&
            println(i,"\t",length(val),":",hex(val[1]),":",val,"\t",length(vnew),
		    ":",hex(vnew[1]),":",vnew,"\t",key)
    end
end

function testall()
    testlatex()
    testemoji()
    for str in pkglist
        include(joinpath(pkgdir, str, "test", "runtests.jl"))
    end
end

@static if VERSION < v"0.7.0-DEV"
    const _rep = replace
else
    _rep(str, a, b) = replace(str, a => b)
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
