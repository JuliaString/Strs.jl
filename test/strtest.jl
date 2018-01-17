ver = "v0.$(VERSION.minor)"
dir = Pkg.dir() # "/Users/scott/.julia/"
loc = "https://github.com/JuliaString/" # /j

const jllist = ["StrTables"]
const jllist2 = ["Format", "StringLiterals", "Strs"]
const buildlist = ["LaTeX_Entities", "Emoji_Entities", "HTML_Entities", "Unicode_Entities"]
const extras = ["StrICU"]

const pkglist = vcat(jllist,buildlist, jllist2, extras)
const datasrc = joinpath(loc, "Unicode_Entities", "data", "UnicodeData.txt")
const datadst = joinpath(dir, ver, "data")

function loadall()
    for pkg in pkglist
        try
            Pkg.installed(pkg) == nothing || Pkg.free(pkg)
        catch ex
            println("$pkg is not registered")
        end
        Pkg.rm(pkg)
        p = joinpath(dir, ver, pkg)
        run(`rm -rf $p`)
        Pkg.clone(joinpath(loc, string(pkg, ".jl")))
    end

    #run(`cp -p $datasrc $datadst`)

    Pkg.installed("LightXML") == nothing || Pkg.free("LightXML")
    Pkg.rm("LightXML")
    Pkg.clone(joinpath("https://github.com/ScottPJones/", "LightXML.jl"))
    Pkg.checkout("LightXML", "spj/v7update")

    for pkg in vcat(buildlist, extras)
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
        include(joinpath(dir,ver,str,"test","runtests.jl"))
    end
end

"""Load up a non-registered package"""
function loadpkg(pkg, build=false, branch=nothing)
    p = joinpath(dir, ver, pkg)
    run(`rm -rf $p`)
    Pkg.clone(joinpath(loc, string(pkg, ".jl")))
    run(`rm -rf $dir/lib/$ver/$pkg.ji`)
    branch == nothing || Pkg.checkout(pkg, branch)
    build && Pkg.build(pkg)
end

macro usestr()
    io = IOBuffer()
    print(io, "using ")
    for pkg in pkglist
        print(io, pkg, ", ")
    end
    :( $(take!(io)[1:end-2]) )
end

nothing
