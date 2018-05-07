# License is MIT: LICENSE.md

include("setup.jl")
include("strtest.jl")
include("bench.jl")

using LaTeX_Entities, Emoji_Entities

# Compare tables against what's currently in Base (for this version of Julia)
testtable(LaTeX_Entities, RC.latex_symbols, (x)->x[2:end],   (x)->string("\\", x))
testtable(Emoji_Entities, RC.emoji_symbols, (x)->x[3:end-1], (x)->string("\\:", x, ':'))

@time books = load_books()
@time save_books(books)
@time tst = checktests()
@time res = benchdir()
dispbench(res)
