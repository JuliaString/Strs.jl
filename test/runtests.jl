# License is MIT: LICENSE.md

include("setup.jl")
include("strtest.jl")
include("bench.jl")

@time books = load_books()
@time save_books(books)
@time tst = checktests(test=true)
