# This file includes code that was formerly a part of Julia.
# License is MIT: http://julialang.org/license

@static VERSION < v"0.7.0-DEV" ? (using Base.Test) : (using Test)

using Strs
import Strs: ascii, checkstring, UTF_ERR_SHORT, UnicodeError

# Function to help generating strings for tests
randchar(::Type{T}) where {T<:Union{Char, UTF32Chr}} =
    (c = rand(0x00000:0x10f7ff); T(ifelse(c < 0xd800, c, c+0x800)))
randchar(::Type{UCS2Chr}) =
    (c = rand(0x0000:0xf7ff); UCS2Chr(ifelse(c < 0xd800, c, c+0x800)))
randchar(::Type{T}) where {T<:CodePoint} =
    T(rand(0x0:UInt8(typemax(T)))

@testset "Invalid sequences" include("invalid.jl")
@testset "Valid sequences"   include("valid.jl")
@testset "Bounds Errors"     include("bounds.jl")
@testset "UTF-16 tests"      include("utf16.jl")
@testset "UTF-32 tests"      include("utf32.jl")
@testset "Conversion errors" include("convert.jl")
@testset "Pointer functions" include("pointer.jl")

@testset "Unicode Strings" begin
# Unicode errors
let io = IOBuffer()
    show(io, UnicodeError(UTF_ERR_SHORT, 1, 10))
    check = "UnicodeError: invalid UTF-8 sequence starting at index 1 (0xa) missing one or more continuation bytes"
    @test String(take!(io)) == check
end
end

@testset "CESU-8 sequences" begin
## UTF-8 tests

# Test for CESU-8 sequences
let ch = 0x10000
    for hichar = 0xd800:0xdbff
        for lochar = 0xdc00:0xdfff
            @test_throws UnicodeError convert(UTF8Str, utf8(Char[hichar, lochar]).data)
            ch += 1
        end
    end
end

end

@testset "Reverse of UTF8" begin
# Reverse of UTF8Str
@test reverse(UTF8Str("")) == ""
@test reverse(UTF8Str("a")) == "a"
@test reverse(UTF8Str("abc")) == "cba"
@test reverse(UTF8Str("xyz\uff\u800\uffff\U10ffff")) == "\U10ffff\uffff\u800\uffzyx"
for binstr in ([0xc1], [0xd0], [0xe0], [0xed, 0x80], [0xf0], [0xf0, 0x80], [0xf0, 0x80, 0x80])
    str = vcat(codeunits("xyz"), binstr)
    @test_throws UnicodeError reverse(UTF8Str(str))
end
end

# Specifically check UTF-8 string whose lead byte is same as a surrogate
@test convert(UTF8Str, [0xed, 0x9f, 0xbf]) == "\ud7ff"

# issue #8
@test !isempty(methods(string, Tuple{Char}))

@testset "Issue 12268" begin
# 12268
for (fun, S, T) in ((utf16, UInt16, UTF16Str), (utf32, UInt32, UTF32Str))
    # AbstractString
    str = "abcd\0\uff\u7ff\u7fff\U7ffff"
    tst = SubString(convert(T,str),4)
    cmp = Char['d','\0','\uff','\u7ff','\u7fff','\U7ffff']
    cmp32 = UInt32['d','\0','\uff','\u7ff','\u7fff','\U7ffff']
    cmp16 = UInt16[0x0064,0x0000,0x00ff,0x07ff,0x7fff,0xd9bf,0xdfff]
    x = fun(tst)
    cmpx = (S == UInt16 ? cmp16 : cmp32)
    @test typeof(tst) == SubString{T}
    @test convert(T, tst) == str[4:end]
    for (i, ch) in enumerate(cmp)
        @test ch == x[i]
    end
    # Vector{T} / Array{T}
    @test convert(Vector{S}, x) == cmpx
    #@test convert(Array{S}, x) == cmpx
    # Embedded nul checking
    @test Base.containsnul(x)
    @test Base.containsnul(tst)
    # map
    #@test_throws UnicodeError map(islower, x)
    #@test_throws ArgumentError map(islower, tst)
    # SubArray conversion
    subarr = view(cmp, 1:6)
    @test convert(T, subarr) == str[4:end]
end

@test isvalid(UTF32Str, Char['d','\uff','\u7ff','\u7fff','\U7ffff'])
@test reverse(utf32("abcd \uff\u7ff\u7fff\U7ffff")) == utf32("\U7ffff\u7fff\u7ff\uff dcba")

