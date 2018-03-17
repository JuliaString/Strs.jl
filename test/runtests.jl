# This file includes code that was formerly a part of Julia.
# License is MIT: http://julialang.org/license

const V6_COMPAT = VERSION < v"0.7.0-DEV"

@static V6_COMPAT ? (using Base.Test) : (using Test)

using Strs
import Strs: check_string, UTF_ERR_SHORT, UnicodeError, codepoint_adj, codepoint_rng

# Function to help generating strings for tests
randchar(::Type{T}) where {T} = codepoint_adj(T, rand(codepoint_rng(T)))

const IndexError = isdefined(Base, :StringIndexError) ? StringIndexError : UnicodeError

include("basics.jl")
include("types.jl")

@testset "Invalid sequences" begin include("invalid.jl") end
@testset "Valid sequences"   begin include("valid.jl")   end
@testset "Bounds Errors"     begin include("bounds.jl")  end
@testset "UTF-16 tests"      begin include("utf16.jl")   end
@testset "UTF-32 tests"      begin include("utf32.jl")   end
@testset "Conversion errors" begin include("convert.jl") end
@testset "Pointer functions" begin include("pointer.jl") end
@testset "Search functions"  begin include("search.jl")  end
@testset "SubStrings"        begin include("substr.jl")  end

include("regex.jl")

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
    for hichar = 0xd800:0xdbff, lochar = 0xdc00:0xdfff
        seq = string(Char(hichar), Char(lochar))
        # Normal conversion throws an error
        @test_throws UnicodeError utf8(seq)
        # Unsafe conversions return invalid strings as Text*Str
        @test typeof(unsafe_str(seq)) == Text1Str
        # With accept_surrogates flag, return converted to valid string (_UTF32Str)
        @test unsafe_str(seq;accept_surrogates=true)[1]%UInt == ch
        ch += 1
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
end

@test isvalid(UTF32Str, Char['d','\uff','\u7ff','\u7fff','\U7ffff'])
@test reverse(utf32("abcd \uff\u7ff\u7fff\U7ffff")) == utf32("\U7ffff\u7fff\u7ff\uff dcba")
