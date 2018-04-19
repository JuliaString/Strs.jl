#=
SubString tests

Copyright 2018 Gandalf Software, Inc., Scott P. Jones, and other contributors to the Julia language
Licensed under MIT License, see LICENSE.md
Based initially on julia/test/strings/types.jl
=#

function testuni(T)
    str = T(u8str)
    str2 = str^2
    str2plain = u8str^2

    @test length(str2) == 2 * length(str)
    @test sizeof(str2) == 2 * sizeof(str)

    for i1 = 1:length(str2)
        if is_valid(str2, i1)
            for i2 = i1:length(str2)
                if is_valid(str2, i2)
                    @eval @test length($str2[$i1:$i2]) == length($str2plain[$i1:$i2])
                    @eval @test $str2[$i1:$i2] == $str2plain[$i1:$i2]
                end
            end
        end
    end

    # tests that SubString of a single multibyte `Char` string, like "∀" which takes 3 bytes
    # gives the same result as `getindex` (except that it is a view not a copy)

    s1 = T("∀")
    s2 = T("∀∀")

    for idx in 0:1
        @test SubString(s1, 1, idx) == s1[1:idx]
    end

    # Substring provided with invalid end index throws BoundsError
    if !V6_COMPAT
        @test_throws IndexError  SubString(s1, 1, 2)
        @test_throws IndexError  SubString(s1, 1, 3)
        @test_throws BoundsError SubString(s1, 1, 4)

        # Substring provided with invalid start index throws BoundsError
        @test SubString(s2, 1, 1) == s1
        @test SubString(s2, 1, 4) == s2
        @test SubString(s2, 4, 4) == s1

        @test_throws IndexError  SubString(s2, 1, 2)
        @test_throws IndexError  SubString(s2, 1, 5)
        @test_throws IndexError  SubString(s2, 2, 4)
        @test_throws BoundsError SubString(s2, 0, 1)
        @test_throws BoundsError SubString(s2, 0, 4)
        @test_throws BoundsError SubString(s2, 1, 7)
        @test_throws BoundsError SubString(s2, 4, 7)
    end

    # tests for SubString of more than one multibyte `Char` string
    # we are consistent with `getindex` for `String`
    for idx in [0, 1, 4]
        @test SubString(s2, 1, idx) == s2[1:idx]
        @test SubString(s2, 4, idx) == s2[4:idx]
    end

    # index beyond lastindex(s2)
    if !V6_COMPAT
        for idx in [2:3; 5:6]
            @test_throws IndexError SubString(s2, 1, idx)
        end
        for idx in 7:8
            @test_throws BoundsError SubString(s2, 1, idx)
        end
    end

    str = T("aa\u2200\u2222bb")
    u = SubString(str, 3, 6)
    @test length(u) == 2
    b = IOBuffer()
    write(b, u)
    @test String(take!(b)) == "\u2200\u2222"

    V6_COMPAT || @test_throws IndexError SubString(str, 4, 5)
    @test_throws BoundsError next(u, 0)
    @test_throws BoundsError next(u, 7)
    @test_throws BoundsError getindex(u, 0)
    @test_throws BoundsError getindex(u, 7)
    @test_throws BoundsError getindex(u, 0:1)
    @test_throws BoundsError getindex(u, 7:7)
    @test reverseind(u, 1) == 4
    if T === String
        @test typeof(Base.cconvert(Ptr{Int8}, u)) == SubString{String}
    else
        @test typeof(Base.cconvert(Ptr{Int8}, u)) == String
    end
    @test Base.cconvert(Ptr{Int8}, u) == u


    str = T("føøbar")
    V6_COMPAT || @test_throws BoundsError SubString(str, 10, 10)
    u = SubString(str, 4, 3)
    @test length(u) == 0
    b = IOBuffer()
    write(b, u)
    @test String(take!(b)) == ""

    # sizeof
    @test sizeof(SubString(T("abc\u2222def"), 4, 4)) == 3

    # proper nextind/prevind/thisind for SubString{String}
    let rng = MersenneTwister(1),
        strs = ["∀∃∀" * String(rand(rng, Char, 40)) * "∀∃∀", String(rand(rng, Char, 50))]
        for sa in strs
            try
                s = T(sa)
            catch ex
                println("$T(\"$sa\") = $(collect(codeunits(sa)))")
                s = sa
            end
            a = 0
            while !done(s, a)
                a = nextind(s, a)
                b = a - 1
                while !done(s, b)
                    ss = SubString(s, a, b)
                    s2 = s[a:b]
                    @test ncodeunits(ss) == ncodeunits(s2)
                    for i in 1:ncodeunits(ss)
                        @test thisind(ss, i) == thisind(s2, i)
                    end
                    for i in 0:ncodeunits(ss)
                        @test nextind(ss, i) == nextind(s2, i)
                        for j in 0:ncodeunits(ss)+5
                            if j > 0 || is_valid(ss, i)
                                @test nextind(ss, i, j) == nextind(s2, i, j)
                            end
                        end
                    end
                    for i in 1:ncodeunits(ss)+1
                        @test prevind(ss, i) == prevind(s2, i)
                        for j in 0:ncodeunits(ss)+5
                            if j > 0 || is_valid(ss, i)
                                @test prevind(ss, i, j) == prevind(s2, i, j)
                            end
                        end
                    end
                    b = nextind(s, b)
                end
            end
        end
    end

    # for is_valid(SubString{String})
    s = T("Σx + βz - 2")
    for i in -1:ncodeunits(s)+2
        if checkbounds(Bool, s, i)
            if is_valid(s, i)
                ss = SubString(s, 1, i)
                for j = 1:ncodeunits(ss)
                    @test is_valid(ss, j) == is_valid(s, j)
                end
            else
                V6_COMPAT || @test_throws IndexError SubString(s, 1, i)
            end
        elseif i > 0
            V6_COMPAT || @test_throws BoundsError SubString(s, 1, i)
        else
            @test SubString(s, 1, i) == ""
        end
    end

    # length(SubString{String}) performance specialization
    s = T("|η(α)-ϕ(κ)| < ε")
    @test length(SubString(s, 1, 0))  == length(s[1:0])
    @test length(SubString(s, 4, 4))  == length(s[4:4])
    @test length(SubString(s, 1, 7))  == length(s[1:7])
    @test length(SubString(s, 4, 11)) == length(s[4:11])


    if false
    @testset "reverseind" for S in (T, SubString, GenericString)
        for prefix in ("", "abcd", "\U0001d6a4\U0001d4c1", "\U0001d6a4\U0001d4c1c",
                       " \U0001d6a4\U0001d4c1"),
            suffix in ("", "abcde", "\U0001d4c1β\U0001d6a4", "\U0001d4c1β\U0001d6a4c",
                       " \U0001d4c1β\U0001d6a4"),
            c in ('X', 'δ', '\U0001d6a5')

            s = convert(S, T(string(prefix, c, suffix)))
            r = reverse(s)
            ri = fnd(First, ==(c), r)
            @test c == s[reverseind(s, ri)] == r[ri]
            s = convert(S, T(string(prefix, prefix, c, suffix, suffix)))
            pre = convert(S, T(prefix))
            sb = SubString(s, nextind(pre, lastindex(pre)),
                           lastindex(convert(S, T(string(prefix, prefix, c, suffix)))))
            r = reverse(sb)
            ri = fnd(First, ==(c), r)
            @test c == sb[reverseind(sb, ri)] == r[ri]
        end
    end
    end
end

function teststr(T)
    str = T("tempus fugit")              #length(str)==12

    ss = SubString(str, 1, lastindex(str)) #match source string
    @test length(ss) == length(str)

    ss = SubString(str, 1:lastindex(str))
    @test length(ss) == length(str)

    ss = SubString(str,1,0)    #empty SubString
    @test length(ss)==0

    ss = SubString(str,1:0)
    @test length(ss)==0

    if !V6_COMPAT
    @test_throws BoundsError SubString(str, 14, 20)  #start indexing beyond source string length
    @test_throws BoundsError SubString(str, 10, 16)  #end indexing beyond source string length

    @test_throws BoundsError SubString(T(""), 1, 4)  #empty source string
    @test_throws BoundsError SubString(T(""), 1, 1)  #empty source string, identical start and end index
    @test_throws BoundsError SubString(T(""), 10, 12)
    @test SubString(T(""), 12, 10) == ""
    end

    @test SubString(T("foobar"), big(1), big(3)) == "foo"

    # search and SubString (issue #5679)
    let str = T("Hello, world!"),
        u = SubString(str, 1, 5)
        @test fnd(Last, T("World"), u) == 0:-1
        @test fnd(Last, ==('z'), u) == 0
        @test fnd(Last, T("ll"), u) == 3:4
    end

    # SubString created from SubString
    let str = T("Hello, world!"),
        u = SubString(str, 2, 5)
        for idx in 1:4
            @test SubString(u, 2, idx) == u[2:idx]
            @test SubString(u, 2:idx) == u[2:idx]
        end
        if !V6_COMPAT
            @test_throws BoundsError SubString(u, 1, 10)
            @test_throws BoundsError SubString(u, 1:10)
            @test_throws BoundsError SubString(u, 20:30)
            @test_throws BoundsError SubString(u, -1:10)
        end
        @test SubString(u, 20:15) == ""
        @test SubString(u, -1, -10) == ""
        @test SubString(SubString(T("123"), 1, 2), -10, -20) == ""
    end

    # sizeof
    @test sizeof(SubString("abc\u2222def",4,4)) == 3

    # issue #3710
    @test prevind(SubString(T("{var}"),2,4),4) == 3

    # issue #4183
    @test split(SubString(T("x"), 2, 0), "y") == [""]

    # issue #6772
    if T != UTF8Str # Todo: Broken for UTF8Str?
        @test parse(Float64, SubString(T("10"),1,1)) === 1.0
        @test parse(Float64, SubString(T("1 0"),1,1)) === 1.0
        @test parse(Float32, SubString(T("10"),1,1)) === 1.0f0
    end

    # issue #5870
    V6_COMPAT || @test !occurs_in(Regex(T("aa")), SubString(T(""),1,0))
    V6_COMPAT || @test occurs_in(Regex(T("")), SubString(T(""),1,0))

    # is_valid, length, prevind, nextind for SubString{String}
    let s = T("lorem ipsum"),
        sdict = Dict(SubString(s, 1, 11)  => T("lorem ipsum"),
                     SubString(s, 1, 6)   => T("lorem "),
                     SubString(s, 1, 0)   => T(""),
                     SubString(s, 2, 4)   => T("ore"),
                     SubString(s, 2, 11)  => T("orem ipsum"),
                     SubString(s, 15, 14) => T(""),
                     )
        for (ss, s) in sdict
            @test ncodeunits(ss) == ncodeunits(s)
            for i in -2:13
                @test is_valid(ss, i) == is_valid(s, i)
            end
            for i in 1:ncodeunits(ss), j = i-1:ncodeunits(ss)
                @test length(ss, i, j) == length(s, i, j)
            end
        end
        for (ss, s) in sdict
            @test length(ss) == length(s)
            for i in 0:ncodeunits(ss), j = 0:length(ss)+1
                @test prevind(ss, i+1, j) == prevind(s, i+1, j)
                @test nextind(ss, i, j) == nextind(s, i, j)
            end
            @test_throws BoundsError prevind(s, 0)
            @test_throws BoundsError prevind(ss, 0)
            @test_throws BoundsError nextind(s, ncodeunits(ss)+1)
            @test_throws BoundsError nextind(ss, ncodeunits(ss)+1)
        end
    end

    ss = SubString(T("hello"), 1, 5)
    @test length(ss, 1, 0) == 0
    @test_throws BoundsError length(ss, 1, -1)
    @test_throws BoundsError length(ss, 1, 6)
    @test_throws BoundsError length(ss, 1, 10)
    #println("$(typeof(ss))(\"$ss\")")
    @test_throws BoundsError prevind(ss, 0, 1)
    @test prevind(ss, 1, 1) == 0
    @test prevind(ss, 6, 1) == 5
    @test_throws BoundsError prevind(ss, 7, 1)
    @test_throws BoundsError nextind(ss, -1, 1)
    @test nextind(ss, 0, 1) == 1
    @test nextind(ss, 5, 1) == 6
    @test_throws BoundsError nextind(ss, 6, 1)

    V6_COMPAT || @testset "reverseind of empty strings" begin
        for s in ("",
                  SubString(T(""), 1, 0),
                  SubString(T("ab"), 1, 0),
                  SubString(T("ab"), 2, 1),
                  SubString(T("ab"), 3, 2),
                  GenericString(T("")))
            @test reverseind(s, 0) == 1
            @test reverseind(s, 1) == 0
        end
    end

    ## Cstring tests ##

    # issue #13974: comparison against pointers
    str = T("foobar")
    ptr = pointer(str)
    cstring = Cstring(ptr)
    @test ptr == cstring
    @test cstring == ptr

    # convenient NULL string creation from Ptr{Cvoid}
    nullstr = Cstring(C_NULL)

    # Comparisons against NULL strings
    @test ptr != nullstr
    @test nullstr != ptr

    # Short-hand comparison against C_NULL
    @test nullstr == C_NULL
    @test C_NULL == nullstr
    @test cstring != C_NULL
    @test C_NULL != cstring

end

## SubString tests ##
@testset "SubString tests" begin
    for T in (UTF8Str, ) #UCS2Str
        @testset "Test Unicode substrings $T" begin
            testuni(T)
        end
    end
    for T in (UTF8Str, ASCIIStr, LatinStr) #UCS2Str
        @testset "Test ASCII substrings $T" begin
            teststr(T)
        end
    end
end
