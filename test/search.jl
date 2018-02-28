# This file is based on a file that was originally part of Julia.
# License is MIT: https://github.com/JuliaString/LICENCE.md

# some test strings
const astr = "Hello, world.\n"
const u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
const fbbstr = "foo,bar,baz"

@testset "BoundsError" begin
    # I think these should give error on 4 also, and "" is not treated
    # consistently with SubString("",1,1), nor with Char[]
    for ind in (0, 5)
        @test_throws BoundsError find_next(SubString("",1,1), "foo", ind)
        @test_throws BoundsError find_prev(SubString("",1,1), "foo", ind)
    end

    # Note: the commented out test will be enabled after fixes to make
    # sure that find_next/find_prev are consistent
    # no matter what type of AbstractString the second argument is
    @test_throws BoundsError find_next(equalto('a'), "foo", 0)
    @test_throws BoundsError find_next(occursin(Char[]), "foo", 5)
    # @test_throws BoundsError find_prev(occursin(Char[]), "foo", 0)
    @test_throws BoundsError find_prev(occursin(Char[]), "foo", 5)

    # @test_throws ErrorException in("foobar","bar")
    @test_throws BoundsError find_next(equalto(0x1), b"\x1\x2",0)
end

const UnicodeStringTypes =
    (String, GenericString, UTF16Str, UTF32Str, UniStr, UTF8Str)
const ASCIIStringTypes =
    (UnicodeStringTypes..., ASCIIStr, LatinStr, UCS2Str)

const testfirst =
    (('x', '\0', '\u80', '∀', 'H', 'l', ',', '\n'), (0, 0, 0, 0, 1, 3, 6, 14))

const testlast =
    (('x', '\0', '\u80', '∀', 'H', 'l', ',', '\n'), (0, 0, 0, 0, 1, 11, 6, 14))

const testnext =
    (('l', 4, 4), ('l', 5, 11), ('l', 12, 0), (',', 7, 0), ('\n', 15, 0))

const testprev =
    (('H', 0, 0), ('l', 5, 4), ('l', 4, 4), ('l', 3, 3),  ('l', 2, 0), (',', 5, 0))

const asciifirst =
    (("x", 0:-1), ("H", 1:1), ("l", 3:3), ("\n", 14:14))

const asciilast =
    (("x", 0:-1), ("H", 1:1), ("l", 11:11), ("\n", 14:14))

const asciinext =
    (("H", 2, 0:-1), ("l", 4, 4:4), ("l", 5, 11:11), ("l", 12, 0:-1), ("\n", 15, 0:-1))

const asciiprev =
    (("H", 2, 1:1), ("H", 0, 0:-1), ("l", 10, 4:4), ("l", 4, 4:4), ("l", 3, 3:3),
     ("l", 2, 0:-1), ("\n", 13, 0:-1))

const unifirst =
    (('z', 0), ('\0', 0), ('\u80', 0), ('∄', 0), ('∀', 1),
     ('∃', 13), ('x', 26), ('δ', 17), ('ε', 5))

const uninext = (('∀', 4, 0), ('∃', 16, 0), ('x', 27, 43), ('x', 44, 0))

const ustr = (("éé", "ééé"), ("€€", "€€€"), ("\U1f596\U1f596", "\U1f596\U1f596\U1f596"))
const resfirst = (1:3, 1:4, 1:5)
const reslast  = (3:5, 4:7, 5:9)

@testset "ASCII Tests" begin
    @testset for T in ASCIIStringTypes
        str = T(astr)
        lst = nextind(str,lastindex(str))
        @testset "BoundsError" begin
            @test_throws BoundsError find_next(equalto('z'), str, 0)
            @test_throws BoundsError find_next(equalto('∀'), str, 0)
            @test_throws BoundsError find_next(equalto('ε'), str, lst + 1)
            @test_throws BoundsError find_next(equalto('a'), str, lst + 1)
        end
        @testset "find_*(equalto(ch)...)" begin
            for (ch, pos) in zip(testfirst[1], testfirst[2])
                @test find_first(equalto(ch), str) == pos
            end
            for (ch, beg, pos) in zip(testnext[1], testfirst[2])
                @test find_next(equalto(ch), str, beg) == pos
            end
            for (ch, pos) in testlast
                @test find_last(equalto(ch), str) == pos
            end
            for (ch, beg, pos) in testprev
                @test find_prev(equalto(ch), str, beg) == pos
            end
        end
        @testset "find_* single-char string" begin
            for (needle, res) in asciifirst
                @test find_first(needle, str) == res
            end
            for (needle, beg, res) in asciinext
                @test find_prev(needle, str, beg) == res
            end
            for (needle, res) in asciilast
                @test find_last(needle, str) == res
            end
            for (needle, beg, res) in asciiprev
                @test find_prev(needle, str, beg) == res
            end
        end

        str = T(fbbstr)
        @testset "find_* two-char string" begin
            @test find_first("xx", str) == 0:-1
            @test find_first("fo", str) == 1:2
            @test find_first("oo", str) == 2:3
            @test find_first("o,", str) == 3:4
            @test find_first(",b", str) == 4:5
            @test find_first("az", str) == 10:11

            @test find_next("fo", str, 3) == 0:-1
            @test find_next("oo", str, 4) == 0:-1
            @test find_next("o,", str, 5) == 0:-1
            @test find_next(",b", str, 6) == 8:9
            @test find_next(",b", str, 10) == 0:-1
            @test find_next("az", str, 12) == 0:-1

            # string backward search with a two-char string literal
            @test find_last("xx", str) == 0:-1
            @test find_last("fo", str) == 1:2
            @test find_last("oo", str) == 2:3
            @test find_last("o,", str) == 3:4
            @test find_last(",b", str) == 8:9
            @test find_last("az", str) == 10:11

            @test find_prev("fo", str, 1) == 0:-1
            @test find_prev("oo", str, 2) == 0:-1
            @test find_prev("o,", str, 1) == 0:-1
            @test find_prev(",b", str, 6) == 4:5
            @test find_prev(",b", str, 3) == 0:-1
            @test find_prev("az", str, 10) == 0:-1
        end

        empty = T("")
        @testset "find_* empty string,..." begin
            for i = 1:lastindex(str)
                @test find_next(empty, str, i) == i:i-1
            end
            for i = 1:lastindex(str)
                @test find_prev(empty, str, i) == i:i-1
            end
        end

        @test find_first(empty, empty) == 1:0
        @test find_last(empty, empty) == 1:0
    end
end

@testset "Unicode Tests" begin
    @testset for T in UnicodeStringTypes
        str = T(u8str)
        lst = nextind(str, lastindex(str))
        @testset "find_*(equalto(chr),..." begin
            @testset "BoundsError" begin
                @test_throws BoundsError find_next(equalto('z'), str, 0)
                @test_throws BoundsError find_next(equalto('∀'), str, 0)
                @test_throws BoundsError find_next(equalto('ε'), str, lst+1)
                @test_throws BoundsError find_next(equalto('a'), str, lst+1)
            end
            @testset "Index Error" begin
                @test_throws StringIndexError find_next(equalto('∀'), str, 2)
                @test_throws StringIndexError find_next(equalto('∃'), str, 15)
                @test_throws StringIndexError find_next(equalto('δ'), str, 18)
            end
            for (ch, pos) in unifirst
                @test find_first(equalto(ch), str) == pos
            end
            for (ch, beg, pos) in uninext
                @test find_next(equalto(ch), str, beg) == pos
            end
            @test find_next(equalto('δ'), str, nextind(str,17)) == 33
            @test find_next(equalto('δ'), str, nextind(str,33)) == 0
            @test find_next(equalto('ε'), str, nextind(str,5)) == 54
            @test find_next(equalto('ε'), str, nextind(str,54)) == 0
            for ch in ('ε', 'a')
                @test find_next(equalto(ch), str, lst) == 0
            end
            @test find_last(equalto('z'), str) == 0
            @test find_last(equalto('\0'), str) == 0
            @test find_last(equalto('\u80'), str) == 0
            @test find_last(equalto('∄'), str) == 0
            @test find_last(equalto('∀'), str) == 1
            @test find_last(equalto('∃'), str) == 13
            @test find_last(equalto('x'), str) == 43
            @test find_last(equalto('δ'), str) == 33
            @test find_last(equalto('ε'), str) == 54

            @test find_prev(equalto('∀'), str, 0) == 0
            @test find_prev(equalto('∃'), str, 14) == 13
            @test find_prev(equalto('∃'), str, 13) == 13
            @test find_prev(equalto('∃'), str, 12) == 0
            @test find_prev(equalto('x'), str, 42) == 26
            @test find_prev(equalto('x'), str, 25) == 0
            @test find_prev(equalto('δ'), str, 32) == 17
            @test find_prev(equalto('δ'), str, 16) == 0
            @test find_prev(equalto('ε'), str, 53) == 5
            @test find_prev(equalto('ε'), str, 4) == 0
        end

        @testset "find_* 1-char string,..." begin
            @test find_first("z", str) == 0:-1
            @test find_first("∄", str) == 0:-1
            @test find_first("∀", str) == 1:1
            @test find_first("∃", str) == 13:13
            @test find_first("x", str) == 26:26
            @test find_first("ε", str) == 5:5

            @test find_next("∀", str, 4) == 0:-1
            @test find_next("∃", str, 16) == 0:-1
            @test find_next("x", str, 27) == 43:43
            @test find_next("x", str, 44) == 0:-1
            @test find_next("ε", str, 7) == 54:54
            @test find_next("ε", str, 56) == 0:-1

            @test find_last("z", str) == 0:-1
            @test find_last("∄", str) == 0:-1
            @test find_last("∀", str) == 1:1
            @test find_last("∃", str) == 13:13
            @test find_last("x", str) == 43:43
            @test find_last("ε", str) == 54:54

            @test find_prev("∀", str, 0) == 0:-1
            #TODO: setting the limit in the middle of a wide char
            #      makes find_next fail but find_prev succeed.
            #      Should find_prev fail as well?
            #@test find_prev("∀", str, 2) == 0:-1 # gives 1:3
            @test find_prev("∃", str, 12) == 0:-1
            @test find_prev("x", str, 42) == 26:26
            @test find_prev("x", str, 25) == 0:-1
            @test find_prev("ε", str, 53) == 5:5
            @test find_prev("ε", str, 4) == 0:-1
        end

        empty = T("")
        @testset "find_* empty string,..." begin
            for i = 1:lastindex(str)
                @test find_next(empty, str, i) == i:i-1
            end
            for i = 1:lastindex(str)
                @test find_prev(empty, str, i) == i:i-1
            end
        end

        # Convert to new type
        cvtstr = ((T(s[1]),T(s[2])) for s in ustr)
        # issue #9365
        @testset "issue #9365" begin
            for (cvt, resf, resl) in zip(cvtstr, resfirst, reslast)
                a, b = cvt
                @test find_first(a, b) == resf
                @test find_next(a, b, 1) == resf
                @test find_first(a, a) == resf
                @test find_next(a, a, 1) == resf
                @test find_last(a, b) == resl
                @test find_prev(a, b, lastindex(b)) == resl
                @test find_last(a, a) == resf
                @test find_prev(a, a, lastindex(b)) == resf
            end
        end
    end
end

@testset "ASCII Regex" begin
    # string forward search with a single-char regex
    @test find_first(r"x", astr) == 0:-1
    @test find_first(r"H", astr) == 1:1
    @test find_first(r"l", astr) == 3:3
    @test find_first(r"\n", astr) == 14:14
    @test find_next(r"H", astr, 2) == 0:-1
    @test find_next(r"l", astr, 4) == 4:4
    @test find_next(r"l", astr, 5) == 11:11
    @test find_next(r"l", astr, 12) == 0:-1
    @test find_next(r"\n", astr, 15) == 0:-1

    for i = 1:lastindex(astr)
        @test find_next(r"."s, astr, i) == i:i
    end
    # string forward search with a zero-char regex
    for i = 1:lastindex(astr)
        @test find_next(r"", astr, i) == i:i-1
    end
end

@testset "Unicode Regex" begin
    @test find_first(r"z", u8str) == 0:-1
    @test find_first(r"∄", u8str) == 0:-1
    @test find_first(r"∀", u8str) == 1:1
    @test find_first(r"∀", u8str) == find_first(r"\u2200", u8str)
    @test find_first(r"∃", u8str) == 13:13
    @test find_first(r"x", u8str) == 26:26
    @test find_first(r"ε", u8str) == 5:5

    @test find_next(r"∀", u8str, 4) == 0:-1
    @test find_next(r"∀", u8str, 4) == find_next(r"\u2200", u8str, 4)
    @test find_next(r"∃", u8str, 16) == 0:-1
    @test find_next(r"x", u8str, 27) == 43:43
    @test find_next(r"x", u8str, 44) == 0:-1
    @test find_next(r"ε", u8str, 7) == 54:54
    @test find_next(r"ε", u8str, 56) == 0:-1

    for i = 1:lastindex(u8str)
        if isvalid(u8str,i)
            @test find_next(r"."s, u8str, i) == i:i
        end
    end

    for i = 1:lastindex(u8str)
        # TODO: should regex search fast-forward invalid indices?
        if isvalid(u8str,i)
            @test find_next(r"", u8str, i) == i:i-1
        end
    end
end

@testset "string search with a two-char regex" begin
    @test find_first(r"xx", fbb) == 0:-1
    @test find_first(r"fo", fbb) == 1:2
    @test find_first(r"oo", fbb) == 2:3
    @test find_first(r"o,", fbb) == 3:4
    @test find_first(r",b", fbb) == 4:5
    @test find_first(r"az", fbb) == 10:11

    @test find_next(r"fo", fbb, 3) == 0:-1
    @test find_next(r"oo", fbb, 4) == 0:-1
    @test find_next(r"o,", fbb, 5) == 0:-1
    @test find_next(r",b", fbb, 6) == 8:9
    @test find_next(r",b", fbb, 10) == 0:-1
    @test find_next(r"az", fbb, 12) == 0:-1
end

@testset "contains with a String and Char needle" begin
    @test contains("foo", "o")
    @test contains("foo", 'o')
end

@testset "in operator" begin
    @test_throws ErrorException "ab" ∈ "abc"
end

@testset "issue #15723" begin
    @test find_first(equalto('('), "⨳(") == 4
    @test find_next(equalto('('), "(⨳(", 2) == 5
    @test find_last(equalto('('), "(⨳(") == 5
    @test find_prev(equalto('('), "(⨳(", 2) == 1
end

#=
@test @inferred findall(equalto('a'), "éa") == [3]
@test @inferred findall(equalto('€'), "€€") == [1, 4]
@test @inferred isempty(findall(equalto('é'), ""))
=#
