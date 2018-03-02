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
    @test_throws BoundsError find_next('a', "foo", 0)
    @test_throws BoundsError find_next(occursin(Char[]), "foo", 5)
    # @test_throws BoundsError find_prev(occursin(Char[]), "foo", 0)
    @test_throws BoundsError find_prev(occursin(Char[]), "foo", 5)

    # @test_throws ErrorException in("foobar","bar")
    #@test_throws BoundsError find_next(equalto(0x1), b"\x1\x2",0)
end

# Should test GenericString also, once overthing else is working
const UnicodeStringTypes =
    (String, UTF16Str, UTF32Str, UniStr, UTF8Str)
const ASCIIStringTypes =
    (UnicodeStringTypes..., ASCIIStr, LatinStr, UCS2Str)

const ustr = (("éé", "ééé"), ("€€", "€€€"), ("\U1f596\U1f596", "\U1f596\U1f596\U1f596"))
const resfirst = (1:3, 1:4, 1:5)
const reslast  = (3:5, 4:7, 5:9)

function cvtchar(T, ch)
    try 
        T(ch)
    catch
        UTF32Chr(ch)
    end
end

function test2(fun, T, tstr, ostr, list)
    for (opat, res) in list
        tpat = T(opat)
        @test fun(tpat, tstr) == res
        @test fun(opat, tstr) == res
        @test fun(tpat, ostr) == res
        @test fun(opat, ostr) == res
    end
end
function test3(fun, T, tstr, ostr, list)
    for (opat, beg, res) in list
        tpat = T(opat)
        @test fun(tpat, tstr, beg) == res
        @test fun(opat, tstr, beg) == res
        @test fun(tpat, ostr, beg) == res
        @test fun(opat, ostr, beg) == res
    end
end
function test2ch(fun, T, tstr, ostr, list)
    cpt = codepoint_type(T)
    for (opat, res) in list
        tpat = cvtchar(cpt, opat)
        @test fun(tpat, tstr) == res
        @test fun(opat, tstr) == res
        @test fun(tpat, ostr) == res
        @test fun(opat, ostr) == res
        @test fun(equalto(tpat), tstr) == res
        @test fun(equalto(opat), tstr) == res
        @test fun(equalto(tpat), ostr) == res
        @test fun(equalto(opat), ostr) == res
    end
end

function test3ch(fun, T, tstr, ostr, list)
    cpt = codepoint_type(T)
    for (opat, beg, res) in list
        tpat = cvtchar(cpt, opat)
        @test fun(tpat, tstr, beg) == res
        @test fun(opat, tstr, beg) == res
        @test fun(tpat, ostr, beg) == res
        @test fun(opat, ostr, beg) == res
        @test fun(equalto(tpat), tstr, beg) == res
        @test fun(equalto(opat), tstr, beg) == res
        @test fun(equalto(tpat), ostr, beg) == res
        @test fun(equalto(opat), ostr, beg) == res
    end
end

@testset "ASCII Tests" begin
    @testset for T in ASCIIStringTypes
        str = T(astr)
        lst = nextind(str, lastindex(str))
        @testset "BoundsError" begin
            @test_throws BoundsError find_next('z', str, 0)
            @test_throws BoundsError find_next('∀', str, 0)
            @test_throws BoundsError find_next('ε', str, lst + 1)
            @test_throws BoundsError find_next('a', str, lst + 1)
        end
        @testset "find_*(equalto(ch)...)" begin
            test2ch(find_first, T, str, astr,
                    zip(('x', '\0', '\u80', '∀', 'H', 'l', ',', '\n'), (0, 0, 0, 0, 1, 3, 6, 14)))
            test3ch(find_next,  T, str, astr,
                    (('l', 4, 4), ('l', 5, 11), ('l', 12, 0), (',', 7, 0), ('\n', 15, 0)))
            test2ch(find_last,  T, str, astr,
                    zip(('x', '\0', '\u80', '∀', 'H', 'l', ',', '\n'), (0, 0, 0, 0, 1, 11, 6, 14)))
            test3ch(find_prev,  T, str, astr,
                    (('H', 0, 0), ('l', 5, 4), ('l', 4, 4), ('l', 3, 3), ('l', 2, 0), (',', 5, 0)))
        end
        @testset "find_* single-char string" begin
            test2(find_first, T, str, astr,
                  (("x", 0:-1), ("H", 1:1), ("l", 3:3), ("\n", 14:14)))
            test2(find_last,  T, str, astr,
                  (("x", 0:-1), ("H", 1:1), ("l", 11:11), ("\n", 14:14)))
            test3(find_next,  T, str, astr,
                  (("H", 2, 0:-1), ("l", 4, 4:4), ("l", 5, 11:11),
                   ("l", 12, 0:-1), ("\n", 15, 0:-1)))
            test3(find_prev,  T, str, astr,
                  (("H", 2, 1:1), ("H", 0, 0:-1), ("l", 10, 4:4),
                   ("l", 4, 4:4), ("l", 3, 3:3), ("l", 2, 0:-1), ("\n", 13, 0:-1)))
        end

        str = T(fbbstr)
        @testset "find_* two-char string" begin
            let pats = ("xx", "fo", "oo", "o", ",b", "az"),
                res = (0:-1, 1:2, 2:3, 3:4, 4:5, 10:11)
                test2(find_first, T, str, fbbstr, zip(pats, res))
            end
            let pats = ("fo", "oo", "o", ",b", ",b", "az"),
                pos  = (3, 4, 5, 6, 10, 12),
                res  = (0:-1, 0:-1, 0:-1, 8:9, 0:-1, 0:-1)
                test3(find_next, T, str, fbbstr, zip(pats, pos, res))
            end
            # string backward search with a two-char string literal
            let pats = ("xx", "fo", "oo", "o", ",b", "az"),
                res = (0:-1, 1:2, 2:3, 3:4, 8:9, 10:11)
                test2(find_last, T, str, fbbstr, zip(pats, res))
            end
            let pats = ("fo", "oo", "o", ",b", ",b", "az"),
                pos  = (1, 2, 1, 6, 3, 10),
                res  = (0:-1, 0:-1, 0:-1, 4:5, 0:-1, 0:-1)
                test3(find_prev, T, str, fbbstr, zip(pats, pos, res))
            end
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

#=
@testset "Unicode Tests" begin
    @testset for T in UnicodeStringTypes
        str = T(u8str)
        lst = nextind(str, lastindex(str))
        @testset "find_*(equalto(chr),..." begin
            @testset "BoundsError" begin
                @test_throws BoundsError find_next('z', str, 0)
                @test_throws BoundsError find_next('∀', str, 0)
                @test_throws BoundsError find_next('ε', str, lst+1)
                @test_throws BoundsError find_next('a', str, lst+1)
            end
            @testset "Index Error" begin
                @test_throws StringIndexError find_next('∀', str, 2)
                @test_throws StringIndexError find_next('∃', str, 15)
                @test_throws StringIndexError find_next('δ', str, 18)
            end
            test2ch(find_first, T, str, u8str,
                    (('z', 0), ('\0', 0), ('\u80', 0), ('∄', 0), ('∀', 1),
                     ('∃', 13), ('x', 26), ('δ', 17), ('ε', 5)))
            test3ch(find_next, T, str, u8str,
                    (('∀', 4, 0), ('∃', 16, 0), ('x', 27, 43), ('x', 44, 0)))

            @test find_next('δ', str, nextind(str, 17)) == 33
            @test find_next('δ', str, nextind(str, 33)) == 0
            @test find_next('ε', str, nextind(str,  5)) == 54
            @test find_next('ε', str, nextind(str, 54)) == 0
            for ch in ('ε', 'a')
                @test find_next(equalto(ch), str, lst) == 0
            end

            test2ch(find_last, T, str, u8str,
                    zip(('z', '\0', '\u80', '∄', '∀', '∃', 'x', 'δ', 'ε'),
                        (0, 0, 0, 0, 1, 13, 43, 33, 54)))
            test3ch(find_prev, T, str, u8str,
                    zip(('∀', '∃', '∃', '∃', 'x', 'x', 'δ', 'δ', 'ε', 'ε'),
                        (0, 14, 13, 12, 42, 25, 32, 16, 53, 4),
                        (0, 13, 13,  0, 26,  0, 17,  0,  5, 0)))
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
                @test find_prev(a, a, lastindex(a)) == resf
            end
        end
    end
end
=#

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
    @test find_first(r"xx", fbbstr) == 0:-1
    @test find_first(r"fo", fbbstr) == 1:2
    @test find_first(r"oo", fbbstr) == 2:3
    @test find_first(r"o,", fbbstr) == 3:4
    @test find_first(r",b", fbbstr) == 4:5
    @test find_first(r"az", fbbstr) == 10:11

    @test find_next(r"fo", fbbstr, 3) == 0:-1
    @test find_next(r"oo", fbbstr, 4) == 0:-1
    @test find_next(r"o,", fbbstr, 5) == 0:-1
    @test find_next(r",b", fbbstr, 6) == 8:9
    @test find_next(r",b", fbbstr, 10) == 0:-1
    @test find_next(r"az", fbbstr, 12) == 0:-1
end

@testset "contains with a String and Char needle" begin
    @test contains("foo", "o")
    @test contains("foo", 'o')
end

@testset "in operator" begin
    @test_throws ErrorException "ab" ∈ "abc"
end

@testset "issue #15723" begin
    @test find_first('(', "⨳(") == 4
    @test find_next('(', "(⨳(", 2) == 5
    @test find_last('(', "(⨳(") == 5
    @test find_prev('(', "(⨳(", 2) == 1
end

#=
@test @inferred findall('a', "éa") == [3]
@test @inferred findall('€', "€€") == [1, 4]
@test @inferred isempty(findall('é', ""))
=#
