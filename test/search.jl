# This file is based on a file that was originally part of Julia.
# License is MIT: https://github.com/JuliaString/LICENCE.md

# some test strings
const astr = "Hello, world.\n"
const u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
const fbbstr = "foo,bar,baz"

const u8map = [1, 4, 5, 7, 8, 9, 10, 11, 12, 13, 16, 17, 19, 20, 21, 22, 23, 24,
               25, 26, 27, 28, 29, 30, 31, 32, 33, 35, 36, 39, 40, 41, 42, 43, 44,
               45, 46, 47, 48, 49, 50, 51, 52, 53, 54]

# Should test GenericString also, once overthing else is working
const UnicodeStringTypes =
    # (String, UTF16Str, UTF32Str, UniStr, UTF8Str)
    # (String, UTF8Str)
    (UTF8Str, )
const ASCIIStringTypes =
    (String, UTF8Str, ASCIIStr, LatinStr)
    #    (UnicodeStringTypes..., ASCIIStr, LatinStr, UCS2Str)

function cvtchar(T, ch)
    try 
        T(ch)
    catch
        Text4Chr(ch)
    end
end

function test2(dir, P, str, list)
    for (p, res) in list
        pat = typeof(p) == Regex ? p : P(p)
        (r = fnd(dir, pat, str)) == res ||
            println("fnd($dir, $(typeof(pat)):\"$pat\", $(typeof(str)):\"$str\") => $r != $res")
        @test fnd(dir, pat, str) == res
    end
end

function test3(dir, P, str, list)
    for (p, beg, res) in list
        pat = typeof(p) == Regex ? p : P(p)
        (r = fnd(dir, pat, str, beg)) == res ||
            println("fnd($dir, $(typeof(pat)):\"$pat\", $(typeof(str)):\"$str\", $beg) => $r != $res")
        @test fnd(dir, pat, str, beg) == res
    end
end

function test2ch(dir, C, str, list)
    for (p, res) in list
        pat = cvtchar(C, p)
        (r = fnd(dir, pat, str)) == res ||
            println("fnd($dir, $(typeof(p)):\"$pat\"), $(typeof(str)):\"$str\") => $r != $res")
        (r = fnd(dir, equalto(pat), str)) == res ||
            println("fnd($dir, equalto($(typeof(pat)):\"$pat\"), $(typeof(str)):\"$str\") => $r != $res")
        @test fnd(dir, pat, str) == res
        @test fnd(dir, equalto(pat), str) == res
    end
end

function test3ch(dir, C, str, list)
    for (p, beg, res) in list
        pat = cvtchar(C, p)
        (r = fnd(dir, pat, str, beg)) == res ||
            println("fnd($dir, $(typeof(pat)):'$pat', $(typeof(str)):\"$str\", $beg) => $r != $res")
        (r = fnd(dir, equalto(pat), str, beg)) == res ||
            println("fnd($dir, equalto($(typeof(pat)):'$pat'), $(typeof(str)):\"$str\", $beg) => $r != $res")
        @test fnd(dir, pat, str, beg) == res
        @test fnd(dir, equalto(pat), str, beg) == res
    end
end

@testset "ASCII Strings" begin
    for T in ASCIIStringTypes, P in ASCIIStringTypes
        @testset "pattern: $P, str: $T" begin
            str = T(astr)
            fbb = T(fbbstr)
            C = eltype(P)
            lst = nextind(str, lastindex(str))
            empty_pred = occursin(C[])
            @testset "BoundsError" begin
                for ind in (0, lst, lst+1), dir in (Fwd, Rev)
                    @test_throws BoundsError fnd(dir, SubString(P(""),1,1), str, ind)
                    @test_throws BoundsError fnd(dir, equalto(cvtchar(C,'a')), str, ind)
                    @test_throws BoundsError fnd(dir, equalto(cvtchar(C,'∀')), str, ind)
                    @test_throws BoundsError fnd(dir, equalto(cvtchar(C,'ε')), str, ind)
                    @test_throws BoundsError fnd(dir, empty_pred, str, ind)
                end
            end
            @testset "fnd(dir, equalto(ch)...)" begin
                let pats = ('x', '\0', '\u80', '∀', 'H', 'l', ',', '\n'),
                    res  = (0,   0,    0,      0,   1,   3,   6,   14)
                    test2ch(Fwd, C, str, zip(pats, res))
                end
                let pats = ('l', 'l', 'l', ',', '.'),
                    pos  = (  4,   5,  12,  7,   14),
                    res  = (  4,  11,   0,  0,    0)
                    test3ch(Fwd,  C, str, zip(pats, pos, res))
                end
                let pats = ('x', '\0', '\u80', '∀', 'H', 'l', ',', '\n'),
                    res  = (0, 0, 0, 0, 1, 11, 6, 14)
                    test2ch(Rev,  C, str, zip(pats, res))
                end
                let pats = ('H', 'l', 'l', 'l', 'l', ','),
                    pos  = (1, 5, 4, 3, 2, 5),
                    res  = (1, 4, 4, 3, 0, 0)
                    test3ch(Rev,  C, str, zip(pats, pos, res))
                end
            end
            @testset "find single-char string" begin
                test2(Fwd, P, str,
                      (("x", 0:-1), ("H", 1:1), ("l", 3:3), ("\n", 14:14)))
                test2(Rev,  P, str,
                      (("x", 0:-1), ("H", 1:1), ("l", 11:11), ("\n", 14:14)))
                test3(Fwd,  P, str,
                      (("H", 2, 0:-1), ("l", 4, 4:4), ("l", 5, 11:11),
                       ("l", 12, 0:-1), (".", 14, 0:-1), ("\n", 14, 14:14)))
                test3(Rev,  P, str,
                      (("H", 2, 1:1), ("H", 1, 1:1), ("l", 10, 4:4),
                       ("l", 4, 4:4), ("l", 3, 3:3), ("l", 2, 0:-1), ("\n", 13, 0:-1)))
            end

            @testset "find two-char string" begin
                let pats = ("xx", "fo", "oo", "o,", ",b", "az"),
                    res  = (0:-1, 1:2,  2:3,  3:4,  4:5,  10:11)
                    test2(Fwd, P, fbb, zip(pats, res))
                end
                let pats = ("fo", "oo", "o,", ",b", ",b", "az"),
                    pos  = (3,    4,    5,    6,    10,   11), # was 12, that gives boundserror
                    res  = (0:-1, 0:-1, 0:-1, 8:9,  0:-1, 0:-1)
                    test3(Fwd, P, fbb, zip(pats, pos, res))
                end
                # string backward search with a two-char string literal
                let pats = ("xx", "fo", "oo", "o,", ",b", "az"),
                    res  = (0:-1, 1:2,  2:3,  3:4,  8:9,  10:11)
                    test2(Rev, P, fbb, zip(pats, res))
                end
                let pats = ("fo", "oo", "o,", ",b", ",b", "az"),
                    pos  = (1,    2,    1,    6,    3,    10),
                    res  = (0:-1, 0:-1, 0:-1, 4:5,  0:-1, 0:-1)
                    test3(Rev, P, fbb, zip(pats, pos, res))
                end
            end

            emptyT = T("")
            emptyP = P("")

            @testset "find empty string,..." begin
                i = 1
                while i <= ncodeunits(str)
                    @test fnd(Fwd, emptyP, str, i) == i:i-1
                    @test fnd(Rev, emptyP, str, i) == i:i-1
                    i = nextind(str, i)
                end
            end

            @test fnd(Fwd, emptyP, emptyT) == 1:0
            @test fnd(Rev, emptyP, emptyT) == 1:0

            @testset "Regex" begin
                # string forward search with a single-char regex
                let pats = (r"x", r"H", r"l", r"\n"),
                    res  = (0:-1, 1:1, 3:3, 14:14)
                    test2(Fwd, P, str, zip(pats, res))
                end
                let pats = (r"H", r"l", r"l", r"l", r"\n"),
                    pos  = (  2,    4,    5,   12,    14), # Was 15 for findnext
                    res  = (0:-1, 4:4,11:11, 0:-1, 14:14)
                    test3(Fwd, P, str, zip(pats, pos, res))
                end
                i = 1
                while i <= ncodeunits(str)
                    @test fnd(Fwd, r"."s, str, i) == i:i
                    # string forward search with a zero-char regex
                    @test fnd(Fwd, r"", str, i) == i:i-1
                    i = nextind(str, i)
                end
                let pats = (r"xx", r"fo", r"oo", r"o,", r",b", r"az"),
                    res  = ( 0:-1,   1:2,   2:3,   3:4,   4:5, 10:11)
                    test2(Fwd, P, fbb, zip(pats, res))
                end
                let pats = (r"fo", r"oo", r"o,", r",b", r",b", r"az"),
                    pos  = (    3,     4,     5,     6,    10,    11),
                    res  = ( 0:-1,  0:-1,  0:-1,   8:9,  0:-1,  0:-1) # was 12 for findnext
                    test3(Fwd, P, fbb, zip(pats, pos, res))
                end
            end
        end
    end
end

@testset "Unicode Tests" begin
    for T in UnicodeStringTypes, P in UnicodeStringTypes
        @testset "pattern: $P, str: $T" begin
            str = T(u8str)
            lst = nextind(str, lastindex(str))
            C = eltype(P)
            @testset "BoundsError" begin
                for ch = ('z', '∀', 'ε', 'a'), ind = (0, lst, lst+1)
                    @test_throws BoundsError fnd(Fwd, cvtchar(C, ch), str, ind)
                end
            end
            @testset "Index Error" begin
                @test_throws StringIndexError fnd(Fwd, cvtchar(C, '∀'), str, 2)
                @test_throws StringIndexError fnd(Fwd, cvtchar(C, '∃'), str, 15)
                @test_throws StringIndexError fnd(Fwd, cvtchar(C, 'δ'), str, 18)
            end
            @testset "fnd(Fwd, equalto(chr),..." begin
                test2ch(Fwd, C, str,
                        (('z', 0), ('\0', 0), ('\u80', 0), ('∄', 0), ('∀', 1),
                         ('∃', 13), ('x', 26), ('δ', 17), ('ε', 5)))
                test3ch(Fwd, C, str,
                        (('∀', 4, 0), ('∃', 16, 0), ('x', 27, 43), ('x', 44, 0)))

                @test fnd(Fwd, cvtchar(C, 'δ'), str, nextind(str, 17)) == 33
                @test fnd(Fwd, cvtchar(C, 'δ'), str, nextind(str, 33)) == 0
                @test fnd(Fwd, cvtchar(C, 'ε'), str, nextind(str,  5)) == 54
                # These give BoundsError now
                #@test fnd(Fwd, 'ε', str, nextind(str, 54)) == 0
                #for ch in ('ε', 'a')
                #   @test fnd(Fwd, equalto(ch), str, lst) == 0
                #end
            end

            @testset "fnd(Rev, equalto(chr),..." begin
                test2ch(Rev, C, str,
                        zip(('z', '\0', '\u80', '∄', '∀', '∃', 'x', 'δ', 'ε'),
                            (  0,    0,      0,   0,   1,  13,  43,  33,  54)))
                test3ch(Rev, C, str,
                        zip(('∀', '∃', '∃', '∃', 'x', 'x', 'δ', 'δ', 'ε', 'ε'),
                            (1, 16, 13, 12, 42, 25, 32, 16, 53, 4), # first entry was 0, second 14
                            (1, 13, 13,  0, 26,  0, 17,  0,  5, 0))) # first entry was 0 -> 1
            end

            @testset "find 1-char string,..." begin
                let pat = ( "z",  "∄", "∀",   "∃",   "x",   "ε"),
                    fwd = (0:-1, 0:-1, 1:1, 13:13, 26:26,   5:5),
                    rev = (0:-1, 0:-1, 1:1, 13:13, 43:43, 54:54)

                    test2(Fwd, P, str, zip(pat, fwd))
                    test2(Rev, P, str, zip(pat, rev))
                end
                let pat  = ( "∀",  "∃", "x",   "x",   "ε",   "ε"),
                    posf = (   4,   16,  27,    44,     7,    54),  # was 56 for findnext
                    resf = (0:-1, 0:-1,43:43, 0:-1, 54:54, 54:54),
                    posr = (   1,   12,  42,    25,    53,     4),
                    resr = ( 1:1, 0:-1,26:26, 0:-1,   5:5,  0:-1)

                    test3(Fwd, P, str, zip(pat, posf, resf))
                    test3(Rev, P, str, zip(pat, posr, resr))
                end
            end

            empty = T("")
            @testset "find empty string,..." begin
                i = 1
                while i <= ncodeunits(str)
                    @test fnd(Fwd, empty, str, i) == i:i-1
                    @test fnd(Rev, empty, str, i) == i:i-1
                    i = nextind(str, i)
                end
            end

            # issue #9365
            @testset "issue #9365" begin
                let ustr = (("éé", "ééé"),
                            ("€€", "€€€"),
                            ("\U1f596\U1f596", "\U1f596\U1f596\U1f596")),
                    fwd = (1:3, 1:4, 1:5),
                    rev = (3:5, 4:7, 5:9)
                    for (s, resf, resl) in zip(ustr, fwd, rev)
                        a = P(first(s))
                        b = T(last(s))
                        @test fnd(Fwd, a, b) == resf
                        @test fnd(Fwd, a, a) == resf
                        @test fnd(Rev, a, b) == resl
                        @test fnd(Rev, a, a) == resf
                    end
                end
            end
            @testset "Regex" begin
                let pats = (r"z", r"∄", r"∀", r"∃", r"x", r"ε"),
                    res  = (0:-1, 0:-1, 1:1, 13:13,26:26,  5:5)
                    test2(Fwd, P, str, zip(pats, res))
                end
                let pats = (r"∀", r"∃", r"x", r"x", r"ε", r"ε"),
                    pos  = (   4,   16,   27,   44,    7,   54), # was 56 for findnext
                    res  = (0:-1, 0:-1,43:43, 0:-1,54:54,54:54)  # was 0:-1 for last
                    test3(Fwd, P, str, zip(pats, pos, res))
                end
                @test fnd(Fwd, r"∀", str)    == fnd(Fwd, r"\u2200", str)
                @test fnd(Fwd, r"∀", str, 4) == fnd(Fwd, r"\u2200", str, 4)
                i = 1
                while i <= ncodeunits(str)
                    @test fnd(Fwd, r"."s, str, i) == i:i
                    # string forward search with a zero-char regex
                    @test fnd(Fwd, r"", str, i) == i:i-1
                    i = nextind(str, i)
                end
            end
            @testset "issue #15723" begin
                str = T("(⨳(")
                test2ch(Fwd, C, T("⨳("), zip(('(',), (4,)))
                test2ch(Rev, C, str, zip(('(',), (5,)))
                test3ch(Fwd, C, str, zip(('(',), (2,), (5,)))
                test3ch(Rev, C, str, zip(('(',), (2,), (1,)))
            end
            @testset "contains with a String and Char needle" begin
                str = T("foo")
                @test contains(str, P("o"))
                @test contains(str, cvtchar(C, 'o'))
            end
        end
    end
end
