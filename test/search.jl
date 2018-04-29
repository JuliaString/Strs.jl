# This file is based on a file that was originally part of Julia.
# License is MIT: https://github.com/JuliaString/LICENCE.md

# some test strings
const astr = "Hello, world.\n"
const u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"
const fbbstr = "foo,bar,baz"

const u8map = [1, 4, 5, 7, 8, 9, 10, 11, 12, 13, 16, 17, 19, 20, 21, 22, 23, 24,
               25, 26, 27, 28, 29, 30, 31, 32, 33, 35, 36, 39, 40, 41, 42, 43, 44,
               45, 46, 47, 48, 49, 50, 51, 52, 53, 54]

function test2(dir, P, str, list)
    for (p, res) in list
        pat = P(p)
        (r = fnd(dir, pat, str)) == res ||
            println("fnd($dir, $(typeof(pat)):\"$pat\", $(typeof(str)):\"$str\") => $r != $res")
        @test fnd(dir, pat, str) == res
    end
end

function test3(dir, P, str, list)
    for (p, beg, res) in list
        pat = P(p)
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
        (r = fnd(dir, ==(pat), str)) == res ||
            println("fnd($dir, ==($(typeof(pat)):\"$pat\"), $(typeof(str)):\"$str\") => $r != $res")
        @test fnd(dir, pat, str) == res
        @test fnd(dir, ==(pat), str) == res
    end
end

function test3ch(dir, C, str, list)
    for (p, beg, res) in list
        pat = cvtchar(C, p)
        (r = fnd(dir, pat, str, beg)) == res ||
            println("fnd($dir, $(typeof(pat)):'$pat', $(typeof(str)):\"$str\", $beg) => $r != $res")
        (r = fnd(dir, ==(pat), str, beg)) == res ||
            println("fnd($dir, ==($(typeof(pat)):'$pat'), $(typeof(str)):\"$str\", $beg) => $r != $res")
        @test fnd(dir, pat, str, beg) == res
        @test fnd(dir, ==(pat), str, beg) == res
    end
end

@testset "ASCII Strings" begin
    for T in ASCIIStringTypes, P in ASCIIStringTypes
        @testset "pattern: $P, str: $T" begin
            str = T(astr)
            fbb = T(fbbstr)
            C = eltype(P)
            lst = nextind(str, lastindex(str))
            empty_pred = in(C[])
            @testset "BoundsError" begin
                for ind in (0, lst+1)
                    @eval @test_throws BoundsError fnd(Fwd, SubString($P(""),1,1), $str, $ind)
                    @eval @test_throws BoundsError fnd(Rev, SubString($P(""),1,1), $str, $ind)
                    @eval @test_throws BoundsError fnd(Fwd, ==(cvtchar($C,'a')), $str, $ind)
                    @eval @test_throws BoundsError fnd(Fwd, ==(cvtchar($C,'∀')), $str, $ind)
                    @eval @test_throws BoundsError fnd(Fwd, ==(cvtchar($C,'ε')), $str, $ind)
                end
                @eval @test_throws BoundsError fnd(Fwd, $empty_pred, $str, $(lst+1))
                @eval @test_throws BoundsError fnd(Rev, $empty_pred, $str, $(lst+1))
            end
            @testset "find(First, ==(ch)...)" begin
                let pats = ('x', '\0', '\u80', '∀', 'H', 'l', ',', '\n'),
                    res  = (0,   0,    0,      0,   1,   3,   6,   14)
                    test2ch(First, C, str, zip(pats, res))
                end
            end
            @testset "find(Fwd, ==(ch)...)" begin
                let pats = ('l', 'l', 'l', ',', '.'),
                    pos  = (  4,   5,  12,  7,   14),
                    res  = (  4,  11,   0,  0,    0)
                    test3ch(Fwd,  C, str, zip(pats, pos, res))
                end
            end
            @testset "find(Last, ==(ch)...)" begin
                let pats = ('x', '\0', '\u80', '∀', 'H', 'l', ',', '\n'),
                    res  = (0, 0, 0, 0, 1, 11, 6, 14)
                    test2ch(Last,  C, str, zip(pats, res))
                end
            end
            
            @static V6_COMPAT || @testset "find(Rev, ==(ch)...)" begin
                let pats = ('H', 'l', 'l', 'l', 'l', ','),
                    pos  = (1, 5, 4, 3, 2, 5),
                    res  = (1, 4, 4, 3, 0, 0)
                    test3ch(Rev,  C, str, zip(pats, pos, res))
                end
            end
            @testset "find Fwd single-char string" begin
                test2(First, P, str,
                      (("x", 0:-1), ("H", 1:1), ("l", 3:3), ("\n", 14:14)))
                test3(Fwd,  P, str,
                      (("H", 2, 0:-1), ("l", 4, 4:4), ("l", 5, 11:11),
                       ("l", 12, 0:-1), (".", 14, 0:-1), ("\n", 14, 14:14)))
            end
            @static V6_COMPAT || @testset "find Rev single-char string" begin
                test2(Last,  P, str,
                      (("x", 0:-1), ("H", 1:1), ("l", 11:11), ("\n", 14:14)))
                test3(Rev,  P, str,
                      (("H", 2, 1:1), ("H", 1, 1:1), ("l", 10, 4:4),
                       ("l", 4, 4:4), ("l", 3, 3:3), ("l", 2, 0:-1), ("\n", 13, 0:-1)))
            end

            @testset "find First two-char string" begin
                let pats = ("xx", "fo", "oo", "o,", ",b", "az"),
                    res  = (0:-1, 1:2,  2:3,  3:4,  4:5,  10:11)
                    test2(First, P, fbb, zip(pats, res))
                end
            end
            @testset "find Fwd two-char string" begin
                let pats = ("fo", "oo", "o,", ",b", ",b", "az"),
                    pos  = (3,    4,    5,    6,    10,   11), # was 12, that gives boundserror
                    res  = (0:-1, 0:-1, 0:-1, 8:9,  0:-1, 0:-1)
                    test3(Fwd, P, fbb, zip(pats, pos, res))
                end
            end
            @testset "find Last two-char string" begin
                # string backward search with a two-char string literal
                let pats = ("xx", "fo", "oo", "o,", ",b", "az"),
                    res  = (0:-1, 1:2,  2:3,  3:4,  8:9,  10:11)
                    test2(Last, P, fbb, zip(pats, res))
                end
            end
            @static V6_COMPAT || @testset "find Rev two-char string" begin
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
                    V6_COMPAT || @test fnd(Rev, emptyP, str, i) == i:i-1
                    i = nextind(str, i)
                end
            end

            @test fnd(First, emptyP, emptyT) == 1:0
            @test fnd(Last, emptyP, emptyT) == 1:0
        end
    end
end

@testset "Unicode Tests" begin
    for T in UnicodeStringTypes, P in UnicodeStringTypes
        @testset "pattern: $P, str: $T" begin
            str = T(u8str)
            lst = nextind(str, lastindex(str))
            C = eltype(P)
            @static if !V6_COMPAT
            @testset "BoundsError" begin
                for ch = ('z', '∀', 'ε', 'a'), ind = (0, lst+1)
                    @eval @test_throws BoundsError fnd(Fwd, cvtchar($C, $ch), $str, $ind)
                end
            end
            @testset "Index Error" begin
                @test_throws IndexError fnd(Fwd, cvtchar(C, '∀'), str, 2)
                @test_throws IndexError fnd(Fwd, cvtchar(C, '∃'), str, 15)
                @test_throws IndexError fnd(Fwd, cvtchar(C, 'δ'), str, 18)
            end

            @testset "find(First, ==(chr),..." begin
                test2ch(First, C, str,
                        (('z', 0), ('\0', 0), ('\u80', 0), ('∄', 0), ('∀', 1),
                         ('∃', 13), ('x', 26), ('δ', 17), ('ε', 5)))
            end
            @testset "find(Fwd, ==(chr),..." begin
                test3ch(Fwd, C, str,
                        (('∀', 4, 0), ('∃', 16, 0), ('x', 27, 43), ('x', 44, 0)))

                @test fnd(Fwd, cvtchar(C, 'δ'), str, nextind(str, 17)) == 33
                @test fnd(Fwd, cvtchar(C, 'δ'), str, nextind(str, 33)) == 0
                @test fnd(Fwd, cvtchar(C, 'ε'), str, nextind(str,  5)) == 54
                # I think these should give BoundsError
                @test fnd(Fwd, 'ε', str, nextind(str, 54)) == 0
                for ch in ('ε', 'a')
                   @test fnd(Fwd, ==(ch), str, lst) == 0
                end
            end
            end

            @static if !V6_COMPAT
            @testset "find(Last, ==(chr),..." begin
                test2ch(Last, C, str,
                        zip(('z', '\0', '\u80', '∄', '∀', '∃', 'x', 'δ', 'ε'),
                            (  0,    0,      0,   0,   1,  13,  43,  33,  54)))
            end
            V6_COMPAT || @testset "find(Rev, ==(chr),..." begin
                test3ch(Rev, C, str,
                        zip(('∀', '∃', '∃', '∃', 'x', 'x', 'δ', 'δ', 'ε', 'ε'),
                            (1, 16, 13, 12, 42, 25, 32, 16, 53, 4), # first entry was 0, second 14
                            (1, 13, 13,  0, 26,  0, 17,  0,  5, 0))) # first entry was 0 -> 1
            end
            end

            @testset "find First/Last 1-char string,..." begin
                let pat = ( "z",  "∄", "∀",   "∃",   "x",   "ε"),
                    fwd = (0:-1, 0:-1, 1:1, 13:13, 26:26,   5:5),
                    rev = (0:-1, 0:-1, 1:1, 13:13, 43:43, 54:54)

                    test2(First, P, str, zip(pat, fwd))
                    V6_COMPAT || test2(Last, P, str, zip(pat, rev))
                end
            end
            @testset "find Fwd/Rev 1-char string,..." begin
                let pat  = ( "∀",  "∃", "x",   "x",   "ε",   "ε"),
                    posf = (   4,   16,  27,    44,     7,    54),  # was 56 for fndnext
                    resf = (0:-1, 0:-1,43:43, 0:-1, 54:54, 54:54),
                    posr = (   1,   12,  42,    25,    53,     4),
                    resr = ( 1:1, 0:-1,26:26, 0:-1,   5:5,  0:-1)

                    test3(Fwd, P, str, zip(pat, posf, resf))
                    V6_COMPAT || test3(Rev, P, str, zip(pat, posr, resr))
                end
            end

            empty = T("")
            @testset "find empty string,..." begin
                i = 1
                while i <= ncodeunits(str)
                    @test fnd(Fwd, empty, str, i) == i:i-1
                    V6_COMPAT || @test fnd(Rev, empty, str, i) == i:i-1
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
                        @eval @test fnd(First, $a, $b) == $resf
                        @eval @test fnd(First, $a, $a) == $resf
                        @static if !V6_COMPAT
                            @eval @test fnd(Last,  $a, $b) == $resl
                            @eval @test fnd(Last,  $a, $a) == $resf
                        end
                    end
                end
            end

            @testset "issue #15723" begin
                str = T("(⨳(")
                test2ch(First, C, T("⨳("), zip(('(',), (4,)))
                test3ch(Fwd, C, str, zip(('(',), (2,), (5,)))
                V6_COMPAT || test2ch(Last,  C, str, zip(('(',), (5,)))
                V6_COMPAT || test3ch(Rev, C, str, zip(('(',), (2,), (1,)))
            end

            @testset "occurs_in with a String and Char needle" begin
                str = T("foo")
                @test occurs_in(P("o"), str)
                @test occurs_in(cvtchar(C, 'o'), str)
            end
        end
    end
end

