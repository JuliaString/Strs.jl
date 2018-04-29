# This file includes code that was formerly a part of Julia.
# License is MIT: LICENSE.md

@testset "Utility functions" for ST in UnicodeStringTypes
    C = eltype(ST)
    foobarbaz = ST("foo,bar,baz")
    foo = ST("foo")
    bar = ST("bar")
    baz = ST("baz")
    foobar = ST("foobar")
    comma = cvtchar(C, ',')
    abc = ST("abc")
    abcd = ST("abcd")

    V6_COMPAT || @testset "padding (lpad and rpad)" begin
        str1 = ST("foo")
        str2 = ST("αβ")
        pad1 = ST("123")
        pad2 = ST("¹₂³")
        for (p, lr, rr) in ((2, "foo", "foo"), (3, "foo", "foo"), (4, " foo", "foo "),
                            (5, "  foo", "foo  "))
            @eval @test lpad($str1, $p) == $lr
            @eval @test rpad($str1, $p) == $rr
        end
        for (p, lr, rr) in ((2, "foo", "foo"), (3, "foo", "foo"), (4, "1foo", "foo1"),
                            (5, "12foo", "foo12"), (6, "123foo", "foo123"),
                            (7, "1231foo", "foo1231"), (8, "12312foo", "foo12312"),
                            (9, "123123foo", "foo123123"))
            @eval @test lpad($str1, $p, $pad1) == $lr
            @eval @test rpad($str1, $p, $pad1) == $rr
        end
        for (p, lr, rr) in ((2, "αβ", "αβ"), (3, "¹αβ", "αβ¹"), (4, "¹₂αβ", "αβ¹₂"),
                            (5, "¹₂³αβ", "αβ¹₂³"), (6, "¹₂³¹αβ", "αβ¹₂³¹"),
                            (7, "¹₂³¹₂αβ", "αβ¹₂³¹₂"), (8, "¹₂³¹₂³αβ", "αβ¹₂³¹₂³"),
                            (9, "¹₂³¹₂³¹αβ", "αβ¹₂³¹₂³¹"))
            @eval @test lpad($str2, $p, $pad2) == $lr
            @eval @test rpad($str2, $p, $pad2) == $rr
        end
    end

    # string manipulation
    V6_COMPAT || @testset "lstrip/rstrip/strip" begin
        @test strip(ST("")) == ""
        @test strip(ST(" ")) == ""
        @test strip(ST("  ")) == ""
        @test strip(ST("   ")) == ""
        @test strip(ST("\t  hi   \n")) == "hi"
        @test strip(ST("foobarfoo"), ['f','o']) == "bar"
        @test strip(ST("foobarfoo"), ('f','o')) == "bar"

        for s in ("", " ", " abc", "abc ", "  abc  "),
            f in (lstrip, rstrip, strip)

            fs = f(s)
            for T = (ST, GenericString)
                local t, b
                t = convert(T,s)
                ft = f(t)
                @test s == t
                @test fs == ft
                @test typeof(ft) == SubString{T}

                b = convert(SubString{T}, t)
                fb = f(b)
                @test s == b
                @test fs == fb
                @test typeof(fb) == SubString{T}
            end
        end
    end

    V6_COMPAT || @testset "rsplit/split" begin
        @test split(foobarbaz, cvtchar(C, 'x')) == [foobarbaz]
        @test split(foobarbaz, comma) == [foo,bar,baz]
        @test split(foobarbaz, ST(",")) == [foo,bar,baz]
        @test split(foobarbaz, comma; limit=0) == [foo,bar,baz]
        @test split(foobarbaz, comma; limit=1) == [foobarbaz]
        @test split(foobarbaz, comma; limit=2) == [foo,ST("bar,baz")]
        @test split(foobarbaz, comma; limit=3) == [foo,bar,baz]
        @test split(ST("foo,bar"), ST("o,b")) == [ST("fo"),ST("ar")]

        @test split(ST(""), comma) == [""]
        @test split(ST(","), comma) == ["",""]
        @test split(ST(",,"), comma) == ["","",""]
        @test split(ST(""), comma  ; keepempty=false) == []
        @test split(ST(","), comma ; keepempty=false) == []
        @test split(ST(",,"), comma; keepempty=false) == []

        @test split(ST("a b c")) == ["a","b","c"]
        @test split(ST("a  b \t c\n")) == ["a","b","c"]

        @test rsplit(foobarbaz, cvtchar(C, 'x')) == [foobarbaz]
        @test rsplit(foobarbaz, comma) == [foo,bar,baz]
        @test rsplit(foobarbaz, ST(",")) == [foo,bar,baz]
        @test rsplit(foobarbaz, comma; limit=0) == [foo,bar,baz]
        @test rsplit(foobarbaz, comma; limit=1) == [foobarbaz]
        @test rsplit(foobarbaz, comma; limit=2) == ["foo,bar",baz]
        @test rsplit(foobarbaz, comma; limit=3) == [foo,bar,baz]
        @test rsplit(ST("foo,bar"), ST("o,b")) == [ST("fo"),ST("ar")]

        @test rsplit(ST(""), comma) == [""]
        @test rsplit(ST(","), comma) == ["",""]
        @test rsplit(ST(",,"), comma) == ["","",""]
        @test rsplit(ST(",,"), comma; limit=2) == [",",""]
        @test rsplit(ST(""), comma  ; keepempty=false) == []
        @test rsplit(ST(","), comma ; keepempty=false) == []
        @test rsplit(ST(",,"), comma; keepempty=false) == []

        #@test rsplit("a b c") == ["a","b","c"]
        #@test rsplit("a  b \t c\n") == ["a","b","c"]

        let str = ST("a.:.ba..:..cba.:.:.dcba.:.")
            @test split(str, ".:.") == ["a","ba.",".cba",":.dcba",""]
            @test split(str, ".:."; keepempty=false) == ["a","ba.",".cba",":.dcba"]
            @test split(str, ".:.") == ["a","ba.",".cba",":.dcba",""]

            @test rsplit(str, ".:.") == ["a","ba.",".cba.:","dcba",""]
            @test rsplit(str, ".:."; keepempty=false) == ["a","ba.",".cba.:","dcba"]
            @test rsplit(str, ".:."; limit=2) == ["a.:.ba..:..cba.:.:.dcba", ""]
            @test rsplit(str, ".:."; limit=3) == ["a.:.ba..:..cba.:", "dcba", ""]
            @test rsplit(str, ".:."; limit=4) == ["a.:.ba.", ".cba.:", "dcba", ""]
            @test rsplit(str, ".:."; limit=5) == ["a", "ba.", ".cba.:", "dcba", ""]
            @test rsplit(str, ".:."; limit=6) == ["a", "ba.", ".cba.:", "dcba", ""]
        end

        # zero-width splits

        @test split(ST(""), "") == rsplit("", "") == [""]
        @test split(abc, "") == rsplit(abc, "") == ["a","b","c"]
        @test rsplit(abc, "", limit=2) == ["ab","c"]
        @test split(abc, "", limit=2) == ["a","bc"]

        # multi-byte unicode characters (issue #26225)
        @test split(ST("α β γ"), ST(" ")) == rsplit(ST("α β γ"), ST(" ")) ==
            split(ST("α β γ"), isspace) == rsplit(ST("α β γ"), isspace) == ["α","β","γ"]
        @test split(ST("ö."), ST(".")) == rsplit(ST("ö."), ST(".")) == ["ö",""]
        @test split(ST("α β γ"), ST("β")) == rsplit(ST("α β γ"), ST("β")) == ["α "," γ"]
    end

    V6_COMPAT || @testset "replace" begin
        @test replace(ST("\u2202"), '*' => '\0') == "\u2202"

        @test replace(foobar, 'o' => '0') == "f00bar"
        @test replace(foobar, 'o' => '0', count=1) == "f0obar"
        @test replace(foobar, 'o' => "") == "fbar"
        @test replace(foobar, 'o' => "", count=1) == "fobar"
        @test replace(foobar, 'f' => 'F') == "Foobar"
        @test replace(foobar, 'r' => 'R') == "foobaR"

        @test replace(ST("foofoofoo"), foo => bar) == "barbarbar"
        @test replace(ST("foobarfoo"), foo => baz) == "bazbarbaz"
        @test replace(ST("barfoofoo"), foo => baz) == "barbazbaz"

        @test replace(ST(""), "" => "") == ""
        @test replace(ST(""), "" => "x") == "x"
        @test replace(ST(""), "x" => "y") == ""

        @test replace(abcd, "" => "^") == "^a^b^c^d^"
        @test replace(abcd, "b" => "^") == "a^cd"

        @test replace(foobar, 'o' => 'ø') == "føøbar"
        @test replace(foobar, 'o' => 'ø', count=1) == "føobar"
        @test replace(ST("føøbar"), 'ø' => 'o') == "foobar"
        @test replace(ST("føøbar"), 'ø' => 'o', count=1) == "foøbar"
        @test replace(ST("føøbar"), 'ø' => 'ö') == "fööbar"
        @test replace(ST("føøbar"), 'ø' => 'ö', count=1) == "föøbar"
        @test replace(ST("føøbar"), 'ø' => "") == "fbar"
        @test replace(ST("føøbar"), 'ø' => "", count=1) == "føbar"
        @test replace(ST("føøbar"), 'f' => 'F') == "Føøbar"
        @test replace(ST("ḟøøbar"), 'ḟ' => 'F') == "Føøbar"
        @test replace(ST("føøbar"), 'f' => 'Ḟ') == "Ḟøøbar"
        @test replace(ST("ḟøøbar"), 'ḟ' => 'Ḟ') == "Ḟøøbar"
        @test replace(ST("føøbar"), 'r' => 'R') == "føøbaR"
        @test replace(ST("føøbaṙ"), 'ṙ' => 'R') == "føøbaR"
        @test replace(ST("føøbar"), 'r' => 'Ṙ') == "føøbaṘ"
        @test replace(ST("føøbaṙ"), 'ṙ' => 'Ṙ') == "føøbaṘ"

        @test replace(ST("ḟøøḟøøḟøø"), "ḟøø" => bar) == "barbarbar"
        @test replace(ST("ḟøøbarḟøø"), "ḟøø" => baz) == "bazbarbaz"
        @test replace(ST("barḟøøḟøø"), "ḟøø" => baz) == "barbazbaz"

        @test replace(ST("foofoofoo"), foo => "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
        @test replace(ST("fooƀäṙfoo"), foo => baz) == "bazƀäṙbaz"
        @test replace(ST("ƀäṙfoofoo"), foo => baz) == "ƀäṙbazbaz"

        @test replace(ST("foofoofoo"), foo => bar) == "barbarbar"
        @test replace(ST("foobarfoo"), foo => "ƀäż") == "ƀäżbarƀäż"
        @test replace(ST("barfoofoo"), foo => "ƀäż") == "barƀäżƀäż"

        @test replace(ST("ḟøøḟøøḟøø"), "ḟøø" => "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
        @test replace(ST("ḟøøƀäṙḟøø"), "ḟøø" => baz) == "bazƀäṙbaz"
        @test replace(ST("ƀäṙḟøøḟøø"), "ḟøø" => baz) == "ƀäṙbazbaz"

        @test replace(ST("ḟøøḟøøḟøø"), "ḟøø" => bar) == "barbarbar"
        @test replace(ST("ḟøøbarḟøø"), "ḟøø" => "ƀäż") == "ƀäżbarƀäż"
        @test replace(ST("barḟøøḟøø"), "ḟøø" => "ƀäż") == "barƀäżƀäż"

        @test replace(ST("ḟøøḟøøḟøø"), "ḟøø" => "ƀäṙ") == "ƀäṙƀäṙƀäṙ"
        @test replace(ST("ḟøøƀäṙḟøø"), "ḟøø" => "ƀäż") == "ƀäżƀäṙƀäż"
        @test replace(ST("ƀäṙḟøøḟøø"), "ḟøø" => "ƀäż") == "ƀäṙƀäżƀäż"

        @test replace(ST(""), "" => "ẍ") == "ẍ"
        @test replace(ST(""), "ẍ" => "ÿ") == ""

        @test replace(ST("äƀçđ"), "" => "π") == "πäπƀπçπđπ"
        @test replace(ST("äƀçđ"), "ƀ" => "π") == "äπçđ"

        @test replace(foo, "oo" => uppercase) == "fOO"

        # Issue 13332
        @test replace(abc, 'b' => 2.1) == "a2.1c"

        # test replace with a count for String and GenericString
        # check that replace is a no-op if count==0
        for s in ["aaa", Test.GenericString("aaa")]
            # @test replace(ST("aaa"), 'a' => 'z', count=0) == "aaa" # enable when undeprecated
            @test replace(s, 'a' => 'z', count=1) == "zaa"
            @test replace(s, 'a' => 'z', count=2) == "zza"
            @test replace(s, 'a' => 'z', count=3) == "zzz"
            @test replace(s, 'a' => 'z', count=4) == "zzz"
            @test replace(s, 'a' => 'z', count=typemax(Int)) == "zzz"
            @test replace(s, 'a' => 'z')    == "zzz"
        end

        # Issue 25741
        @test replace(abc, ['a', 'd'] => 'A') == "Abc"

        # for Char pattern call Char replacement function
        @test replace(ST("a"), "a" => typeof) == "SubString{$ST}"
        @test replace(ST("a"), 'a' => typeof) == string(eltype(ST))
        @test replace(ST("a"), in("a") => typeof) == string(eltype(ST))
        @test replace(ST("a"), ['a'] => typeof) == string(eltype(ST))

    end # replace tests

    V6_COMPAT || @testset "chomp/chop" begin
        @test chomp(ST("foo\n")) == "foo"
        @test chomp(ST("fo∀\n")) == "fo∀"
        @test chomp(ST("foo\r\n")) == "foo"
        @test chomp(ST("fo∀\r\n")) == "fo∀"
        @test chomp(ST("fo∀")) == "fo∀"
        @test chop(ST("fooε")) == "foo"
        @test chop(ST("foεo")) == "foε"
        @test chop(ST("∃∃∃∃")) == "∃∃∃"
        str = ST("∀ϵ∃Δ")
        @test chop(str, head=0, tail=0) == "∀ϵ∃Δ"
        @test chop(str, head=0, tail=1) == "∀ϵ∃"
        @test chop(str, head=0, tail=2) == "∀ϵ"
        @test chop(str, head=0, tail=3) == "∀"
        @test chop(str, head=0, tail=4) == ""
        @test chop(str, head=0, tail=5) == ""
        @test chop(str, head=1, tail=0) == "ϵ∃Δ"
        @test chop(str, head=2, tail=0) == "∃Δ"
        @test chop(str, head=3, tail=0) == "Δ"
        @test chop(str, head=4, tail=0) == ""
        @test chop(str, head=5, tail=0) == ""
        @test chop(str, head=1, tail=1) == "ϵ∃"
        @test chop(str, head=2, tail=2) == ""
        @test chop(str, head=3, tail=3) == ""
        @test_throws ArgumentError chop(str, head=-3, tail=3)
        @test_throws ArgumentError chop(str, head=3, tail=-3)
        @test_throws ArgumentError chop(str, head=-3, tail=-3)

        @test isa(chomp(ST("foo")), SubString)
        @test isa(chop(ST("foo")), SubString)
    end

    V6_COMPAT || @testset "bytes2hex and hex2bytes" begin
        hex_str = "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
        bin_val = hex2bytes(hex_str)

        @test div(length(hex_str), 2) == length(bin_val)
        @test hex_str == bytes2hex(bin_val)

        bin_val = hex2bytes("07bf")
        @test bin_val[1] == 7
        @test bin_val[2] == 191
        @test typeof(bin_val) == Array{UInt8, 1}
        @test length(bin_val) == 2

        # all valid hex chars
        @test "0123456789abcdefabcdef" == bytes2hex(hex2bytes("0123456789abcdefABCDEF"))

        # odd size
        @test_throws ArgumentError hex2bytes("0123456789abcdefABCDEF0")

        #non-hex characters
        @test_throws ArgumentError hex2bytes("0123456789abcdefABCDEFGH")

        @testset "Issue 23161" begin
            arr = b"0123456789abcdefABCDEF"
            arr1 = Vector{UInt8}(undef, length(arr) >> 1)
            @test hex2bytes!(arr1, arr) === arr1 # check in-place
            @test "0123456789abcdefabcdef" == bytes2hex(arr1)
            @test hex2bytes("0123456789abcdefABCDEF") == hex2bytes(arr)
            @test_throws ArgumentError hex2bytes!(arr1, b"") # incorrect arr1 length
            @test hex2bytes(b"") == UInt8[]
            @test hex2bytes(view(b"012345",1:6)) == UInt8[0x01,0x23,0x45]
            @test begin
                s = view(b"012345ab",1:6)
                d = view(zeros(UInt8, 10),1:3)
                hex2bytes!(d,s) == UInt8[0x01,0x23,0x45]
            end
            # odd size
            @test_throws ArgumentError hex2bytes(b"0123456789abcdefABCDEF0")

            #non-hex characters
            @test_throws ArgumentError hex2bytes(b"0123456789abcdefABCDEFGH")
        end
    end

    # b"" should be immutable
    V6_COMPAT || let testb() = b"0123"
        b = testb()
        @test eltype(b) === UInt8
        @test b isa AbstractVector
        @test_throws ErrorException b[4] = '4'
        @test testb() == UInt8['0','1','2','3']
    end
end
