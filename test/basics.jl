string_types = [ASCIIStr, LatinStr, UTF8Str, UCS2Str, UTF16Str, UniStr]

@testset "constructors" begin
    for strtype in string_types
        @test convert(strtype, [0x61,0x62,0x63,0x21]) == "abc!"
        @test convert(strtype, "abc!") == "abc!"

        @test isempty(string())
        @test eltype(GenericString) == Char
        @test start("abc") == 1
        @test cmp("ab","abc") == -1
        @test "abc" === "abc"
        @test "ab"  !== "abc"
        @test string("ab", 'c') === "abc"
        @test string() === ""
        codegen_egal_of_strings(x, y) = (x===y, x!==y)
        @test codegen_egal_of_strings(string("ab", 'c'), "abc") === (true, false)
        let strs = ["", "a", "a b c", "до свидания"]
            for x in strs, y in strs
                @test (x === y) == (object_id(x) == object_id(y))
            end
        end
    end
end

@testset "{starts,ends}with" begin
    for strtype in string_types
        string_abcd = convert(strtype, "abcd")
        string_ab = convert(strtype, "ab")
        string_cd = convert(strtype, "cd")
        string_abcdA = convert(strtype, "ab\0cd")
        @test startswith(string_abcd, 'a')
        @test startswith(string_abcd, "a")
        @test startswith(string_abcd, "ab")
        @test !startswith(string_ab, "abcd")
        @test !startswith(string_abcd, "bc")
        @test endswith(string_abcd, 'd')
        @test endswith(string_abcd, "d")
        @test endswith(string_abcd, "cd")
        @test !endswith(string_abcd, "dc")
        @test !endswith(string_cd, "abcd")
        @test startswith(string_abcdA, "ab\0c")
        @test !startswith(string_abcdA, "ab\0d")
    end
end
