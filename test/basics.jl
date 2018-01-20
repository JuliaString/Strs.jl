string_types = [ASCIIStr, LatinStr, UTF8Str, UCS2Str, UTF16Str, UniStr]

@testset "constructors" begin
    for T in string_types
        @test convert(T, [0x61,0x62,0x63,0x21]) == "abc!"
        @test convert(T, "abc!") == "abc!"

        emptystr = convert(T, "")
        @test isempty(emptystr)
        #@test eltype(GenericString) == Char
        str_ab = convert(T, "ab")
        str_abc = convert(T, "abc")
        @test start("abc") == 1
        #@test cmp("ab","abc") == -1
        #@test "abc" === "abc"
        #@test "ab"  !== "abc"
        #@test string("ab", 'c') === "abc"
        #@test string() === ""
        #codegen_egal_of_strings(x, y) = (x===y, x!==y)
        #@test codegen_egal_of_strings(string("ab", 'c'), "abc") === (true, false)
        let strs = [convert(T, ""), convert(T, "a"), convert(T, "a b c"), convert(T, "до свидания")]
            for x in strs, y in strs
                @test (x === y) == (object_id(x) == object_id(y))
            end
        end
    end
end

@testset "{starts,ends}with" begin
    for T in string_types
        string_abcd = convert(T, "abcd")
        string_ab = convert(T, "ab")
        string_cd = convert(T, "cd")
        string_abcdA = convert(T, "ab\0cd")
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
