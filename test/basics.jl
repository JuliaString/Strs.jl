test_string_length = 100

#string_types = [ASCIIStr, LatinStr, UTF8Str, UCS2Str, UTF16Str, UTF32Str, UniStr]
string_types = [ASCIIStr, LatinStr, UTF8Str, UTF16Str, UTF32Str]

##  create type specific test strings
test_strings_base = Dict{String, Any}()
for T in string_types
    test_strings_base["$T"] = join([randchar(Strs.codepoint_type(T)) for i in 1:test_string_length])
end


test_strings_dict = Dict(
    "ASCIIStr" => [test_strings_base["ASCIIStr"]],
    "LatinStr" => [test_strings_base["ASCIIStr"], test_strings_base["LatinStr"]],
    "UTF8Str" => [test_strings_base["ASCIIStr"], test_strings_base["LatinStr"], test_strings_base["UTF8Str"]],
    "UTF16Str" => [test_strings_base["ASCIIStr"], test_strings_base["LatinStr"], test_strings_base["UTF8Str"], test_strings_base["UTF16Str"]],
    "UTF32Str" => [test_strings_base["ASCIIStr"], test_strings_base["LatinStr"], test_strings_base["UTF8Str"], test_strings_base["UTF16Str"], test_strings_base["UTF32Str"]]
)

@testset "constructors" begin
    for T in string_types
        #@test convert(T, [0x61,0x62,0x63,0x21]) == "abc!"
        @test convert(T, "abc!") == "abc!"

        emptystr = convert(T, "")
        @test isempty(emptystr)
        str_ab = convert(T, "ab")
        str_abc = convert(T, "abc")
        # let strs = [convert(T, ""),
        #             convert(T, "a"),
        #             convert(T, "a b c"),
        #             convert(T, "до свидания")]
        #     for x in strs, y in strs
        #         @test (x === y) == (object_id(x) == object_id(y))
        #     end
        # end
    end
end

@testset "{starts,ends}with" begin
    i = 1
    for T in string_types
        test_strings = test_strings_dict["$T"]
        for test_string in test_strings
            converted_string = convert(T, test_string)
            @test startswith(converted_string, test_string[1])
            @test endswith(converted_string, test_string[end])
            ##   TODO needs test which would run in case the start and end chars are the same
            if test_string[1] != test_string[end]
                @test !startswith(converted_string, test_string[end])
                @test !endswith(converted_string, test_string[1])
            end
            i += 1
        end
    end
end
