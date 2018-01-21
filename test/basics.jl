test_string_length = 100

#string_types = [ASCIIStr, LatinStr, UTF8Str, UCS2Str, UTF16Str, UTF32Str, UniStr]
#string_types = [ASCIIStr, LatinStr, UTF8Str, UCS2Str, UTF16Str, UTF32Str]
string_types = [ASCIIStr, LatinStr]

##  create type specific test strings
test_strings_base = Dict()
for string_type in string_types
    tmax = UInt32(typemax(Strs.codepoint_type(string_type)))
    random_seq = rand(1:tmax, test_string_length)
    test_string = []
    for random_val in random_seq
        push!(test_string, Char(random_val))
    end
    test_string = join(test_string)
    test_strings_base["$string_type"] = test_string
end


test_strings_dict = Dict(
    "ASCIIStr" => [test_strings_base["ASCIIStr"]],
    "LatinStr" => [test_strings_base["ASCIIStr"], test_strings_base["LatinStr"]]
)

@testset "constructors" begin
    for string_type in string_types
        #@test convert(string_type, [0x61,0x62,0x63,0x21]) == "abc!"
        #@test convert(string_type, "abc!") == "abc!"

        emptystr = convert(string_type, "")
        @test isempty(emptystr)
        str_ab = convert(string_type, "ab")
        str_abc = convert(string_type, "abc")
        @test start("abc") == 1
        # let strs = [convert(string_type, ""),
        #             convert(string_type, "a"),
        #             convert(string_type, "a b c"),
        #             convert(string_type, "до свидания")]
        #     for x in strs, y in strs
        #         @test (x === y) == (object_id(x) == object_id(y))
        #     end
        # end
    end
end

@testset "{starts,ends}with" begin
    i = 1
    for string_type in string_types
        test_strings = test_strings_dict["$string_type"]
        for test_string in test_strings
            converted_string = convert(string_type, test_string)
            @test startswith(converted_string, test_string[1])
            @test !startswith(converted_string, test_string[end])
            @test endswith(converted_string, test_string[end])
            @test !endswith(converted_string, test_string[1])
            i += 1
        end
    end
end
