test_string_length = 1:256

AllCharTypes = (ASCIIChr, LatinChr, UCS2Chr, UTF32Chr);
compat_types = Dict(ASCIIStr => (ASCIIChr, ), LatinStr => (ASCIIChr, LatinChr), UCS2Str => (ASCIIChr, LatinChr, UCS2Chr))
for T in (String, UTF8Str, UTF16Str, UTF32Str)
    compat_types[T] = AllCharTypes
end
string_types = keys(compat_types)

##  create type specific test strings
test_strings_base = Dict()
for T in AllCharTypes
    test_strings_base[T] = [String([convert(Char, randchar(T)%UInt32) for i = 1:len]) for len in test_string_length]
end

@testset "constructors" begin
    for (ST, type_list) in compat_types, CT in type_list, test_string in test_strings_base[CT]
        @eval @test convert($ST, $test_string) == test_string
    end
end

@testset "empty strings" begin
    for ST in keys(compat_types)
        @eval @test is_empty(convert($ST, ""))
    end
end

@testset "{starts,ends}with" begin
    for (ST, type_list) in compat_types, CT in type_list, test_string in test_strings_base[CT]
        converted_string = convert(ST, test_string)
        try
        @test starts_with(converted_string, test_string[1])
        starts_with(converted_string, test_string[1]) ||
            println("starts_with($converted_string, $(test_string[1])): $ST, $CT")
        @test ends_with(converted_string, test_string[end])
        ends_with(converted_string, test_string[end]) ||
            println("ends_with($converted_string, $(test_string[end])): $ST, $CT")
        catch ex
            println("Error: $converted_string, $(test_string[end]): $ST, $CT")
            println(sprint(showerror, ex, catch_backtrace()))
        end
        ##   TODO needs test which would run in case the start and end chars are the same
        if test_string[1] != test_string[end]
            @test !starts_with(converted_string, test_string[end])
            @test !ends_with(converted_string, test_string[1])
        end
    end
end
