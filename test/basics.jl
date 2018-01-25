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
    clist = String[]
    for i in test_string_length
        push!(clist, String([convert(Char, randchar(T)%UInt32) for j in 1:i]))
    end
    test_strings_base[T] = clist
end


@testset "constructors" begin
    for (ST, type_list) in compat_types, CT in type_list, test_string in test_strings_base[CT]
        @test convert(ST, test_string) == test_string

        emptystr = convert(ST, "")
        @test isempty(emptystr)
    end
end

@testset "{starts,ends}with" begin
    for (ST, type_list) in compat_types, CT in type_list, test_string in test_strings_base[CT]
        converted_string = convert(ST, test_string)
        @test startswith(converted_string, test_string[1])
        @test endswith(converted_string, test_string[end])
        ##   TODO needs test which would run in case the start and end chars are the same
        if test_string[1] != test_string[end]
            @test !startswith(converted_string, test_string[end])
            @test !endswith(converted_string, test_string[1])
        end
    end
end
