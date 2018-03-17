# Test bounds checking
let b1 = codeunits("abcdef")
    @test_throws BoundsError check_string(b1, -10)
    @test_throws BoundsError check_string(b1, 0)
    @test_throws BoundsError check_string(b1, 7)
    @test_throws BoundsError check_string(b1, 3, -10)
    @test_throws BoundsError check_string(b1, 3, 0)
    @test_throws BoundsError check_string(b1, 3, 7)
    @test_throws UnicodeError check_string(b1, 3, 1)
end

let str = UTF8Str("this is a test\uff")
    @test_throws UnicodeError UTF8Str(vcat(codeunits("this is a test"), [0xed, 0x80]))
    # This next test was broken by #24999
    #@test next(str, 15) == ('\ufffd', 16)
    @test_throws BoundsError getindex(str, 0:3)
    @test_throws BoundsError getindex(str, 17:18)
    @test_throws BoundsError getindex(str, 2:17)
    # This next test was broken by #24999
    #@test string(Char(0x110000)) == "\ufffd"
    #=
    sa = SubString{ASCIIStr}(ascii("This is a silly test"), 1, 14)
    s8 = convert(SubString{UTF8Str}, sa)
    @test typeof(s8) == SubString{UTF8Str}
    @test s8 == "This is a sill"
    =#
    @test convert(UTF8Str, [0xed, 0x80, 0x80]) == "\ud000"
end
