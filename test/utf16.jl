## UTF-16 tests

let u8 = "\U10ffff\U1d565\U1d7f6\U00066\U2008a",
    u16 = utf16(u8)

    @test sizeof(u16) == 18
    @test length(u16) == 5
    #@test utf8(u16) == u8
    @test collect(u8) == collect(u16)
    #@test_throws UnicodeError utf16(utf32(Char(0x120000)))
    @test_throws UnicodeError utf16(UInt8[1,2,3])

    @test convert(UTF16Str, "test") == "test"
    @test convert(UTF16Str, u16) == u16

    # Do we really need to convert Arrays that aren't Vectors?
    #@test convert(UTF16Str, UInt16[[0x65, 0x66] [0x67, 0x68]]) == "efgh"
    #@test convert(UTF16Str, Int16[[0x65, 0x66] [0x67, 0x68]]) == "efgh"

    @test map(lowercase, utf16("TEST\U1f596")) == "test\U1f596"
    #@test typeof(Base.unsafe_convert(Ptr{UInt16}, utf16("test"))) == Ptr{UInt16}
end
