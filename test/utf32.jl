## UTF-32 tests

let u8 = "\U10ffff\U1d565\U1d7f6\U00066\U2008a",
    u32 = utf32(u8)

    @test sizeof(u32) == 20
    @test length(u32) == 5
    @test utf8(u32) == u8
    @test collect(u8) == collect(u32)
    @test_throws UnicodeError utf32(UInt8[1,2,3])
end
