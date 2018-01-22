## UTF-32 tests

u8 = "\U10ffff\U1d565\U1d7f6\U00066\U2008a"
u32 = utf32(u8)
@test sizeof(u32) == 20
@test length(u32) == 5
@test utf8(u32) == u8
@test collect(u8) == collect(u32)
#@test u8 == utf32(u32.data) == utf32(copy!(Vector{UInt8}(20), 1, reinterpret(UInt8, u32.data), 1, 20))
#@test u8 == utf32(pointer(u32)) == utf32(convert(Ptr{Int32}, pointer(u32)))
@test_throws UnicodeError utf32(UInt8[1,2,3])
