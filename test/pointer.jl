# Test pointer() functions
let str = to_ascii("this ")
    u8  = utf8(str)
    u16 = utf16(str)
    u32 = utf32(str)
    pa  = pointer(str)
    p8  = pointer(u8)
    p16 = pointer(u16)
    p32 = pointer(u32)
    @test typeof(pa) == Ptr{UInt8}
    @test unsafe_load(pa,1) == 0x74
    @test typeof(p8) == Ptr{UInt8}
    @test unsafe_load(p8,1) == 0x74
    @test typeof(p16) == Ptr{UInt16}
    @test unsafe_load(p16,1) == 0x74
    @test typeof(p32) == Ptr{UInt32}
    @test unsafe_load(p32,1) == 0x74
    pa  = pointer(str, 2)
    p8  = pointer(u8,  2)
    p16 = pointer(u16, 2)
    p32 = pointer(u32, 2)
    @test typeof(pa) == Ptr{UInt8}
    @test unsafe_load(pa,1) == 0x68
    @test typeof(p8) == Ptr{UInt8}
    @test unsafe_load(p8,1) == 0x68
    @test typeof(p16) == Ptr{UInt16}
    @test unsafe_load(p16,1) == 0x68
    @test typeof(p32) == Ptr{UInt32}
    @test unsafe_load(p32,1) == 0x68
    sa  = SubString{ASCIIStr}(str, 3, 5)
    s8  = SubString{UTF8Str}(u8,   3, 5)
    s16 = SubString{UTF16Str}(u16, 3, 5)
    s32 = SubString{UTF32Str}(u32, 3, 5)
    pa  = pointer(sa)
    p8  = pointer(s8)
    p16 = pointer(s16)
    p32 = pointer(s32)
    @test typeof(pa) == Ptr{UInt8}
    @test unsafe_load(pa,1) == 0x69
    @test typeof(p8) == Ptr{UInt8}
    @test unsafe_load(p8,1) == 0x69
    @test typeof(p16) == Ptr{UInt16}
    @test unsafe_load(p16,1) == 0x69
    @test typeof(p32) == Ptr{UInt32}
    @test unsafe_load(p32,1) == 0x69
    pa  = pointer(sa, 2)
    p8  = pointer(s8,  2)
    p16 = pointer(s16, 2)
    p32 = pointer(s32, 2)
    @test typeof(pa) == Ptr{UInt8}
    @test unsafe_load(pa,1) == 0x73
    @test typeof(p8) == Ptr{UInt8}
    @test unsafe_load(p8,1) == 0x73
    @test typeof(p16) == Ptr{UInt16}
    @test unsafe_load(p16,1) == 0x73
    @test typeof(p32) == Ptr{UInt32}
    @test unsafe_load(p32,1) == 0x73
end

