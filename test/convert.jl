# issue #11551 (#11004,#10959)
function tstcvt(strUTF8::UTF8Str, strUTF16::UTF16Str, strUTF32::UTF32Str)
    @test utf16(strUTF8) == strUTF16
    @test utf32(strUTF8) == strUTF32
    @test utf8(strUTF16) == strUTF8
    @test utf32(strUTF16) == strUTF32
    @test utf8(strUTF32)  == strUTF8
    @test utf16(strUTF32) == strUTF16
end

# Create some ASCII, UTF8, UTF16, and UTF32 strings
let strAscii = to_ascii("abcdefgh"),
    strA_UTF8 = utf8(("abcdefgh\uff")[1:8]),
    strL_UTF8 = utf8("abcdef\uff\uff"),
    str2_UTF8 = utf8("abcd\uff\uff\u7ff\u7ff"),
    str3_UTF8 = utf8("abcd\uff\uff\u7fff\u7fff"),
    str4_UTF8 = utf8("abcd\uff\u7ff\u7fff\U7ffff"),
    #strS_UTF8 = UTF8Str([0xc3, 0xbf, 0xdf, 0xbf, 0xe7, 0xbf, 0xbf, 0xed, 0xa0, 0x80, 0xed, 0xb0, 0x80])
    strC_UTF8 = UTF8Str(vcat([0xc3, 0xbf, 0xdf, 0xbf, 0xe7, 0xbf, 0xbf], codeunits("\U10000")))
    #strz_UTF8 = UTF8Str([0xc3, 0xbf, 0xdf, 0xbf, 0xe7, 0xbf, 0xbf, 0x00])
    #strZ      = ['a'%UInt8, 'b'%UInt8, 'c'%UInt8, 'd'%UInt8, 0xc3, 0xbf, 0xdf, 0xbf, 0xe7, 0xbf, 0xbf, 0xc0, 0x80]

    strA_UTF16 = utf16(strA_UTF8)
    strL_UTF16 = utf16(strL_UTF8)
    str2_UTF16 = utf16(str2_UTF8)
    str3_UTF16 = utf16(str3_UTF8)
    str4_UTF16 = utf16(str4_UTF8)
    #strS_UTF16 = utf16(strS_UTF8)

    strA_UTF32 = utf32(strA_UTF8)
    strL_UTF32 = utf32(strL_UTF8)
    str2_UTF32 = utf32(str2_UTF8)
    str3_UTF32 = utf32(str3_UTF8)
    str4_UTF32 = utf32(str4_UTF8)
    #strS_UTF32 = utf32(strS_UTF8)

    @test utf8(strAscii) == strAscii
    @test utf16(strAscii) == strAscii
    @test utf32(strAscii) == strAscii

    tstcvt(strA_UTF8,strA_UTF16,strA_UTF32)
    tstcvt(strL_UTF8,strL_UTF16,strL_UTF32)
    tstcvt(str2_UTF8,str2_UTF16,str2_UTF32)
    tstcvt(str3_UTF8,str3_UTF16,str3_UTF32)
    tstcvt(str4_UTF8,str4_UTF16,str4_UTF32)

    #=
    # Test converting surrogate pairs
    @test utf16(strS_UTF8) == strC_UTF8
    @test utf32(strS_UTF8) == strC_UTF8
    @test utf8(strS_UTF16) == strC_UTF8
    @test utf32(strS_UTF16) == strC_UTF8
    @test utf8(strS_UTF32)  == strC_UTF8
    @test utf16(strS_UTF32) == strC_UTF8

    # Test converting overlong \0
    @test utf8(strZ)  == strz_UTF8
    @test utf16(UTF8Str(strZ)) == strz_UTF8
    @test utf32(UTF8Str(strZ)) == strz_UTF8
    =#
end
