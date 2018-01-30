charsets =
    (:Binary,  # really, no character set at all, not text
     :ASCII,   # (7-bit subset of Unicode)
     :Latin,   # ISO-8859-1 (8-bit subset of Unicode)
     :UCS2,    # BMP (16-bit subset of Unicode)
     :UTF32,   # corresponding to codepoints (0-0xd7ff, 0xe000-0x10fff)
     :UniPlus, # valid Unicode, plus unknown characters (for String)
     :Text1,   # Unknown character set, 1 byte
     :Text2,   # Unknown character set, 2 byte
     :Text4)   # Unknown character set, 4 byte

@testset "CharSet" begin
    for CT in charsets
        @test "$(typeof(CharSet(CT)))" == "CharSet{$(CT)}"
    end
end

@testset "Encoding" begin
    for CT in charsets
        @test "$(typeof(Encoding(CT)))" == "Encoding{$(CT)}"
    end
end
