## Test invalid sequences

# Continuation byte not after lead
for byt in 0x80:0xbf
    @test_throws UnicodeError check_string(UInt8[byt])
end

# Test lead bytes
for byt in 0xc0:0xff
    # Single lead byte at end of string
    @test_throws UnicodeError check_string(UInt8[byt])
    # Lead followed by non-continuation character < 0x80
    @test_throws UnicodeError check_string(UInt8[byt,0])
    # Lead followed by non-continuation character > 0xbf
    @test_throws UnicodeError check_string(UInt8[byt,0xc0])
end

# Test overlong 2-byte
for byt in 0x81:0xbf
    @test_throws UnicodeError check_string(UInt8[0xc0,byt])
end
for byt in 0x80:0xbf
    @test_throws UnicodeError check_string(UInt8[0xc1,byt])
end

# Test overlong 3-byte
for byt in 0x80:0x9f
    @test_throws UnicodeError check_string(UInt8[0xe0,byt,0x80])
end

# Test overlong 4-byte
for byt in 0x80:0x8f
    @test_throws UnicodeError check_string(UInt8[0xef,byt,0x80,0x80])
end

# Test 4-byte > 0x10ffff
for byt in 0x90:0xbf
    @test_throws UnicodeError check_string(UInt8[0xf4,byt,0x80,0x80])
end
for byt in 0xf5:0xf7
    @test_throws UnicodeError check_string(UInt8[byt,0x80,0x80,0x80])
end

# Test 5-byte
for byt in 0xf8:0xfb
    @test_throws UnicodeError check_string(UInt8[byt,0x80,0x80,0x80,0x80])
end

# Test 6-byte
for byt in 0xfc:0xfd
    @test_throws UnicodeError check_string(UInt8[byt,0x80,0x80,0x80,0x80,0x80])
end

# Test 7-byte
@test_throws UnicodeError check_string(UInt8[0xfe,0x80,0x80,0x80,0x80,0x80,0x80])

# Three and above byte sequences
for byt in 0xe0:0xef
    # Lead followed by only 1 continuation byte
    @test_throws UnicodeError check_string(UInt8[byt,0x80])
    # Lead ended by non-continuation character < 0x80
    @test_throws UnicodeError check_string(UInt8[byt,0x80,0])
    # Lead ended by non-continuation character > 0xbf
    @test_throws UnicodeError check_string(UInt8[byt,0x80,0xc0])
end

# 3-byte encoded surrogate character(s)
# Single surrogate
@test_throws UnicodeError check_string(UInt8[0xed,0xa0,0x80])
# Not followed by surrogate
@test_throws UnicodeError check_string(UInt8[0xed,0xa0,0x80,0xed,0x80,0x80])
# Trailing surrogate first
@test_throws UnicodeError check_string(UInt8[0xed,0xb0,0x80,0xed,0xb0,0x80])
# Followed by lead surrogate
@test_throws UnicodeError check_string(UInt8[0xed,0xa0,0x80,0xed,0xa0,0x80])

# Four byte sequences
for byt in 0xf0:0xf4
    # Lead followed by only 2 continuation bytes
    @test_throws UnicodeError check_string(UInt8[byt,0x80,0x80])
    # Lead followed by non-continuation character < 0x80
    @test_throws UnicodeError check_string(UInt8[byt,0x80,0x80,0])
    # Lead followed by non-continuation character > 0xbf
    @test_throws UnicodeError check_string(UInt8[byt,0x80,0x80,0xc0])
end

# Long encoding of 0x01

@test_throws UnicodeError utf8([0xf0, 0x80, 0x80, 0x80])
# Test ends of long encoded surrogates
@test_throws UnicodeError utf8([0xf0, 0x8d, 0xa0, 0x80])
@test_throws UnicodeError utf8([0xf0, 0x8d, 0xbf, 0xbf])
@test_throws UnicodeError check_string([0xf0, 0x80, 0x80, 0x80])
@test check_string([0xc0, 0x81]; accept_long_char=true) == (1,0x1,0,0,0,0,0)
@test check_string([0xf0, 0x80, 0x80, 0x80]; accept_long_char=true) == (1,0x1,0,0,0,0,0)

# Surrogates
@test_throws UnicodeError check_string(UInt16[0xd800])
@test_throws UnicodeError check_string(UInt16[0xdc00])
@test_throws UnicodeError check_string(UInt16[0xdc00,0xd800])

# Surrogates in UTF-32
@test_throws UnicodeError check_string(UInt32[0xd800])
@test_throws UnicodeError check_string(UInt32[0xdc00])
@test_throws UnicodeError check_string(UInt32[0xdc00,0xd800])

# Characters > 0x10ffff
@test_throws UnicodeError check_string(UInt32[0x110000])

# Test more invalid (overlong) sequences
for (seq, res) in (
    ([0xc0,0x80],          (1,1,0,0,0,0,0)),  # Long encoded Nul byte (Modified UTF-8, Java)
    ([0xed,0xa0,0x80,0xed,0xb0,0x80], (1,0x30,1,0,0,0,0)), # Overlong \U10000, (CESU-8)
    ([0xed,0xaf,0xbf,0xed,0xbf,0xbf], (1,0x30,1,0,0,0,0)), # Overlong \U10ffff, (CESU-8)
    ([0x0d800,0x0dc00],    (1,0x30,1,0,0,0,0)), # Overlong \U10000, (CESU-8)
    ([0x0dbff,0x0dfff],    (1,0x30,1,0,0,0,0))) # Overlong \U10ffff, (CESU-8)
    @test_throws UnicodeError check_string(seq)
    @test check_string(seq;accept_long_null=true, accept_surrogates=true, accept_long_char=true) == res
end

# Test conversions of invalid sequences

strval(::Type{UTF8Str}, dat) = dat
strval(::Union{Type{UTF16Str},Type{UTF32Str}}, dat) = convert(UTF8Str, dat)

for T in (UTF8Str, UTF16Str, UTF32Str)
    @testset "$T invalid sequences" begin
    # Continuation byte not after lead
    for byt in 0x80:0xbf
        @test_throws UnicodeError convert(T,  strval(T, UInt8[byt]))
    end

    # Test lead bytes
    for byt in 0xc0:0xff
        # Single lead byte at end of string
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt]))
        # Lead followed by non-continuation character < 0x80
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0]))
        # Lead followed by non-continuation character > 0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0xc0]))
    end

    # Test overlong 2-byte
    for byt in 0x81:0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[0xc0,byt]))
    end
    for byt in 0x80:0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[0xc1,byt]))
    end

    # Test overlong 3-byte
    for byt in 0x80:0x9f
        @test_throws UnicodeError convert(T, strval(T, UInt8[0xe0,byt,0x80]))
    end

    # Test overlong 4-byte
    for byt in 0x80:0x8f
        @test_throws UnicodeError convert(T, strval(T, UInt8[0xef,byt,0x80,0x80]))
    end

    # Test 4-byte > 0x10ffff
    for byt in 0x90:0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[0xf4,byt,0x80,0x80]))
    end
    for byt in 0xf5:0xf7
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80,0x80]))
    end

    # Test 5-byte
    for byt in 0xf8:0xfb
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80,0x80,0x80]))
    end

    # Test 6-byte
    for byt in 0xfc:0xfd
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80,0x80,0x80,0x80]))
    end

    # Test 7-byte
    @test_throws UnicodeError convert(T, strval(T, UInt8[0xfe,0x80,0x80,0x80,0x80,0x80,0x80]))

    # Three and above byte sequences
    for byt in 0xe0:0xef
        # Lead followed by only 1 continuation byte
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80]))
        # Lead ended by non-continuation character < 0x80
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0]))
        # Lead ended by non-continuation character > 0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0xc0]))
    end

    # 3-byte encoded surrogate character(s)
    # Single surrogate
    @test_throws UnicodeError convert(T, strval(T, UInt8[0xed,0xa0,0x80]))
    # Not followed by surrogate
    @test_throws UnicodeError convert(T, strval(T, UInt8[0xed,0xa0,0x80,0xed,0x80,0x80]))
    # Trailing surrogate first
    @test_throws UnicodeError convert(T, strval(T, UInt8[0xed,0xb0,0x80,0xed,0xb0,0x80]))
    # Followed by lead surrogate
    @test_throws UnicodeError convert(T, strval(T, UInt8[0xed,0xa0,0x80,0xed,0xa0,0x80]))

    # Four byte sequences
    for byt in 0xf0:0xf4
        # Lead followed by only 2 continuation bytes
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80]))
        # Lead followed by non-continuation character < 0x80
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80,0]))
        # Lead followed by non-continuation character > 0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80,0xc0]))
    end
    end
end
