# Test starting and different position
@test check_string(UInt32[0x110000, 0x1f596], 2) == (1,0x10,1,0,0,0,0)

# Test valid sequences
for (seq, res) in (
    ([0x0],                (1,0,0,0,0,0,0)),  # Nul byte, beginning of ASCII range
    ([0x7f],               (1,0,0,0,0,0,0)),  # End of ASCII range
    ([0xc2,0x80],          (1,2,0,0,0,1,0)),  # \u80, beginning of Latin1 range
    ([0xc3,0xbf],          (1,2,0,0,0,1,0)),  # \uff, end of Latin1 range
    ([0xc4,0x80],          (1,4,0,0,1,0,0)),  # \u100, beginning of non-Latin1 2-byte range
    ([0xdf,0xbf],          (1,4,0,0,1,0,0)),  # \u7ff, end of non-Latin1 2-byte range
    ([0xe0,0xa0,0x80],     (1,8,0,1,0,0,0)),  # \u800, beginning of 3-byte range
    ([0xed,0x9f,0xbf],     (1,8,0,1,0,0,0)),  # \ud7ff, end of first part of 3-byte range
    ([0xee,0x80,0x80],     (1,8,0,1,0,0,0)),  # \ue000, beginning of second part of 3-byte range
    ([0xef,0xbf,0xbf],     (1,8,0,1,0,0,0)),  # \uffff, end of 3-byte range
    ([0xf0,0x90,0x80,0x80],(1,16,1,0,0,0,0)), # \U10000, beginning of 4-byte range
    ([0xf4,0x8f,0xbf,0xbf],(1,16,1,0,0,0,0)), # \U10ffff, end of 4-byte range
    ([0x0000],             (1,0,0,0,0,0,0)),  # Nul byte, beginning of ASCII range
    ([0x007f],             (1,0,0,0,0,0,0)),  # End of ASCII range
    ([0x0080],             (1,2,0,0,0,1,0)),  # Beginning of Latin1 range
    ([0x00ff],             (1,2,0,0,0,1,0)),  # End of Latin1 range
    ([0x0100],             (1,4,0,0,1,0,0)),  # Beginning of non-Latin1 2-byte range
    ([0x07ff],             (1,4,0,0,1,0,0)),  # End of non-Latin1 2-byte range
    ([0x0800],             (1,8,0,1,0,0,0)),  # Beginning of 3-byte range
    ([0xd7ff],             (1,8,0,1,0,0,0)),  # End of first part of 3-byte range
    ([0xe000],             (1,8,0,1,0,0,0)),  # Beginning of second part of 3-byte range
    ([0xffff],             (1,8,0,1,0,0,0)),  # End of 3-byte range
    ([0xd800,0xdc00],      (1,16,1,0,0,0,0)), # \U10000, beginning of 4-byte range
    ([0xdbff,0xdfff],      (1,16,1,0,0,0,0)), # \U10ffff, end of 4-byte range
    ([0x00000],            (1,0,0,0,0,0,0)),  # Nul byte, beginning of ASCII range
    ([0x0007f],            (1,0,0,0,0,0,0)),  # End of ASCII range
    ([0x00080],            (1,2,0,0,0,1,0)),  # Beginning of Latin1 range
    ([0x000ff],            (1,2,0,0,0,1,0)),  # End of Latin1 range
    ([0x00100],            (1,4,0,0,1,0,0)),  # Beginning of non-Latin1 2-byte range
    ([0x007ff],            (1,4,0,0,1,0,0)),  # End of non-Latin1 2-byte range
    ([0x00800],            (1,8,0,1,0,0,0)),  # Beginning of 3-byte range
    ([0x0d7ff],            (1,8,0,1,0,0,0)),  # End of first part of 3-byte range
    ([0x0e000],            (1,8,0,1,0,0,0)),  # Beginning of second part of 3-byte range
    ([0x0ffff],            (1,8,0,1,0,0,0)),  # End of 3-byte range
    ([0x10000],            (1,16,1,0,0,0,0)), # \U10000, beginning of 4-byte range
    ([0x10ffff],           (1,16,1,0,0,0,0))) # \U10ffff, end of 4-byte range
    @test check_string(seq) == res
end
