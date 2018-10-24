Ideas:
Have substring support built in

Have types using both a UniStr type along with UTF-8 and/or UTF-16 versions, and take advantage of
knowing the real length of the string from the UniStr version, and always reading the string,
except for times when a UTF-8 (or UTF-16) version is needed, using the O(1) version.

Have extra parameter(s?), for types to hold things like: UnitRange{UInt16,UInt32,UInt64},
for substrings, UInt64 for caching hash value, Str{T}, where T could be Raw*, UTF8, or UTF16,
to cache either unchanged input data and/or validated UTF8/UTF16, and possibly even one of those with a substring type!

Add types _UCS2 and _UTF32, for the internal versions of those encodings, which are guaranteed to
have at least one character of that type, which allows for O(1) == comparisons between strings of
different types. [done!]

Having a mixed string type, for large strings where you may have occasional BMP or non-BMP characters, but the major portion is ASCII/Latin1.
   -> Keep a table of offsets and types, 2 bits per 64 character chunk for encoded type of chunk,
      offset table, 16-bits, 32-bits, or 64-bits?
      16-bits could handle 16+5-2 = 2^19 characters, max cost 16K for 512K character string,
      32-bits could handle up to 32GB character strings

Have a function that given a Vector{UInt8}, BinaryStr, UTF8Str, String, etc.
returns a vector of substring'ed UniStr, which only has to make copies if some of the lines
are _Latin, _UCS2, _UTF32.
(This could be done as well for input of Vector{UInt16}, RawWordStr)

To save space, and for better performance, lines with different types are pooled together,
and lines with ASCII can point to the original, except if the input is Vector{UInt8},
in which case they are also added to the pooled buffer for the lines.
In addition, depending on the percentage of characters still pointing into the original string
(again, only if it's not a Vector, which must be copied), it may decide to go ahead and copy
all of the ASCII substrings into the pool.
Note: for good substring performance, some of the operations that are optimized to work 8 bytes
(or more) at a time, will need to deal with masking the initial chunk, not just the final chunk.


New ideas:
Have a single concrete "UniStr" type, which uses bits in the trailing "nul" byte of the String
representation, to store the following information:

NotValidated, Invalid, NoASCII, Latin, BMP, UTF32, Hash present, Short

2 bits: 00 -> Valid, 01 -> Invalid, 10 -> NotValidated, 11 means?
1 bit:  0  -> Some ASCII, 1 -> no ASCII (bit flipped from others so that 0 -> ASCIIStr)
1 bit:  0  -> No Latin1,  1 -> some Latin1
1 bit:  0  -> ByteWise,   1 -> WordWise
1 bit:  0/1 Hash present
1 bit:  0/1 Short
1 bit:  ?

Extra byte for wordwise:
1 bit:  0  -> None > 0x7ff, 1 -> some > 0x7ff (for UTF8 conversions?)
1 bit:  0  -> No BMP,   1 -> some BMP (0x800-0xd7ff,0xe000-0xffff)
1 bit:  0  -> Only BMP, 1 -> some non-BMP (0x10000-0x10ffff)
Have at least 5 bits for other information.

So: ASCIIStr would be: Valid, All ASCII, ... i.e. 0 + short/hash bits
    _LatinStr would be: Valid, maybe no ascii, Latin1, no bmp, no non-bmp
    _UCS2Str  would be: Valid, maybe no ascii, maybe Latin1, some bmp, no non-bmp
    _UTF32Str would be: Valid, maybe no ascii, maybe Latin1, maybe BMP, some non-bmp


1) Pick up last 32 bits

Top byte all zero means: ASCII, Valid

7 00 Valid, 10 Invalid, 10 Not validated
6 
5 0 -> no Latin1, 1 -> some Latin1
4 0 -> None > 0x7ff, 1 -> some > 0x7ff (for UTF8 conversions?)
3 free
2 free
1 UCS2,  UTF32 (i.e. have at least extra 8 bits, maybe extra 24)
0 ASCII, 1 Latin1
-------
7 bits 16-23 hold 8-bit count, of # char > 0xff for UCS2, or > 0xffff for UTF32
6
5
4
3
2
1
0
------
lower 16 on UTF32 holds 16-bit count, # char > 0xff & <= 0xffff

max of count means unknown (not counted)
max-1 means that value or more
