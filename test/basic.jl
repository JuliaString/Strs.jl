# This file is based on a file (julia/test/basic.jl) which is a part of Julia
# License is MIT: LICENSE.md

codegen_egal_of_strings(x, y) = (x===y, x!==y)

# test AbstractString functions at beginning of string.jl
struct tstStringType <: AbstractString
    data::Array{UInt8,1}
end
mutable struct CharStr <: AbstractString
    chars::Vector{Char}
    CharStr(x) = new(collect(x))
end
Base.start(x::CharStr) = start(x.chars)
Base.next(x::CharStr, i::Int) = next(x.chars, i)
Base.done(x::CharStr, i::Int) = done(x.chars, i)
Base.lastindex(x::CharStr) = lastindex(x.chars)

const IS_WORKING = false

function testbasic(::Type{ST}, ::Type{C}) where {ST, C}
    emptystr = ST("")
    a_str    = ST("a")
    ab_str   = ST("ab")
    abc_str  = ST("abc")
    abce_str = ST("abc!")
    abcd_str = ST("abcd")
    hello1   = ST("hello")
    hello2   = ST("hellø")

    c_chr    = cvtchar(C, 'c')

@testset "constructors" begin
    @test abce_str == "abc!"
    v = [0x61,0x62,0x63,0x21]
    @test ST(v) == abce_str # && isempty(v)
    ST === String && @test isempty(v)
    if IS_WORKING # Need to add constructor with range
        @test ST(0x61:0x63) == abc_str
    end

    # Check that resizing empty source vector does not corrupt string
    b = IOBuffer()
    write(b, ab_str)
    x = take!(b)
    s = ST(x)
    resize!(x, 0)
    empty!(x) # Another method which must be tested
    @test s == ab_str
    resize!(x, 1)
    @test s == ab_str

    @test isempty(ST(string()))
    @test eltype(GenericString) == Char
    @test firstindex(abc_str) == 1
    @test cmp(ab_str,abc_str) == -1
    @test typemin(ST) === ST("")
    @test typemin(abc_str) === ST("")
    @test abc_str === abc_str
    @test ab_str  !== abc_str
    if IS_WORKING
        @test string(ab_str, c_chr) === abc_str
        @test string() === ""
        @test codegen_egal_of_strings(string(ab_str, c_chr), abc_str) === (true, false)
    end
    let strs = [emptystr, a_str, ST("a b c"), ST("до свидания")]
        for x in strs, y in strs
            @test (x === y) == (objectid(x) == objectid(y))
        end
    end
end

@testset "{starts,ends}_with" begin
    @test starts_with(abcd_str, cvtchar(C,'a'))
    @test starts_with(abcd_str, a_str)
    @test starts_with(abcd_str, ab_str)
    @test !starts_with(ab_str, abcd_str)
    @test !starts_with(abcd_str, ST("bc"))
    @test ends_with(abcd_str, cvtchar(C,'d'))
    @test ends_with(abcd_str, ST("d"))
    @test ends_with(abcd_str, ST("cd"))
    @test !ends_with(abcd_str, ST("dc"))
    @test !ends_with(ST("cd"), abcd_str)
    @test starts_with(ST("ab\0cd"), ST("ab\0c"))
    @test !starts_with(ST("ab\0cd"), ST("ab\0d"))
end

@test filter(x -> x ∈ ['f', 'o'], "foobar") == "foo"

@testset "string iteration, and issue #1454" begin
    str = ST("é")
    str_a = vcat(str...)
    @test length(str_a)==1
    @test str_a[1] == str[1]

    str = ST("s\u2200")
    @test str[1:end] == str
end

@testset "sizeof" begin
    @test sizeof(abc_str) == 3
    @test sizeof(ST("\u2222")) == 3
end

# issue #3597
@test string(GenericString(ST("Test"))[1:1], ST("X")) == "TX"

@testset "parsing Int types" begin
    let b, n
    for T = (UInt8,Int8,UInt16,Int16,UInt32,Int32,UInt64,Int64,UInt128,Int128,BigInt),
            b = 2:62,
            _ = 1:10
        n = (T != BigInt) ? rand(T) : BigInt(rand(Int128))
        @test parse(T, ST(string(n, base = b)),  base = b) == n
    end
    end
end

@testset "Symbol and gensym" begin
    @test Symbol("asdf") === :asdf
    @test Symbol(:abc,"def",'g',"hi",0) === :abcdefghi0
    @test :a < :b
    @test starts_with(string(gensym("asdf")),"##asdf#")
    @test gensym("asdf") != gensym("asdf")
    @test gensym() != gensym()
    @test starts_with(string(gensym()),"##")
    @test_throws ArgumentError Symbol("ab\0")
    @test_throws ArgumentError gensym("ab\0")
end
@testset "issue #6949" begin
    f = IOBuffer()
    x = split(ST("1 2 3"))
    for c in x
        print(f, c) # was write
    end
    @test f.size == 3
    @test ST(take!(f)) == "123"
end

@testset "issue #7248" begin
    @test_throws BoundsError length(hello1, 1, -1)
    @test_throws BoundsError prevind(hello1, 0, 1)
    @test_throws BoundsError length(hello2, 1, -1)
    @test_throws BoundsError prevind(hello2, 0, 1)
    @test_throws BoundsError length(hello1, 1, 10)
    ST === String && @test nextind(hello1, 0, 10) == 10
    @test_throws BoundsError length(hello2, 1, 10) == 9
    @test nextind(ST("hellø"), 0, 10) == 11
    @test_throws BoundsError checkbounds(hello1, 0)
    @test_throws BoundsError checkbounds(hello1, 6)
    @test_throws BoundsError checkbounds(hello1, 0:3)
    @test_throws BoundsError checkbounds(hello1, 4:6)
    @test_throws BoundsError checkbounds(hello1, [0:3;])
    @test_throws BoundsError checkbounds(hello1, [4:6;])
    @test checkbounds(hello1, 1) === nothing
    @test checkbounds(hello1, 5) === nothing
    @test checkbounds(hello1, 1:3) === nothing
    @test checkbounds(hello1, 3:5) === nothing
    @test checkbounds(hello1, [1:3;]) === nothing
    @test checkbounds(hello1, [3:5;]) === nothing
    @test checkbounds(Bool, hello1, 0) === false
    @test checkbounds(Bool, hello1, 1) === true
    @test checkbounds(Bool, hello1, 5) === true
    @test checkbounds(Bool, hello1, 6) === false
    @test checkbounds(Bool, hello1, 0:5) === false
    @test checkbounds(Bool, hello1, 1:6) === false
    @test checkbounds(Bool, hello1, 1:5) === true
    @test checkbounds(Bool, hello1, [0:5;]) === false
    @test checkbounds(Bool, hello1, [1:6;]) === false
    @test checkbounds(Bool, hello1, [1:5;]) === true
end

    @testset "issue #15624 (indexing with out of bounds empty range)" begin
    @test emptystr[10:9] == ""
    @test hello1[10:9] == ""
    @test hello2[10:9] == ""
    @test SubString(hello1, 1, 5)[10:9] == ""
    @test SubString(hello1, 1, 0)[10:9] == ""
    @test SubString(hello2, 1, 5)[10:9] == ""
    @test SubString(hello2, 1, 0)[10:9] == ""
    @test SubString(emptystr, 1, 0)[10:9] == ""
    @test_throws BoundsError SubString(emptystr, 1, 6)
    @test_throws BoundsError SubString(emptystr, 1, 1)
end

@testset "issue #22500 (using `get()` to index strings with default returns)" begin
    utf8_str = ST("我很喜欢Julia")

    # Test that we can index in at valid locations
    @test get(utf8_str, 1, cvtchar(C,'X')) == '我'
    @test get(utf8_str, 13, cvtchar(C,'X')) == 'J'

    # Test that obviously incorrect locations return the default
    @test get(utf8_str, -1, cvtchar(C,'X')) == 'X'
    @test get(utf8_str, 1000, cvtchar(C,'X')) == 'X'

    # Test that indexing into the middle of a character throws
    @test_throws StringIndexError get(utf8_str, 2, cvtchar(C,'X'))
end

#=
# issue #7764
let
    srep = repeat("Σβ",2)
    s="Σβ"
    ss=SubString(s,1,lastindex(s))

    @test repeat(ss,2) == "ΣβΣβ"

    @test lastindex(srep) == 7

    @test next(srep, 3) == ('β',5)
    @test next(srep, 7) == ('β',9)

    @test srep[7] == 'β'
    @test_throws BoundsError srep[8]
end
=#

# This caused JuliaLang/JSON.jl#82
rng = C('\x00'):C('\x7f')
@test first(rng) === C('\x00')
@test last(rng) === C('\x7f')

# make sure substrings do not accept code unit if it is not start of codepoint
let s = ST("x\u0302")
    @test s[1:2] == s
    @test_throws BoundsError s[0:3]
    @test_throws BoundsError s[1:4]
    @test_throws StringIndexError s[1:3]
end

@testset "issue #9781" begin
    # parse(Float64, SubString) wasn't tolerant of trailing whitespace, which was different
    # to "normal" strings. This also checks we aren't being too tolerant and allowing
    # any arbitrary trailing characters.
    @test parse(Float64, ST("1\n")) == 1.0
    @test [parse(Float64,x) for x in split(ST("0,1\n"),ST(","))][2] == 1.0
    @test_throws ArgumentError parse(Float64,split(ST("0,1 X\n"),ST(","))[2])
    @test parse(Float32,ST("1\n")) == 1.0
    @test [parse(Float32,x) for x in split(ST("0,1\n"),ST(","))][2] == 1.0
    @test_throws ArgumentError parse(Float32,split(ST("0,1 X\n"),ST(","))[2])
end

@testset "AbstractString functions" begin
    tstr = tstStringType(unsafe_wrap(Vector{UInt8},"12"))
    @test_throws MethodError ncodeunits(tstr)
    @test_throws MethodError codeunit(tstr)
    @test_throws MethodError codeunit(tstr, 1)
    @test_throws MethodError codeunit(tstr, true)
    @test_throws MethodError isvalid(tstr, 1)
    @test_throws MethodError isvalid(tstr, true)
    @test_throws MethodError next(tstr, 1)
    @test_throws MethodError next(tstr, true)
    @test_throws MethodError lastindex(tstr)

    gstr = GenericString("12")
    @test string(gstr) isa GenericString

    @test Array{UInt8}(gstr) == [49, 50]
    @test Array{Char,1}(gstr) == ['1', '2']

    @test gstr[1] == '1'
    @test gstr[1:1] == "1"
    @test gstr[[1]] == "1"

    @test s"∀∃"[big(1)] == '∀'
    @test_throws StringIndexError GenericString("∀∃")[Int8(2)]
    @test_throws BoundsError GenericString("∀∃")[UInt16(10)]

    foobar = ST("foobar")
    
    @test first(eachindex(foobar)) === 1
    @test first(eachindex(ST(""))) === 1
    @test last(eachindex(foobar)) === lastindex(foobar)
    @test done(eachindex(foobar),7)
    @test Int == eltype(Base.EachStringIndex) ==
                 eltype(Base.EachStringIndex{String}) ==
                 eltype(Base.EachStringIndex{GenericString}) ==
                 eltype(eachindex(foobar)) == eltype(eachindex(gstr))
    @test map(uppercase, ST("foó")) == ST("FOÓ")
    @test nextind(ST("fóobar"), 0, 3) == 4

    @test Symbol(gstr) == Symbol("12")

    @test sizeof(gstr) == 2
    @test ncodeunits(gstr) == 2
    @test length(gstr) == 2
    @test length(GenericString("")) == 0

    @test nextind(1:1, 1) == 2
    @test nextind([1], 1) == 2

    @test length(gstr, 1, 2) == 2

    # no string promotion
    let svec = [s"12", GenericString("12"), SubString("123", 1, 2)]
        @test all(x -> x == "12", svec)
        @test svec isa Vector{AbstractString}
    end
end

@testset "issue #10307" begin
    @test typeof(map(x -> parse(Int16, x), AbstractString[])) == Vector{Int16}

    for T in [Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128]
        for i in [typemax(T), typemin(T)]
            s = "$i"
            @test tryparse(T, s) == i
        end
    end

    for T in [Int8, Int16, Int32, Int64, Int128]
        for i in [typemax(T), typemin(T)]
            f = "$(i)0"
            @test tryparse(T, f) === nothing
        end
    end
end

@testset "issue #11142" begin
    s = ST("abcdefghij")
    sp = pointer(s)
    @test unsafe_string(sp) == s
    @test unsafe_string(sp,5) == ST("abcde")
    @test typeof(unsafe_string(sp)) == String
    s = ST("abcde\uff\u2000\U1f596")
    sp = pointer(s)
    @test unsafe_string(sp) == s
    @test unsafe_string(sp,5) == ST("abcde")
    @test typeof(unsafe_string(sp)) == String

    @test tryparse(BigInt, ST("1234567890")) == BigInt(1234567890)
    @test tryparse(BigInt, ST("1234567890-")) === nothing

    @test tryparse(Float64, ST("64")) == 64.0
    @test tryparse(Float64, ST("64o")) === nothing
    @test tryparse(Float32, ST("32")) == 32.0f0
    @test tryparse(Float32, ST("32o")) === nothing
end

@testset "issue #10994: handle embedded NUL chars for string parsing" begin
    for T in [BigInt, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128]
        @test_throws ArgumentError parse(T, ST("1\0"))
    end
    for T in [BigInt, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128,
              Float64, Float32]
        @test tryparse(T, ST("1\0")) === nothing
    end
    let s = Unicode.normalize(ST("tést"), :NFKC)
        @test unsafe_string(Base.unsafe_convert(Cstring, Base.cconvert(Cstring, s))) == s
        @test unsafe_string(Base.unsafe_convert(Cstring, Symbol(s))) == s
    end
    @test_throws ArgumentError Base.unsafe_convert(Cstring, Base.cconvert(Cstring, ST("ba\0d")))

    cstrdup(s) = @static Sys.iswindows() ? ccall(:_strdup, Cstring, (Cstring,), s) : ccall(:strdup, Cstring, (Cstring,), s)
    let p = cstrdup(hello1)
        @test unsafe_string(p) == hello1
        Libc.free(p)
    end
end

@testset "iteration" begin
    str = ST("ḟøøƀäṙ")
    @test [c for c in str] == ['ḟ', 'ø', 'ø', 'ƀ', 'ä', 'ṙ']
    @test [i for i in eachindex(str)] == [1, 4, 6, 8, 10, 12]
    @test [x for x in enumerate(str)] ==
        [(1, 'ḟ'), (2, 'ø'), (3, 'ø'), (4, 'ƀ'), (5, 'ä'), (6, 'ṙ')]
end
@testset "isvalid edge conditions" begin
    for (val, pass) in (
            (0, true), (0xd7ff, true),
            (0xd800, false), (0xdfff, false),
            (0xe000, true), (0xffff, true),
            (0x10000, true), (0x10ffff, true),
            (0x110000, false)
        )
        @test is_valid(C, val) == pass
    end
    for (val, pass) in (
            ("\x00", true),
            ("\x7f", true),
            ("\x80", false),
            ("\xbf", false),
            ("\xc0", false),
            ("\xff", false),
            ("\xc0\x80", false),
            ("\xc1\x80", false),
            ("\xc2\x80", true),
            ("\xc2\xc0", false),
            ("\xed\x9f\xbf", true),
            ("\xed\xa0\x80", false),
            ("\xed\xbf\xbf", false),
            ("\xee\x80\x80", true),
            ("\xef\xbf\xbf", true),
            ("\xf0\x90\x80\x80", true),
            ("\xf4\x8f\xbf\xbf", true),
            ("\xf4\x90\x80\x80", false),
            ("\xf5\x80\x80\x80", false),
            ("\ud800\udc00", false),
            ("\udbff\udfff", false),
            ("\ud800\u0100", false),
            ("\udc00\u0100", false),
            ("\udc00\ud800", false),
        )
        @test is_valid(ST, val) == pass == is_valid(ST(val))
        @test is_valid(C, val[1]) == pass
    end

    # Issue #11203
    @test is_valid(ST, UInt8[]) == true == is_valid(ST(""))

    # Check UTF-8 characters
    # Check ASCII range (true),
    # then single continuation bytes and lead bytes with no following continuation bytes (false)
    for (rng,flg) in ((0:0x7f, true), (0x80:0xff, false))
        for byt in rng
            @test is_valid(ST, UInt8[byt]) == flg
        end
    end
    # Check overlong lead bytes for 2-character sequences (false)
    for byt = 0xc0:0xc1
        @test is_valid(ST, UInt8[byt,0x80]) == false
    end
    # Check valid lead-in to two-byte sequences (true)
    for byt = 0xc2:0xdf
        for (rng,flg) in ((0x00:0x7f, false), (0x80:0xbf, true), (0xc0:0xff, false))
            for cont in rng
                @test is_valid(ST, UInt8[byt, cont]) == flg
            end
        end
    end
    # Check three-byte sequences
    for r1 in (0xe0:0xec, 0xee:0xef)
        for byt = r1
            # Check for short sequence
            @test is_valid(ST, UInt8[byt]) == false
            for (rng,flg) in ((0x00:0x7f, false), (0x80:0xbf, true), (0xc0:0xff, false))
                for cont in rng
                    @test is_valid(ST, UInt8[byt, cont]) == false
                    @test is_valid(ST, UInt8[byt, cont, 0x80]) == flg
                end
            end
        end
    end
    # Check hangul characters (0xd000-0xd7ff) hangul
    # Check for short sequence, or start of surrogate pair
    for (rng,flg) in ((0x00:0x7f, false), (0x80:0x9f, true), (0xa0:0xff, false))
        for cont in rng
            @test is_valid(ST, UInt8[0xed, cont]) == false
            @test is_valid(ST, UInt8[0xed, cont, 0x80]) == flg
        end
    end
    # Check valid four-byte sequences
    for byt = 0xf0:0xf4
        if (byt == 0xf0)
            r0 = ((0x00:0x8f, false), (0x90:0xbf, true), (0xc0:0xff, false))
        elseif byt == 0xf4
            r0 = ((0x00:0x7f, false), (0x80:0x8f, true), (0x90:0xff, false))
        else
            r0 = ((0x00:0x7f, false), (0x80:0xbf, true), (0xc0:0xff, false))
        end
        for (rng,flg) in r0
            for cont in rng
                @test is_valid(ST, UInt8[byt, cont]) == false
                @test is_valid(ST, UInt8[byt, cont, 0x80]) == false
                @test is_valid(ST, UInt8[byt, cont, 0x80, 0x80]) == flg
            end
        end
    end
    # Check five-byte sequences, should be invalid
    for byt = 0xf8:0xfb
        @test is_valid(ST, UInt8[byt, 0x80, 0x80, 0x80, 0x80]) == false
    end
    # Check six-byte sequences, should be invalid
    for byt = 0xfc:0xfd
        @test is_valid(ST, UInt8[byt, 0x80, 0x80, 0x80, 0x80, 0x80]) == false
    end
    # Check seven-byte sequences, should be invalid
    @test is_valid(ST, UInt8[0xfe, 0x80, 0x80, 0x80, 0x80, 0x80]) == false

    # invalid Chars
    @test  is_valid(C('a'))
    @test  is_valid(C('柒'))
    @test !is_valid("\xff"[1])
    @test !is_valid("\xc0\x80"[1])
    @test !is_valid("\xf0\x80\x80\x80"[1])
    @test !is_valid('\ud800')
    @test  is_valid('\ud7ff')
    @test !is_valid('\udfff')
    @test  is_valid('\ue000')
end

@testset "NULL pointers are handled consistently by String" begin
    @test_throws ArgumentError unsafe_string(Ptr{UInt8}(0))
    @test_throws ArgumentError unsafe_string(Ptr{UInt8}(0), 10)
end
@testset "ascii for ASCII strings and non-ASCII strings" begin
    s = "Hello, world"
    str = ST(s)
    @test ascii(str) == s
    @test typeof(ascii(str)) == String
    @test ascii(GenericString(str)) == s
    @test typeof(ascii(GenericString(str))) == String
    @test_throws ArgumentError ascii(ST("Hello, ∀"))
    @test_throws ArgumentError ascii(GenericString(ST("Hello, ∀")))
end
@testset "issue #17271: lastindex() doesn't throw an error even with invalid strings" begin
    @test lastindex("\x90") == 1
    @test lastindex("\xce") == 1
end
# issue #17624, missing getindex method for String
@test abc_str[:] == "abc"

@testset "issue #18280: next/nextind must return past String's underlying data" begin
    for st in ("Hello", "Σ", "こんにちは", "😊😁")
        local s
        s = ST(st)
        @test next(s, lastindex(s))[2] > sizeof(s)
        @test nextind(s, lastindex(s)) > sizeof(s)
    end
end
# Test cmp with AbstractStrings that don't index the same as UTF-8, which would include
# (LegacyString.)UTF16String and (LegacyString.)UTF32String, among others.

@testset "cmp without UTF-8 indexing" begin
    # Simple case, with just ANSI Latin 1 characters
    @test ST("áB") != CharStr("áá") # returns false with bug
    @test cmp(ST("áB"), CharStr("áá")) == -1 # returns 0 with bug

    # Case with Unicode characters
    @test cmp(ST("\U1f596\U1f596"), CharStr("\U1f596")) == 1   # Gives BoundsError with bug
    @test cmp(CharStr("\U1f596"), ST("\U1f596\U1f596")) == -1
end

@testset "repeat" begin
    @inferred repeat(GenericString("x"), 1)
    @test repeat("xx",3) == repeat("x",6) == repeat('x',6) == repeat(GenericString("x"), 6) == "xxxxxx"
    @test repeat("αα",3) == repeat("α",6) == repeat('α',6) == repeat(GenericString("α"), 6) == "αααααα"
    @test repeat("x",1) == repeat('x',1) == "x"^1 == 'x'^1 == GenericString("x")^1 == "x"
    @test repeat("x",0) == repeat('x',0) == "x"^0 == 'x'^0 == GenericString("x")^0 == ""

    for S in ["xxx", "ååå", "∀∀∀", "🍕🍕🍕"]
        c = S[1]
        s = string(c)
        @test_throws ArgumentError repeat(c, -1)
        @test_throws ArgumentError repeat(s, -1)
        @test_throws ArgumentError repeat(S, -1)
        @test repeat(c, 0) == ""
        @test repeat(s, 0) == ""
        @test repeat(S, 0) == ""
        @test repeat(c, 1) == s
        @test repeat(s, 1) == s
        @test repeat(S, 1) == S
        @test repeat(c, 3) == S
        @test repeat(s, 3) == S
        @test repeat(S, 3) == S*S*S
    end
end
@testset "issue #12495: check that logical indexing attempt raises ArgumentError" begin
    @test_throws ArgumentError abc_str[[true, false, true]]
    @test_throws ArgumentError abc_str[BitArray([true, false, true])]
end

@testset "concatenation" begin
    @test ab_str * ST("cd") == abcd_str
    @test C('a') * ST("bc") == abc_str
    @test ab_str * C('c') == abc_str
    @test C('a') * C('b') == ab_str
    @test C('a') * ST("b") * C('c') == abc_str
    @test a_str * C('b') * C('c') == abc_str
end

@testset "unrecognized escapes in string/char literals" begin
    @test_throws Meta.ParseError Meta.parse(ST("\"\\.\""))
    @test_throws Meta.ParseError Meta.parse(ST("\'\\.\'"))
end

@testset "thisind" begin
    let strs = Any[ST("∀α>β:α+1>β"),
                   s"∀α>β:α+1>β",
                   SubString(ST("123∀α>β:α+1>β123"), 4, 18),
                   SubString(s"123∀α>β:α+1>β123", 4, 18)]
        for s in strs
            @test_throws BoundsError thisind(s, -2)
            @test_throws BoundsError thisind(s, -1)
            @test thisind(s, 0) == 0
            @test thisind(s, 1) == 1
            @test thisind(s, 2) == 1
            @test thisind(s, 3) == 1
            @test thisind(s, 4) == 4
            @test thisind(s, 5) == 4
            @test thisind(s, 6) == 6
            @test thisind(s, 15) == 15
            @test thisind(s, 16) == 15
            @test thisind(s, 17) == 17
            @test_throws BoundsError thisind(s, 18)
            @test_throws BoundsError thisind(s, 19)
        end
    end

    let strs = Any[ST(""), s"", SubString(ST("123"), 2, 1), SubString(s"123", 2, 1)]
        for s in strs
            @test_throws BoundsError thisind(s, -1)
            @test thisind(s, 0) == 0
            @test thisind(s, 1) == 1
            @test_throws BoundsError thisind(s, 2)
        end
    end
end

@testset "prevind and nextind" begin
    for s in Any[ST("∀α>β:α+1>β"), GenericString(ST("∀α>β:α+1>β"))]
        @test_throws BoundsError prevind(s, 0)
        @test_throws BoundsError prevind(s, 0, 0)
        @test_throws BoundsError prevind(s, 0, 1)
        @test prevind(s, 1) == 0
        @test prevind(s, 1, 1) == 0
        @test prevind(s, 1, 0) == 1
        @test prevind(s, 2) == 1
        @test prevind(s, 2, 1) == 1
        @test prevind(s, 4) == 1
        @test prevind(s, 4, 1) == 1
        @test prevind(s, 5) == 4
        @test prevind(s, 5, 1) == 4
        @test prevind(s, 5, 2) == 1
        @test prevind(s, 5, 3) == 0
        @test prevind(s, 15) == 14
        @test prevind(s, 15, 1) == 14
        @test prevind(s, 15, 2) == 13
        @test prevind(s, 15, 3) == 12
        @test prevind(s, 15, 4) == 10
        @test prevind(s, 15, 10) == 0
        @test prevind(s, 15, 9) == 1
        @test prevind(s, 16) == 15
        @test prevind(s, 16, 1) == 15
        @test prevind(s, 16, 2) == 14
        @test prevind(s, 17) == 15
        @test prevind(s, 17, 1) == 15
        @test prevind(s, 17, 2) == 14
        @test_throws BoundsError prevind(s, 18)
        @test_throws BoundsError prevind(s, 18, 0)
        @test_throws BoundsError prevind(s, 18, 1)

        @test_throws BoundsError nextind(s, -1)
        @test_throws BoundsError nextind(s, -1, 0)
        @test_throws BoundsError nextind(s, -1, 1)
        @test nextind(s, 0, 2) == 4
        @test nextind(s, 0, 20) == 26
        @test nextind(s, 0, 10) == 15
        @test nextind(s, 1) == 4
        @test nextind(s, 1, 1) == 4
        @test nextind(s, 1, 2) == 6
        @test nextind(s, 1, 9) == 15
        @test nextind(s, 1, 10) == 17
        @test nextind(s, 2) == 4
        @test nextind(s, 2, 1) == 4
        @test nextind(s, 3) == 4
        @test nextind(s, 3, 1) == 4
        @test nextind(s, 4) == 6
        @test nextind(s, 4, 1) == 6
        @test nextind(s, 14) == 15
        @test nextind(s, 14, 1) == 15
        @test nextind(s, 15) == 17
        @test nextind(s, 15, 1) == 17
        @test nextind(s, 15, 2) == 18
        @test nextind(s, 16) == 17
        @test nextind(s, 16, 1) == 17
        @test nextind(s, 16, 2) == 18
        @test nextind(s, 16, 3) == 19
        @test_throws BoundsError nextind(s, 17)
        @test_throws BoundsError nextind(s, 17, 0)
        @test_throws BoundsError nextind(s, 17, 1)

        for x in 0:ncodeunits(s)+1
            n = p = x
            for j in 1:40
                if 1 ≤ p
                    p = prevind(s, p)
                    @test prevind(s, x, j) == p
                end
                if n ≤ ncodeunits(s)
                    n = nextind(s, n)
                    @test nextind(s, x, j) == n
                end
            end
        end
    end
end

@testset "first and last" begin
    s = ST("∀ϵ≠0: ϵ²>0")
    @test_throws ArgumentError first(s, -1)
    @test first(s, 0) == ""
    @test first(s, 1) == "∀"
    @test first(s, 2) == "∀ϵ"
    @test first(s, 3) == "∀ϵ≠"
    @test first(s, 4) == "∀ϵ≠0"
    @test first(s, length(s)) == s
    @test first(s, length(s)+1) == s
    @test_throws ArgumentError last(s, -1)
    @test last(s, 0) == ""
    @test last(s, 1) == "0"
    @test last(s, 2) == ">0"
    @test last(s, 3) == "²>0"
    @test last(s, 4) == "ϵ²>0"
    @test last(s, length(s)) == s
    @test last(s, length(s)+1) == s
end

@testset "invalid code point" begin
    s = ST([0x61, 0xba, 0x41])
    @test !isvalid(s)
    @test s[2] == reinterpret(Char, UInt32(0xba) << 24)
end

@testset "ncodeunits" begin
    for (str, n) in [""     => 0, a_str   => 1, abc_str  => 3,
                   "α"    => 2, "abγ" => 4, "∀"    => 3,
                   "∀x∃y" => 8, "🍕"  => 4, "🍕∀" => 7]
        s = ST(str)
        @test ncodeunits(s) == n
        @test ncodeunits(GenericString(s)) == n
    end
end

@testset "0-step nextind and prevind" begin
    for T in [ST, SubString, Base.SubstitutionString, GenericString]
        e = convert(T, ST(""))
        @test nextind(e, 0, 0) == 0
        @test_throws BoundsError nextind(e, 1, 0)
        @test_throws BoundsError prevind(e, 0, 0)
        @test prevind(e, 1, 0) == 1

        s = convert(T, ST("∀x∃"))
        @test nextind(s, 0, 0) == 0
        @test nextind(s, 1, 0) == 1
        @test_throws StringIndexError nextind(s, 2, 0)
        @test_throws StringIndexError nextind(s, 3, 0)
        @test nextind(s, 4, 0) == 4
        @test nextind(s, 5, 0) == 5
        @test_throws StringIndexError nextind(s, 6, 0)
        @test_throws StringIndexError nextind(s, 7, 0)
        @test_throws BoundsError nextind(s, 8, 0)

        @test_throws BoundsError prevind(s, 0, 0)
        @test prevind(s, 1, 0) == 1
        @test_throws StringIndexError prevind(s, 2, 0)
        @test_throws StringIndexError prevind(s, 3, 0)
        @test prevind(s, 4, 0) == 4
        @test prevind(s, 5, 0) == 5
        @test_throws StringIndexError prevind(s, 6, 0)
        @test_throws StringIndexError prevind(s, 7, 0)
        @test prevind(s, 8, 0) == 8
    end
end

    # codeunit vectors

    let s = ST("∀x∃y"), u = codeunits(s)
        @test u isa Base.CodeUnits{UInt8,String}
        @test length(u) == ncodeunits(s) == 8
        @test sizeof(u) == sizeof(s)
        @test eltype(u) === UInt8
        @test size(u) == (length(u),)
        @test strides(u) == (1,)
        @test u[1] == 0xe2
        @test u[2] == 0x88
        @test u[8] == 0x79
        @test_throws ErrorException (u[1] = 0x00)
        @test collect(u) == b"∀x∃y"
    end

    # issue #24388
    let v = unsafe_wrap(Vector{UInt8}, abc_str)
        s = ST(v)
        @test_throws BoundsError v[1]
        push!(v, UInt8('x'))
        @test s == abc_str
    end

    # PR #25535
    let v = [0x40,0x41,0x42]
        @test ST(view(v, 2:3)) == "AB"
    end

    # make sure length for identical String and AbstractString return the same value, PR #25533
    let rng = MersenneTwister(1),
        strs = [ST("∀εa∀aε"*String(rand(rng, Char, 100))*"∀εa∀aε"), ST(rand(rng, Char, 200))]
        for s in strs, i in 1:ncodeunits(s)+1, j in 0:ncodeunits(s)
            @test length(s,i,j) == length(GenericString(s),i,j)
        end
        for i in 0:10, j in 1:100,
            s in [ST(randstring(rng, i)), ST(randstring(rng, "∀∃α1", i)), ST(rand(rng, UInt8, i))]
            @test length(s) == length(GenericString(s))
        end
    end

    # conversion of SubString to the same type, issue #25525
    let x = SubString(ab_str, 1, 1)
        y = convert(SubString{ST}, x)
        @test y === x
        chop(ab_str) === chop.([ab_str])[1]
    end
end

function testbin(::Type{ST}) where {ST}

    @test unsafe_wrap(Vector{UInt8},"\xcc\xdd\xee\xff\x80") == [0xcc,0xdd,0xee,0xff,0x80]

    for lst in (("a", "az", "a\xb1", "a\xb1z", "a\xb1\x83", "a\xb1\x83\x84", "a\xb1\x83\x84z"),
                ("\x81", "\x81z", "\x81\xb1", "\x81\xb1z", "\x81\xb1\x83", "\x81\xb1\x83\x84",
                 "\x81\xb1\x83\x84z"),
                ("\xf8", "\xf8z", "\xf8\x9f", "\xf8\x9fz", "\xf8\x9f\x98", "\xf8\x9f\x98z",
                 "\xf8\x9f\x98\x84", "\xf8\x9f\x98\x84z")),
        s in lst
        st = ST(s)
        @eval @test next($st, 1)[2] == 2
        @eval @test nextind($st, 1) == 2
    end

    for lst in ((("\xce", 2), ("\xcez", 2), ("\xce\xb1", 3), ("\xce\xb1z", 3),
                 ("\xce\xb1\x83", 3), ("\xce\xb1\x83\x84", 3), ("\xce\xb1\x83\x84z", 3)),
                (("\xe2", 2), ("\xe2z", 2), ("\xe2\x88", 3), ("\xe2\x88z", 3),
                 ("\xe2\x88\x83", 4), ("\xe2\x88\x83z", 4), ("\xe2\x88\x83\x84", 4),
                 ("\xe2\x88\x83\x84z", 4)),
                (("\xf0", 2), ("\xf0z", 2), ("\xf0\x9f", 3), ("\xf0\x9fz", 3),
                 ("\xf0\x9f\x98", 4), ("\xf0\x9f\x98z", 4), ("\xf0\x9f\x98\x84", 5),
                 ("\xf0\x9f\x98\x84z", 5))),
        s in lst
        st = ST(s)
        @eval @test next($st, 1)[2] == $r
        @eval @test nextind($st, 1) == $r
    end
end

for ST in UnicodeStringTypes
    C = eltype(ST)
    @testset "Basic String Tests: $ST, $C" begin testbasic(ST, C) end
end
for ST in (Text1Str, )
    @testset "Binary String Tests: $ST" begin testbin(ST) end
end

# This only works for String
@testset "issue #6027 - make symbol with invalid char" begin
    sym = Symbol(Char(0xdcdb))
    @test string(sym) == string(Char(0xdcdb))
    @test sym == string(Char(0xdcdb))
    @test Meta.lower(Main, sym) === sym
    res = string(Meta.parse(string(Char(0xdcdb)," = 1"),1,raise=false)[1])
    @test res == """\$(Expr(:error, "invalid character \\\"\\udcdb\\\"\"))"""
end
