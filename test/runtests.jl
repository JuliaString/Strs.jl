# This file includes code that was formerly a part of Julia.
# License is MIT: LICENSE.md

const V6_COMPAT = VERSION < v"0.7.0-DEV"

@static V6_COMPAT ? (using Base.Test) : (using Test, Random, Unicode)

using Strs
import Strs: check_string, UTF_ERR_SHORT, UnicodeError, codepoint_adj, codepoint_rng

const IndexError = isdefined(Base, :StringIndexError) ? StringIndexError : UnicodeError

# Add definitions not present in v0.6.2 for GenericString
@static if V6_COMPAT
    Strs.ncodeunits(s::GenericString) = ncodeunits(s.string)
    Strs.codeunit(s::GenericString) = codeunit(s.string)
    Strs.codeunit(s::GenericString, i::Integer) = codeunit(s.string, i)
end
const CodeUnits = @static V6_COMPAT ? Strs.CodeUnits : Base.CodeUnits

# Should test GenericString also, once overthing else is working
const UnicodeStringTypes = (String, UTF8Str, )
    # (String, UTF16Str, UTF32Str, UniStr, UTF8Str)
    # (String, UTF8Str)
const ASCIIStringTypes = (String, UTF8Str, ASCIIStr, LatinStr)
    #    (UnicodeStringTypes..., ASCIIStr, LatinStr, UCS2Str)

function cvtchar(T, ch)
    try 
        T(ch)
    catch
        Text4Chr(ch)
    end
end

include("basic.jl")

@testset "Invalid sequences" begin include("invalid.jl")  end
@testset "Valid sequences"   begin include("valid.jl")    end
@testset "Bounds Errors"     begin include("bounds.jl")   end
@testset "UTF-16 tests"      begin include("utf16.jl")    end
@testset "UTF-32 tests"      begin include("utf32.jl")    end
@testset "Conversion errors" begin include("convert.jl")  end
@testset "Pointer functions" begin include("pointer.jl")  end
@testset "Search functions"  begin include("search.jl")   end
@testset "SubStrings"        begin include("substr.jl")   end
@testset "Utility functions" begin include("util.jl")     end
@testset "IO functions"      begin include("io.jl")       end
