""""
String literals with Swift-like format

Copyright 2016-2018 Gandalf Software, Inc., Scott P. Jones
Portions originally from code in Julia, copyright Julia contributors
Licensed under MIT License, see LICENSE.md
"""
module Literals end

export @f_str, @F_str, @pr_str, @PR_str, @pr, @PR
#export s_unescape_string, s_escape_string, s_print_unescaped, s_print_escaped

@static if V6_COMPAT
    const _parse = parse
    const _ParseError = ParseError
    _sprint(f, s) = sprint(endof(s), f, s)
    _sprint(f, s, c) = sprint(endof(s), f, s, c)
else
    const _parse = Meta.parse
    const _ParseError = Base.Meta.ParseError
    _sprint(f, s) = sprint(f, s; sizehint=lastindex(s))
    _sprint(f, s, c) = sprint(f, s, c; sizehint=lastindex(s))
end

"""
String macro with more Swift-like syntax, plus support for emojis and LaTeX names
"""
macro f_str(str) ; s_interp_parse(false, UniStr, str) ; end
macro f_str(str, args...) ; for v in args ; dump(v) end ; s_interp_parse(false, UniStr, str) ; end

"""
String macro with more Swift-like syntax, plus support for emojis and LaTeX names, also legacy
"""
macro F_str(str) ; s_interp_parse(true, UniStr, str) ; end

"""
String macros that calls print directly
"""
macro pr_str(str) ; s_print(false, str) ; end
macro PR_str(str) ; s_print(true, str) ; end

throw_arg_err(msg) = throw(ArgumentError(msg))
throw_arg_err(msg, val) = throw_arg_err(string(msg, repr(val)))

"""
Handle Unicode character constant, of form \\u{<hexdigits>}
"""
function s_parse_unicode(io, str,  pos)
    done(str, pos) && throw_arg_err("Incomplete \\u{...} in ", str)
    chr, pos = next(str, pos)
    chr != '{' && throw_arg_err("\\u missing opening { in ", str)
    done(str, pos) && throw_arg_err("Incomplete \\u{...} in ", str)
    beg = pos
    chr, pos = next(str, pos)
    num::UInt32 = 0
    cnt = 0
    while chr != '}'
        done(str, pos) && throw_arg_err("\\u{ missing closing } in ", str)
        (cnt += 1) > 6 && throw_arg_err("Unicode constant too long in ", str)
        num = num<<4 + chr - ('0' <= chr <= '9' ? '0' :
                              'a' <= chr <= 'f' ? 'a' - 10 :
                              'A' <= chr <= 'F' ? 'A' - 10 :
                              throw_arg_err("\\u missing closing } in ", str))
        chr, pos = next(str, pos)
    end
    cnt == 0 && throw_arg_err("\\u{} has no hex digits in ", str)
    ((0x0d800 <= num <= 0x0dfff) || num > 0x10ffff) &&
        throw_arg_err("Invalid Unicode character constant ", str[beg-3:pos-1])
    print(io, Char(num))
    pos
end

check_extended(chr) = false
parse_extended(io, str, pos, chr) = error("No literal extensions loaded")
parse_fmt(io, str, pos, chr) = error("No format parser loaded")
parse_pyfmt(io, str, pos, chr) = error("No Python format parser loaded")

"""
String interpolation parsing, allow legacy \$, \\xHH, \\uHHHH, \\UHHHHHHHH
"""
s_print_unescaped_legacy(io, str::AbstractString) = s_print_unescaped(io, str, true)

"""
String interpolation parsing
Based on code resurrected from Julia base:
https://github.com/JuliaLang/julia/blob/deab8eabd7089e2699a8f3a9598177b62cbb1733/base/string.jl
"""
function s_print_unescaped(io, str::AbstractString, flg::Bool=false)
    pos = start(str)
    while !done(str, pos)
        chr, pos = next(str, pos)
        if !done(str, pos) && chr == '\\'
            chr, pos = next(str, pos)
            if (chr == 'u' ||  chr == 'U' || chr == 'x')
                if flg
                    pos = s_parse_legacy(io, str, pos, chr)
                elseif chr == 'u'
                    pos = s_parse_unicode(io, str, pos)
                else
                    throw_arg_err(string("\\", chr, " only supported in legacy mode (i.e. ",
                                         "F\"...\" or PR\"...\""))
                end
            elseif check_extended(chr)
                pos = parse_extended(io, str, pos, chr)
            else
                chr = (chr == '0' ? '\0' :
                       chr == '$' ? '$'  :
                       chr == '"' ? '"'  :
                       chr == '\'' ? '\'' :
                       chr == '\\' ? '\\' :
                       chr == 'a' ? '\a' :
                       chr == 'b' ? '\b' :
                       chr == 't' ? '\t' :
                       chr == 'n' ? '\n' :
                       chr == 'v' ? '\v' :
                       chr == 'f' ? '\f' :
                       chr == 'r' ? '\r' :
                       chr == 'e' ? '\e' :
                       throw_arg_err(string("Invalid \\", chr, " sequence in "), str))
                write(io, UInt8(chr))
            end
        else
            print(io, chr)
        end
    end
end

hexerr(chr) = throw_arg_err("\\$chr used with no following hex digits")

function s_parse_legacy(io, str, pos, chr)
    done(str, pos) && hexerr(chr)
    beg = pos
    max = chr == 'x' ? 2 : chr == 'u' ? 4 : 8
    if str[pos] == '{'
        max == 4 || throw_arg_err("{ only allowed with \\u")
        return s_parse_unicode(io, str, pos)
    end
    num = cnt = 0
    while (cnt += 1) <= max && !done(str, pos)
        chr, nxt = next(str, pos)
        num = '0' <= chr <= '9' ? num << 4 + chr - '0' :
              'a' <= chr <= 'f' ? num << 4 + chr - 'a' + 10 :
              'A' <= chr <= 'F' ? num << 4 + chr - 'A' + 10 : break
        pos = nxt
    end
    cnt == 1 && hexerr(chr)
    if max == 2
        write(io, UInt8(num))
    elseif is_valid(UTF32Chr, num)
        print(io, UTF32Chr(num))
    else
        throw_arg_err("Invalid Unicode character constant ", str[beg-2:pos-1])
    end
    pos
end

s_unescape_string(str::AbstractString) = _sprint(s_print_unescaped, str)

function s_print_escaped(io, str::AbstractString, esc::Union{AbstractString, AbsChar})
    pos = start(str)
    while !done(str, pos)
        chr, pos = next(str, pos)
        chr == '\0'         ? print(io, "\\0") :
        chr == '\e'         ? print(io, "\\e") :
        chr == '\\'         ? print(io, "\\\\") :
        chr in esc          ? print(io, '\\', chr) :
        '\a' <= chr <= '\r' ? print(io, '\\', "abtnvfr"[Int(chr)-6]) :
        is_printable(chr)   ? print(io, chr) :
                              print(io, "\\u{", outhex(chr%UInt32), "}")
    end
end

s_escape_string(str::AbstractString) = _sprint(s_print_escaped, str, '\"')

s_print(flg::Bool, str::AbstractString) =
    s_print(flg, str, flg ? s_unescape_str : s_unescape_legacy)

function s_print(flg::Bool, str::AbstractString, unescape::Function)
    sx = s_interp_parse_vec(flg, str, unescape)
    (length(sx) == 1 && isa(sx[1], String)
     ? Expr(:call, :print, sx[1])
     : Expr(:call, :print, sx...))
end

function s_interp_parse(flg::Bool, ::Type{S}, str::AbstractString,
                        unescape::Function, p::Function) where {S<:AbstractString}
    sx = s_interp_parse_vec(flg, str, unescape)
    ((length(sx) == 1 && isa(sx[1], String)) ? sx[1]
     : Expr(:call, :convert, S, Expr(:call, :sprint, p, sx...)))
end

function s_interp_parse_vec(flg::Bool, s::AbstractString, unescape::Function)
    sx = []
    i = j = start(s)
    while !done(s, j)
        c, k = next(s, j)
        if c == '\\' && !done(s, k)
            if s[k] == '('
                # Handle interpolation
                is_empty(s[i:j-1]) ||
                    push!(sx, unescape(s[i:j-1]))
                ex, j = _parse(s, k, greedy=false)
                isa(ex, Expr) && (ex.head === :continue) &&
                    throw(_ParseError("Incomplete expression"))
                push!(sx, esc(ex))
                i = j
            elseif s[k] == '%'
                i = j = parse_fmt(sx, s, unescape, i, j, k)
            elseif s[k] == '{'
                i = j = parse_pyfmt(sx, s, unescape, i, j, k)
           elseif flg && s[k] == '$'
                is_empty(s[i:j-1]) ||
                    push!(sx, unescape(s[i:j-1]))
                i = k
                # Move past \\, c should point to '$'
                c, j = next(s, k)
            else
                j = k
            end
        elseif flg && c == '$'
            is_empty(s[i:j-1]) ||
                push!(sx, unescape(s[i:j-1]))
            ex, j = _parse(s, k, greedy=false)
            isa(ex,Expr) && ex.head === :continue &&
                throw(_ParseError("incomplete expression"))
            push!(sx, esc(ex))
            i = j
        else
            j = k
        end
    end
    is_empty(s[i:end]) ||
        push!(sx, unescape(s[i:j-1]))
    sx
end

function s_unescape_str(str)
    str = s_unescape_string(str)
    is_valid(String, str) ? str : throw_arg_err("Invalid UTF-8 sequence")
end
function s_unescape_legacy(str)
    str = _sprint(s_print_unescaped_legacy, str)
    is_valid(String, str) ? str : throw_arg_err("Invalid UTF-8 sequence")
end

s_interp_parse(flg::Bool, ::Type{S}, str::AbstractString, u::Function) where {S<:AbstractString} =
    s_interp_parse(flg, S, str, u, print)
s_interp_parse(flg::Bool,  ::Type{S}, str::AbstractString) where {S<:AbstractString} =
    s_interp_parse(flg, S, str, flg ? s_unescape_legacy : s_unescape_str)
