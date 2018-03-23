""""
Enhanced string literals

String literals with Swift-like format
C and Python like formatting
LaTex, Emoji, HTML, and Unicode names

Copyright 2016-2018 Gandalf Software, Inc., Scott P. Jones
Portions originally from code in Julia, copyright Julia contributors
Licensed under MIT License, see LICENSE.md
"""
module Literals end

#using Format
#using StrTables, LaTeX_Entities, HTML_Entities, Unicode_Entities, Emoji_Entities

#export @f_str, @F_str, @sinterpolate, @pr_str, @PR_str, @pr, @PR
#export s_unescape_string, s_escape_string, s_print_unescaped, s_print_escaped

@static if VERSION < v"0.7.0-DEV"
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
String macro with more Swift-like syntax
"""
macro str_str(str)     ; _str(s_interp_parse(false, str)) ; end
macro str_str(str, cs) ; dump(cs) ; _str(s_interp_parse(false, str)) ; end

"""
String macros that calls print directly
"""
macro prn_str(str)     ; s_print(false, str) ; end
macro prn_str(str, cs) ; s_print(false, str) ; end

throw_arg_err(msg) = throw(ArgumentError(msg))
throw_arg_err(msg, s) = throw_arg_err(string(msg, repr(s)))

"""
Handle Unicode character constant, of form \\u{<hexdigits>}
"""
function s_parse_unicode(io, s,  i)
    done(s,i) &&
        throw_arg_err("Incomplete \\u{...} in ", s)
    c, i = next(s, i)
    c != '{' &&
        throw_arg_err("\\u missing opening { in ", s)
    done(s,i) &&
        throw_arg_err("Incomplete \\u{...} in ", s)
    beg = i
    c, i = next(s, i)
    n::UInt32 = 0
    k = 0
    while c != '}'
        done(s, i) &&
            throw_arg_err("\\u{ missing closing } in ", s)
        (k += 1) > 6 &&
            throw_arg_err("Unicode constant too long in ", s)
        n = n<<4 + c - ('0' <= c <= '9' ? '0' :
                        'a' <= c <= 'f' ? 'a' - 10 :
                        'A' <= c <= 'F' ? 'A' - 10 :
                        throw_arg_err("\\u missing closing } in ", s))
        c, i = next(s,i)
    end
    k == 0 &&
        throw_arg_err("\\u{} has no hex digits in ", s)
    ((0x0d800 <= n <= 0x0dfff) || n > 0x10ffff) &&
        throw_arg_err("Invalid Unicode character constant ", s[beg-3:i-1])
    print(io, Char(n))
    i
end

#=
"""
Handle Emoji character, of form \\:name:
"""
function s_parse_emoji(io, s,  i)
    beg = i # start location
    c, i = next(s, i)
    while c != ':'
        done(s, i) &&
            throw_arg_err("\\: missing closing : in ", s)
        c, i = next(s, i)
    end
    emojistr = lookupname(Emoji_Entities.default, s[beg:i-2])
    emojistr == "" &&
        throw_arg_err("Invalid Emoji name in ", s)
    print(io, emojistr)
    i
end

"""
Handle LaTeX character/string, of form \\<name>
"""
function s_parse_latex(io, s,  i)
    beg = i # start location
    c, i = next(s, i)
    while c != '>'
        done(s, i) &&
            throw_arg_err("\\< missing closing > in ", s)
        c, i = next(s, i)
    end
    latexstr = lookupname(LaTeX_Entities.default, s[beg:i-2])
    latexstr == "" &&
        throw_arg_err("Invalid LaTeX name in ", s)
    print(io, latexstr)
    i
end

"""
Handle HTML character/string, of form \\&name;
"""
function s_parse_html(io, s,  i)
    beg = i # start location
    c, i = next(s, i)
    while c != ';'
        done(s, i) &&
            throw_arg_err("\\& missing ending ; in ", s)
        c, i = next(s, i)
    end
    htmlstr = lookupname(HTML_Entities.default, s[beg:i-2])
    htmlstr == "" &&
        throw_arg_err("Invalid HTML name in ", s)
    print(io, htmlstr)
    i
end

"""
Handle Unicode name, of form \\N{name}, from Python
"""
function s_parse_uniname(io, s,  i)
    done(s, i) &&
        throw_arg_err("\\N incomplete in ", s)
    c, i = next(s, i)
    c != '{' &&
        throw_arg_err("\\N missing initial { in ", s)
    done(s, i) &&
        throw_arg_err("\\N{ incomplete in ", s)
    beg = i # start location
    c, i = next(s, i)
    while c != '}'
        done(s, i) &&
            throw_arg_err("\\N{ missing closing } in ", s)
        c, i = next(s, i)
    end
    unistr = lookupname(Unicode_Entities.default, s[beg:i-2])
    unistr == "" &&
        throw_arg_err("Invalid Unicode name in ", s)
    print(io, unistr)
    i
end
=#

"""
String interpolation parsing, allow legacy \$, \\xHH, \\uHHHH, \\UHHHHHHHH
"""
s_print_unescaped_legacy(io, s::AbstractString) = s_print_unescaped(io, s, true)

"""
String interpolation parsing
Based on code resurrected from Julia base:
https://github.com/JuliaLang/julia/blob/deab8eabd7089e2699a8f3a9598177b62cbb1733/base/string.jl
"""
function s_print_unescaped(io, s::AbstractString, flg::Bool=false)
    i = start(s)
    while !done(s,i)
        c, i = next(s,i)
        if !done(s,i) && c == '\\'
            c, i = next(s,i)
            if c == 'u'
                i = flg ? s_parse_unicode_legacy(io, s, i, c) : s_parse_unicode(io, s, i)
#=
            elseif c == ':'     # Emoji
                i = s_parse_emoji(io, s, i)
            elseif c == '&'     # HTML
                i = s_parse_html(io, s, i)
            elseif c == '<'     # LaTeX
                i = s_parse_latex(io, s, i)
            elseif c == 'N'     # Unicode name
                i = s_parse_uniname(io, s, i)
=#
            elseif flg && c == 'U'
                i = s_parse_unicode_legacy(io, s, i, c)
            elseif flg && c == 'x' # hex byte
                i = s_parse_hex_legacy(io, s, i)
            else
                c = (c == '0' ? '\0' :
                     c == '$' ? '$'  :
                     c == '"' ? '"'  :
                     c == '\'' ? '\'' :
                     c == '\\' ? '\\' :
                     c == 'a' ? '\a' :
                     c == 'b' ? '\b' :
                     c == 't' ? '\t' :
                     c == 'n' ? '\n' :
                     c == 'v' ? '\v' :
                     c == 'f' ? '\f' :
                     c == 'r' ? '\r' :
                     c == 'e' ? '\e' :
                     throw_arg_err(string("Invalid \\",c," sequence in "), s))
                write(io, UInt8(c))
            end
        else
            print(io, c)
        end
    end
end

function s_parse_unicode_legacy(io, s, i, c)
    done(s, i) &&
        throw_arg_err(string("\\", c, " used with no following hex digits"))
    beg = i
    m = (c == 'u' ? 4 : 8)
    if s[i] == '{'
        m == 4 || throw_arg_err("{ only allowed with \\u")
        return s_parse_unicode(io, s, i)
    end
    n = k = 0
    while (k += 1) <= m && !done(s,i)
        c, j = next(s,i)
        n = '0' <= c <= '9' ? n<<4 + c-'0' :
            'a' <= c <= 'f' ? n<<4 + c-'a'+10 :
            'A' <= c <= 'F' ? n<<4 + c-'A'+10 : break
        i = j
    end
    k == 1 &&
        throw_arg_err(string("\\", c, " used with no following hex digits"))
    ((0x0d800 <= n <= 0x0dfff) || n > 0x10ffff) &&
        throw_arg_err("Invalid Unicode character constant ", s[beg-2:i-1])
    print(io, Char(n))
    i
end

hexerr() = throw_arg_err("\\x used with no following hex digits")
function s_parse_hex_legacy(io, s, i)
    done(s,i) && hexerr()
    c, i = next(s,i)
    n = '0' <= c <= '9' ? c-'0' :
        'a' <= c <= 'f' ? c-'a'+10 :
        'A' <= c <= 'F' ? c-'A'+10 : hexerr()
    if done(s,i)
        write(io, UInt8(n))
        return i
    end
    c, j = next(s,i)
    n = '0' <= c <= '9' ? n<<4 + c-'0' :
        'a' <= c <= 'f' ? n<<4 + c-'a'+10 :
        'A' <= c <= 'F' ? n<<4 + c-'A'+10 : (j = i; n)
    write(io, UInt8(n))
    j
end

s_unescape_string(s::AbstractString) = _sprint(s_print_unescaped, s)

function s_print_escaped(io, s::AbstractString, esc::Union{AbstractString, Char})
    i = start(s)
    while !done(s,i)
        c, i = next(s, i)
        c == '\0'       ? print(io, "\\0") :
        c == '\e'       ? print(io, "\\e") :
        c == '\\'       ? print(io, "\\\\") :
        c in esc        ? print(io, '\\', c) :
        '\a' <= c <= '\r' ? print(io, '\\', "abtnvfr"[Int(c)-6]) :
        isprint(c)      ? print(io, c) :
                          print(io, "\\u{", outhex(c), "}")
    end
end

s_escape_string(s::AbstractString) = _sprint(s_print_escaped, s, '\"')

s_print(flg::Bool, s::AbstractString) = s_print(flg, s, flg ? s_unescape_str : s_unescape_legacy)
function s_print(flg::Bool, s::AbstractString, unescape::Function)
    sx = s_interp_parse_vec(flg, s, unescape)
    (length(sx) == 1 && isa(sx[1], String)
     ? Expr(:call, :print, sx[1])
     : Expr(:call, :print, sx...))
end

function s_interp_parse(flg::Bool, s::AbstractString, unescape::Function, p::Function)
    sx = s_interp_parse_vec(flg, s, unescape)
    length(sx) == 1 && isa(sx[1], String) ? sx[1] : Expr(:call, :sprint, p, sx...)
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
                # Move past \\, c should point to '%'
                c, k = next(s, k)
                done(s, k) && throw(_ParseError("Incomplete % expression"))
                # Handle interpolation
                if !is_empty(s[i:j-1])
                    push!(sx, unescape(s[i:j-1]))
                end
                if s[k] == '('
                    # Need to find end to parse to
                    _, j = _parse(s, k, greedy=false)
                    # This is a bit hacky, and probably doesn't perform as well as it could,
                    # but it works! Same below.
                    str = string("(StringLiterals.fmt", s[k:j-1], ')')
                else
                    # Move past %, c should point to letter
                    beg = k
                    while true
                        c, k = next(s, k)
                        done(s, k) &&
                            throw(_ParseError("Incomplete % expression"))
                        s[k] == '(' && break
                    end
                    _, j = _parse(s, k, greedy=false)
                    str = string("(StringLiterals.cfmt(\"", s[beg-1:k-1], "\",", s[k+1:j-1], ')')
                end
                ex, _ = _parse(str, 1, greedy=false)
                isa(ex, Expr) && (ex.head === :continue) &&
                    throw(_ParseError("Incomplete expression"))
                push!(sx, esc(ex))
                i = j
            elseif s[k] == '{'
                # Move past \\, c should point to '{'
                c, k = next(s, k)
                done(s, k) &&
                    throw(_ParseError("Incomplete {...} Python format expression"))
                # Handle interpolation
                is_empty(s[i:j-1]) ||
                    push!(sx, unescape(s[i:j-1]))
                beg = k # start location
                c, k = next(s, k)
                while c != '}'
                    done(s, k) &&
                        throw_arg_err("\\{ missing closing } in ", s)
                    c, k = next(s, k)
                end
                done(s, k) &&
                    throw(_ParseError("Missing (expr) in Python format expression"))
                c, k = next(s, k)
                c != '(' &&
                    throw(_ParseError(string("Missing (expr) in Python format expression: ", c)))
                # Need to find end to parse to
                _, j = _parse(s, k-1, greedy=false)
                # This is a bit hacky, and probably doesn't perform as well as it could,
                # but it works! Same below.
                str = string("(StringLiterals.pyfmt(\"", s[beg:k-3], "\",", s[k:j-1], ')')
                ex, _ = _parse(str, 1, greedy=false)
                isa(ex, Expr) && (ex.head === :continue) &&
                    throw(_ParseError("Incomplete expression"))
                push!(sx, esc(ex))
                i = j
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

function s_unescape_str(s)
    s = s_unescape_string(s)
    is_valid(String, s) ? s : throw_arg_err("Invalid UTF-8 sequence")
end
function s_unescape_legacy(s)
    s = _sprint(s_print_unescaped_legacy, s)
    is_valid(String, s) ? s : throw_arg_err("Invalid UTF-8 sequence")
end

s_interp_parse(flg::Bool, s::AbstractString, u::Function) = s_interp_parse(flg, s, u, print)
s_interp_parse(flg::Bool, s::AbstractString) =
    s_interp_parse(flg, s, flg ? s_unescape_legacy : s_unescape_str)
