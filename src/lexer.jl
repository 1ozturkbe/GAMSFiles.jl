const openers = ('"', '\'', '(', '{', '[')
const closerdict = Dict('"'=>'"',
                        '\''=>'\'',
                        '('=>')',
                        '['=>']',
                        '{'=>'}')
const closers = (collect(values(closerdict))...)
closerdict['/'] = '/'  # / is a weird delimiter since it also means division

function lex(io::IO)
    lexed = AbstractLex[]
    buf = IOBuffer()
    pos = 1
    inrelation = innumber = false
    while !eof(io)
        c = read(io, Char)
        if pos == 1 && (c == '*' || c == '$')
            # Comment line
            readline(io)
            continue
        end
        pos += 1
        isopener = c ∈ openers
        if isdigit(c)
            if position(buf) == 0
                innumber = true
            end
        elseif innumber && c ∈ ('.', 'e', 'E', '+', '-')
        else
            innumber = false
        end
        if c == '/'
            # We have to distinguish division from "set" notation, so look backward for
            # a keyword
            i = endof(lexed)
            while i > 0
                lexi = lexed[i]
                if lexi isa StatementEnd
                    break
                end
                if lexi isa Keyword
                    isopener = lexi.text ∈ ("sets", "scalars", "parameters", "models") ? true : false
                    break
                end
                i -= 1
            end
        end
        if isopener
            pre = String(take!(buf))
            if lowercase(pre) ∈ gamskws
                push!(lexed, Keyword(lowercase(pre)))
                pre = ""
            end
            write(buf, c)
            pos = lex_delim(buf, io, closerdict[c], pos)
            tag_delim!(lexed, pre * String(take!(buf)))
        elseif isspace(c)
            if c == '\n'
                pos = 1
            end
            if position(buf) > 0
                tag!(lexed, String(take!(buf)))
                if lexed[end] == Keyword("table")
                    # Tables are space-sensitive, lex them seperately
                    pos = lex_table!(buf, io, lexed, pos)
                end
            end
        elseif c == ';'
            tag!(lexed, String(take!(buf)))
            push!(lexed, StatementEnd())
        elseif c == ','
            tag!(lexed, String(take!(buf)))
        elseif innumber && c ∈ ('+', '-', '.')
            write(buf, c)
        elseif c ∈ ('+', '-', '*', '/', '^', '=', '.')
            # if position(buf) > 0
            #     lc = lastuint8(buf)
            #     if c ∈ ('+', '-') && (lc == UInt8('e') || lc == UInt8('E'))
            #         # This is probably scientific notation, don't interrupt it
            #         write(buf, c)
            #         continue
            #     elseif c == '.' && 0x30 <= lc <= 0x39
            #         # Decimal point
            #         write(buf, c)
            #         continue
            #     end
            # end
            # Test for =E= and similar relations
            if inrelation
                write(buf, c)
                if c == '='
                    tag!(lexed, String(take!(buf)))
                end
                inrelation = false
                continue
            end
            if c == '=' && peekchar(io) ∈ ('e', 'E', 'g', 'G', 'l', 'L')
                tag!(lexed, String(take!(buf)))
                write(buf, c)
                inrelation = true
                continue
            end
            tag!(lexed, String(take!(buf)))
            if c ∈ ('*', '.') && !eof(io)
                # Translate "x**y" into "x^y"
                c2 = peekchar(io)
                if c2 == c == '*'
                    read(io, Char)
                    push!(lexed, GText("^"))
                elseif c2 == c == '.'
                    read(io, Char)
                    push!(lexed, Dots(".."))
                else
                    push!(lexed, c == '.' ? Dots(".") : GText(string(c)))
                end
            else
                if c == '.'
                    push!(lexed, Dots("."))
                else
                    push!(lexed, GText(string(c)))
                end
            end
        else
            write(buf, c)
        end
    end
    if position(buf) > 0
        tag!(lexed, String(take!(buf)))
    end
    return lexed
end

function lex_delim(buf, io, cmatch, pos)
    local c
    while !eof(io)
        c = read(io, Char)
        pos += 1
        write(buf, c)
        c == cmatch && break
        if c ∈ openers
            pos = lex_delim(buf, io, closerdict[c], pos)
            continue
        end
        c ∈ closers && break
    end
    c == cmatch && return pos
    c ∈ closers && error("closing got $c, wanted $cmatch. Rest of line:\n", readline(io))
    error("never encountered $cmatch before end of file")
end

matcharray(str) = match(r"^([a-zA-Z][a-zA-Z0-9]*)\((.*)\)$", str)

function tag_delim!(lexed, str)
    m = matcharray(str)
    if m != nothing
        return push!(lexed, GArray(m.captures[1], (strip.(split(m.captures[2], ','))...,)))
    end
    f, l = str[1], str[end]
    if f == l == '"'
        return push!(lexed, GText(str[2:end-1]))
    elseif f == l == '/'
        return push!(lexed, Slashed(strip(str[2:end-1])))
    elseif l == ')' || l == ']' # don't check f because this could be a function call or arrayref
        return push!(lexed, GText(str))
    end
    error("delim ", str, " not handled")
end

function tag!(lexed, str)
    isempty(str) && return nothing
    m = match(r"^[+-]?[0-9]+\.?[0-9]*([eE][+-]?[0-9]*)?$", str)
    if m != nothing
        m = match(r"[\.eE]", str)
        return push!(lexed, m == nothing ? GNumber{Int}(parse(Int, str)) : GNumber{Float64}(parse(Float64, str)))
    end
    lc = lowercase(str)
    if lc ∈ gamskws
        return push!(lexed, Keyword(get(stdkws, lc, lc)))
    end
    push!(lexed, GText(str))
end

function lex_table!(buf, io, lexed, pos)
    local array
    c = read(io, Char)
    pos += 1
    while !eof(io) && (c == ' ' || c == '\t')
        c = read(io, Char)
        pos += 1
    end
    while true
        if isspace(c)
            str = String(take!(buf))
            m = matcharray(str)
            array = GArray(m.captures[1], (strip.(split(m.captures[2], ','))...,))
            break
        end
        write(buf, c)
        if c ∈ openers
            pos = lex_delim(buf, io, closerdict[c], pos)
        else
            @assert(isalnum(c))
        end
        eof(io) && error("table should not have ended yet")
        c = read(io, Char)
        pos += 1
    end
    if c != '\n'
        str = readline(io)
        if !isempty(str)
            push!(lexed, GText(str))
        end
    end
    body, term = readupto(io, ';')
    push!(lexed, Table(array, body))
    return pos
end

function lex(file::AbstractString)
    return open(file) do io
        lex(io)
    end
end
