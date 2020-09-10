const openers = ('"', '\'', '(', '{', '[')
const closerdict = Dict('"'=>'"',
                        '\''=>'\'',
                        '('=>')',
                        '['=>']',
                        '{'=>'}')
const closers = (collect(values(closerdict))...,)
closerdict['/'] = '/'  # / is a weird delimiter since it also means division

function lex(io::IO)
    lexed = AbstractLex[]
    buf = IOBuffer()
    pos = 1
    lex!(io, buf, lexed, pos)
    return lexed
end

function lex!(io, buf, lexed, pos, cterminate = '\0')
    inrelation = false   # true if we're lexing "=e=", "=g=", or "=l="
    innumber = false     # true if we're lexing a number (starts with digit, then contains 0-9, ., e, E, and + or -)
    hadexponent = false  # true if we've seen e/E while lexing a number. Ensures that 3-5 isn't considered a single number.
    prevbool = false     # op that returns a boolean, only relevant for lexing "<=" and ">="
    while !eof(io)
        c = read(io, Char)
        if pos == 1 && (c == '*' || c == '$')
            # Comment line
            readline(io)
            continue
        end
        pos += 1
        if c != '='
            prevbool = false
        end
        if c == cterminate
            str = String(take!(buf))
            if !isempty(str)
                tag!(lexed, str)
            end
            return pos
        end
        isopener = c ∈ openers
        if isdigit(c)
            if position(buf) == 0
                innumber = true
            end
        elseif innumber && c == '.'
        elseif innumber && c ∈ ('e', 'E')
            hadexponent = true
        elseif innumber && c ∈ ('+', '-')
            if !hadexponent
                innumber = false
            end
        else
            innumber = hadexponent = false
        end
        if c == '/'
            # We have to distinguish division from "set" notation, so look backward for
            # a keyword
            i = lastindex(lexed)
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
            prelc = lowercase(pre)
            if prelc ∈ gamskws
                push!(lexed, Keyword(prelc))
                pre = prelc = ""
            end
            if (c == '(' || c == '[')
                args = AbstractLex[]
                pos = lex!(io, buf, args, pos, closerdict[c])
                if c == '(' && prelc ∈ funcnames
                    push!(lexed, GCall(prelc, args))
                elseif isempty(pre)
                    push!(lexed, Parens(args))
                else
                    push!(lexed, GArray(pre, (filter(x->x!=GText(","), args)...,)))
                end
            else
                write(buf, c)
                pos = lex_delim(buf, io, closerdict[c], pos)
                tag_delim!(lexed, pre * String(take!(buf)))
            end
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
            push!(lexed, GText(","))
        elseif innumber && c ∈ ('+', '-', '.')
            write(buf, c)
        elseif c == '=' && prevbool
            write(buf, c)  # ensure <= doesn't get split into GText("<"), GText("=")
        elseif c ∈ ('+', '-', '*', '/', '^', '=', '.')
            # Test for =E= and similar relations
            if inrelation
                write(buf, c)
                if c == '='
                    tag!(lexed, lowercase(String(take!(buf))))
                end
                inrelation = false
                continue
            end
            if c == '=' && peek(io) ∈ ('e', 'E', 'g', 'G', 'l', 'L')
                tag!(lexed, String(take!(buf)))
                write(buf, c)
                inrelation = true
                continue
            end
            tag!(lexed, String(take!(buf)))
            if c ∈ ('*', '.') && !eof(io)
                # Translate "x**y" into "x^y"
                c2 = Char(peek(io))
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
            if c ∈ ('<', '>')
                prevbool = true
            end
            write(buf, c)
        end
    end
    if position(buf) > 0
        tag!(lexed, String(take!(buf)))
    end
    return pos
end

function lex_delim(buf, io, cmatch, pos)
    local c
    while !eof(io)
        c = read(io, Char)
        pos += 1
        write(buf, c)
        c == cmatch && break
        if cmatch ∉ ('"', '\'')  # text in quotes should not be interpreted
            if c ∈ openers       # use recursion to ensure balanced parens
                pos = lex_delim(buf, io, closerdict[c], pos)
                continue
            end
            c ∈ closers && break
        end
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
    elseif f == l == '\''
        substr = str[2:end-1]
        if isnumberstring(substr)
            val = GNumber(parse(isintegerstring(substr) ? Int : Float64, substr))
        else
            val = GText(substr)
        end
        return push!(lexed, val)
    elseif f == l == '/'
        return push!(lexed, Slashed(strip(str[2:end-1])))
    elseif l == ')' || l == ']' # don't check f because this could be a function call or arrayref
        return push!(lexed, GText(str))
    end
    error("delim ", str, " not handled")
end

function tag!(lexed, str)
    isempty(str) && return nothing
    if isnumberstring(str)
        return push!(lexed, isintegerstring(str) ? GNumber{Int}(parse(Int, str)) : GNumber{Float64}(parse(Float64, str)))
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
            array = GArray(m.captures[1], (GText.(strip.(split(m.captures[2], ',')))...,))
            break
        end
        write(buf, c)
        if c ∈ openers
            pos = lex_delim(buf, io, closerdict[c], pos)
        else
            @assert(isnumeric(c) || isletter(c))
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
    i = 1;
    # Fixing double Dots(".") issue
    while i <= length(lexed)
        if lexed[i] == lexed[i+1] == Dots(".")
            splice!(lexed, i)
            lexed[i] = Dots("..")
        else i +=1;
        end
    end
    return pos
end

function lex(file::AbstractString)
    """ Parses a .gms file into Array{AbstractLex, 1}. """
    return open(file) do io
        lex(io)
    end
end

function isnumberstring(str)
    m = match(r"^[+-]?[0-9]+\.?[0-9]*([eE][+-]?[0-9]*)?$", str)
    return m !== nothing
end

isintegerstring(str) = match(r"[\.eE]", str) === nothing
