"""
    gams = parsegams(filename)

Do low-level lexing and parsing, but do not try to compile into a module.
"""
function parsegams(lexed::Vector{AbstractLex})
    gams = Dict{String,Any}()
    i = 1
    while i <= length(lexed)
        stmt = lexed[i]
        if stmt isa StatementEnd
            # do nothing
        elseif stmt isa Keyword
            kwname = stmt.text
            if kwname == "sets"
                sets = getdefault!(gams, kwname, Dict{String,Any})
                newsets = Dict{String,Vector{String}}()
                i = parse_slashed!(newsets, lexed, i)
                # Scan for i*j range declarations
                for (k, s) in newsets
                    if length(s) == 1
                        m = match(r"(\d+)\s*\*\s*(\d+)", s[1])
                        if m != nothing
                            lo, hi = parse(Int, m.captures[1]), parse(Int, m.captures[2])
                            sets[k] = lo == 1 ? Base.OneTo(hi) : (lo:hi)
                        else
                            sets[k] = s
                        end
                    else
                        sets[k] = s
                    end
                end
            elseif kwname == "alias"
                alias = lexed[i+=1]
                @assert(alias isa Parens && length(alias.args) == 3 && alias.args[2] == GText(","))
                sets = gams["sets"]
                symold, symnew = string(alias.args[1]), string(alias.args[3])
                if !haskey(sets, symold)  # GAMS allows arbitrary ordering
                    symnew, symold = symold, symnew
                end
                sets[symnew] = sets[symold]
            elseif kwname == "scalars" || kwname == "parameters"
                params = getdefault!(gams, "parameters", Dict{Any,Any})
                i = parse_slashed!(params, lexed, i; parseitems=false)
            elseif kwname == "table"
                table = getdefault!(gams, "tables", Dict{Any,Any})
                item = lexed[i+=1]
                table[item.name] = item.body
                # Some GAMS files declare it a parameter and then a table; remove it
                # from parameters
                if haskey(gams, "parameters")
                    params = gams["parameters"]
                    key = GText(getname(item.name))
                    if haskey(params, key)
                        delete!(params, key)
                    end
                end
            elseif kwname == "variables" || kwname ∈ vartypes
                attr = "free"
                if kwname ∈ vartypes
                    attr = kwname
                    kw = lexed[i+=1]
                    @assert(kw == Keyword("variables"))
                end
                sets = get(gams, "sets", nothing)
                variables = getdefault!(gams, "variables", OrderedDict{Any,VarInfo})
                while i < length(lexed) && !isa(lexed[i+1], StatementEnd) && !isa(lexed[i+1], Keyword)
                    var = lexed[i+=1]
                    var == GText(",") && continue
                    @assert(var isa GText || var isa GArray)
                    push!(variables, varinfo(var, attr, sets))
                end
            elseif kwname == "models"
                models = getdefault!(gams, kwname, Dict{String,ModelInfo})
                newmodels = Dict{String,Any}()
                i = parse_slashed!(newmodels, lexed, i)
                for (k,v) in newmodels
                    models[k] = ModelInfo(v)
                end
            elseif kwname == "options"
                options = getdefault!(gams, kwname)
                str = lexed[i+=1]
                while !isa(lexed[i+1], StatementEnd)
                    if lexed[i+1] == GText("=")
                        options[str.text] = string(lexed[i+2])
                        i+=1
                    else
                        options[str.text] = ""
                    end
                    i += 1
                end
            elseif kwname == "solve"
                newkw = lexed[i+=1]
                while !isa(newkw, Keyword) || newkw.text == "using"
                    newkw = lexed[i+=1]
                end
                @assert newkw.text == "minimizing" || newkw.text == "maximizing"
                modelname = lexed[i+=1]
                gams[newkw.text] = modelname.text
            elseif kwname == "equations"
                eqs = getdefault!(gams, kwname, OrderedDict{Any,Any})
                while i < length(lexed) && !isa(lexed[i+1], StatementEnd) && !isa(lexed[i+1], Keyword)
                    eq = lexed[i+=1]
                    @assert(eq isa GText || eq isa GArray)
                    # push!(eqs, eq=>"")  # add later so the ordering respects the definition order, not decl order
                end
            elseif kwname == "files"
                filename = lexed[i+=1]
                newkw = lexed[i+=1]
                while newkw != GText("putclose") || lexed[i+1] != filename
                    newkw = lexed[i+=1]
                end
                newkw = lexed[i+=1]
                while !isa(newkw, StatementEnd)
                    newkw = lexed[i+=1]
                end
            elseif kwname ∈ Set(["display"])
                newkw = lexed[i+=1]
                while !isa(newkw, StatementEnd)
                    newkw = lexed[i+=1]
                end
            else
                error("unhandled keyword ", kwname)
            end
        else
            @assert(stmt isa GText || stmt isa GArray)
            if length(lexed) < i+1
                # some files seem to have junk after the last semicolon
                println("skipping trailing ", stmt)
                break
            end
            if lexed[i+1] == Dots("..")
                # an equation definition
                eqs = gams["equations"]
                iend = seek_to_end(lexed, i+=2)
                eqs[stmt] = parseexprs1(lexed[i:iend])
                i = iend
            elseif lexed[i+1] == Dots(".")
                # a variable or model attribute
                @assert(lexed[i+3] == GText("="))
                attr = lexed[i+=2]
                iend = seek_to_end(lexed, i+=2)
                rhs = parseexprs1(lexed[i:iend])
                if attr isa GText && string(attr) ∈ modelattributes
                    m = gams["models"][string(stmt)]
                    m.assignments[string(attr)] = rhs
                else
                    attrstr = isa(attr, GText) ? string(attr) : attr.name
                    varkey = isa(attr, GText) ? stmt : GArray(string(stmt), attr.indices)
                    if attrstr ∈ varattributes
                        v = gams["variables"][varkey]  # gams["variables"][stmt] ?
                        push!(v.assignments, attr=>rhs)
                    else
                        error(attr, " not a recognized attribute")
                    end
                end
                i = iend
            else
                # general assignment (to, e.g., a parameter)
                @assert lexed[i+1] == GText("=")
                iend = seek_to_end(lexed, i+=2)
                assign = getdefault!(gams, "assignments", Vector{Pair{Any,Any}})
                push!(assign, stmt=>parseexprs1(lexed[i:iend]))
                i = iend
            end
        end
        i += 1
    end
    return gams
end
parsegams(arg) = parsegams(lex(arg))

function parse_slashed!(dest, lexed, i; parseitems::Bool=true)
    function parse_slashed1!(dest, lexed, i)
        name = lexed[i+=1]
        @assert(name isa GText || name isa GArray)
        if lexed[i+1] isa StatementEnd
            dest[name] = ""
            return i
        elseif lexed[i+1] isa Slashed
            val = lexed[i+=1]
        elseif lexed[i+1] isa GText && lexed[i+2] isa Slashed
            val = lexed[i+=2]
        else
            dest[name] = ""
            return i
        end
        @assert(val isa Slashed)
        if parseitems
            items = strip.(split(val.text, (',','\n')))
            # drop text for each item
            for j = 1:length(items)
                itemname, itemtext = splitws(items[j])
                items[j] = itemname
            end
            dest[convert(keytype(dest), name)] = items
        else
            dest[name] = val.text
        end
        return i
    end
    i = parse_slashed1!(dest, lexed, i)
    while i < length(lexed) && !isa(lexed[i+1], StatementEnd) && !isa(lexed[i+1], Keyword)
        i = parse_slashed1!(dest, lexed, i)
    end
    return i
end

function parseexprs(lexed, debug=false)
    # Step 1: split at commas
    args = []
    thisarg = []
    for i = 1:length(lexed)
        tok = lexed[i]
        if tok == GText(",")
            @assert(!isempty(thisarg))
            push!(args, copy(thisarg))
            empty!(thisarg)
        else
            push!(thisarg, tok)
        end
    end
    if !isempty(thisarg)
        push!(args, copy(thisarg))
        empty!(thisarg)
    end
    # Step 2: parse each arg
    n = length(args)
    parsed = Vector{AbstractLex}(uninitialized, n)
    todelete = Int[]
    for j = 1:n
        thisarg = args[j]
        # Step 2a: recurse into calls and parse their args
        debug && sexprlf.(thisarg)
        for i = 1:length(thisarg)
            tok = thisarg[i]
            if tok isa GCall
                callargs = parseexprs(tok.args, false)
                thisarg[i] = GCall(tok.name, callargs)
            elseif tok isa Parens
                pargs = parseexprs(tok.args, false)
                thisarg[i] = Parens(pargs)
            end
        end
        # Step 2b: process ^ (highest-precedence operator)
        debug && sexprlf.(thisarg)
        process_ops!(thisarg, ("^",), debug)
        # Step 2c: process * and / (next-highest)
        debug && sexprlf.(thisarg)
        process_ops!(thisarg, ("*", "/"), debug)
        # Step 2d: process + and -
        debug && println("prior to +/-")
        debug && sexprlf.(thisarg)
        process_ops!(thisarg, ("+", "-"), debug)
        # Step 2e: process "and" and "or"
        debug && println("post to +/-")
        debug && sexprlf.(thisarg)
        process_ops!(thisarg, ("and", "or"), debug)
        # Step 2f: process other boolean-returning operations
        process_ops!(thisarg, ("<", "<=", ">", ">="), debug)
        # Step 2g: process relations
        debug && sexprlf.(thisarg)
        process_ops!(thisarg, ("=e=", "=g=", "=l="), debug)
        if length(thisarg) != 1
            println(STDERR, "thisarg:")
            foreach(x->(print(STDERR, "  "); sexpr(STDERR, x); println(STDERR)), thisarg)
        end
        @assert(length(thisarg) == 1)
        parsed[j] = stripnewcall!(thisarg[1])
    end
    return parsed
end

function parseexprs1(lexed)
    parsed = parseexprs(lexed)
    @assert(length(parsed) == 1)
    return parsed[1]
end

# A "temporary type" for marking calls that arose from operator-precedence resolution
# Future lower-priority operators will need to recurse into any NewCalls
struct NewCall <: AbstractLex
    call
end

function process_ops!(tokens::Vector, ops, debug)
    debug = false
    # i = length(tokens)
    i = 1
    # while i > 0
    while i < length(tokens)
        debug && @show i
        tok = tokens[i]
        debug && @show tok
        if tok isa NewCall
            newtok = tok.call
            for arg in newtok.args
                process_ops!(arg, ops)
            end
        end
        if !isa(tok, GText) || tok.text ∉ ops
            i += 1
            continue
        end
        if i > 1 && !isoperator(tokens[i-1])
            debug && @show tokens[i-1] isoperator(tokens[i-1])
            tokens[i-1] = NewCall(GCall(tok.text, [tokens[i-1], tokens[i+1]]))
            deleteat!(tokens, (i,i+1))
            i -= 1
        else
            # unary + or -
            @assert(tok.text ∈ ("+", "-"))
            if tok.text == "+"
                tokens[i] = tokens[i+1]
            else
                nexttok = tokens[i+1]
                if nexttok isa GNumber
                    tokens[i] = GNumber(-nexttok.val)
                else
                    tokens[i] = NewCall(GCall("-", [nexttok]))
                end
            end
            deleteat!(tokens, (i+1,))
        end
        i += 1
    end
    return tokens
end
process_ops!(tokens, ops) = nothing

isoperator(op) = op isa GText && op.text ∈ operatortokens

function sexpr(io::IO, l::NewCall)
    print(io, "NewCall(")
    sexpr(io, l.call)
    print(io, ")")
end

function stripnewcall!(expr::NewCall)
    call = expr.call
    for i = 1:length(call.args)
        call.args[i] = stripnewcall!(call.args[i])
    end
    return call
end
stripnewcall!(expr) = expr

function deparen!(expr)
    if expr isa GCall
        for i = 1:length(expr.args)
            expr.args[i] = deparen!(expr.args[i])
        end
    elseif expr isa Parens && length(expr.args) == 1
        expr = expr.args[1]
        expr = deparen!(expr)
    end
    return expr
end

function varinfo(name::GText, attr, sets)
    return name=>VarInfo(attr, ())
end
function varinfo(name::GArray, attr, sets)
    return name=>VarInfo(attr, getaxes(name.indices, sets))
end

function seek_to_end(lexed, i)
    iend = i
    while iend < length(lexed) && !isa(lexed[iend+1], Union{StatementEnd, Keyword})
        iend += 1
    end
    return iend
end

function getwithkey(dict, key::AbstractString)
    for (k, v) in dict
        if getname(k) == key
            return v
        end
    end
    error(key, " not found")
end

function getdefault!(gams, tag, ::Type{T}) where T
    if !haskey(gams, tag)
        gams[tag] = T<:Associative ? T() : (T<:Vector ? T(uninitialized, 0) : error("type $T not handled"))
    end
    return gams[tag]
end
getdefault!(gams, tag) = getdefault!(gams, tag, Dict{String,String})
