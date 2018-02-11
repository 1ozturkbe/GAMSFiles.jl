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
                @assert(alias isa Parens && length(alias.args) == 2)
                sets = gams["sets"]
                symold, symnew = string.(alias.args)
                if !haskey(sets, symold)  # GAMS allows arbitrary ordering
                    symnew, symold = symold, symnew
                end
                sets[symnew] = sets[symold]
            elseif kwname == "scalars" || kwname == "parameters"
                params = getdefault!(gams, "parameters", Dict{Any,Any})
                i = parse_slashed!(params, lexed, i; parseitems=false)
            elseif kwname == "table"
                table = getdefault!(gams, "tables", Dict{Any,String})
                item = lexed[i+=1]
                table[item.name] = item.body
            elseif kwname == "variables" || kwname ∈ vartypes
                attr = "free"
                if kwname ∈ vartypes
                    attr = kwname
                    kw = lexed[i+=1]
                    @assert(kw == Keyword("variables"))
                end
                sets = get(gams, "sets", nothing)
                variables = getdefault!(gams, "variables", Dict{Any,VarInfo})
                while i < length(lexed) && !isa(lexed[i+1], StatementEnd) && !isa(lexed[i+1], Keyword)
                    var = lexed[i+=1]
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
                eqs = getdefault!(gams, kwname, Dict{Any,Any})
                while i < length(lexed) && !isa(lexed[i+1], StatementEnd) && !isa(lexed[i+1], Keyword)
                    eq = lexed[i+=1]
                    @assert(eq isa GText || eq isa GArray)
                    push!(eqs, eq=>"")
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
                println("skipping trailing ", stmt)
                return gams  # some files seem to have junk after the last semicolon
            end
            if lexed[i+1] == Dots("..")
                # an equation definition
                eqs = getdefault!(gams, "equations", Dict{Any,Any})
                iend = seek_to_end(lexed, i+=2)
                eqs[stmt] = lexed[i:iend]
                i = iend
            elseif lexed[i+1] == Dots(".")
                # a variable or model attribute
                @assert(lexed[i+3] == GText("="))
                attr = lexed[i+=2]
                iend = seek_to_end(lexed, i+=2)
                rhs = lexed[i:iend]
                if attr isa GText && string(attr) ∈ modelattributes
                    m = gams["models"][string(stmt)]
                    m.assignments[string(attr)] = rhs
                else
                    attrstr = isa(attr, GText) ? string(attr) : attr.name
                    varkey = isa(attr, GText) ? stmt : GArray(string(stmt), attr.indices)
                    if attrstr ∈ varattributes
                        v = gams["variables"][varkey]  # gams["variables"][stmt] ?
                        push!(v.assignments, attr=>lexed[i:iend])
                    else
                        error(attr, " not a recognized attribute")
                    end
                end
                i = iend
            else
                @assert lexed[i+1] == GText("=")
                iend = seek_to_end(lexed, i+=1)
                assign = getdefault!(gams, "assignments", Vector{Pair{Any,Any}})
                push!(assign, stmt=>lexed[i:iend])
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

function varinfo(name::GText, attr, sets)
    return name=>VarInfo(attr, ())
end
function varinfo(name::GArray, attr, sets)
    return name=>VarInfo(attr, getaxes(name.indices, sets))
end

function seek_to_end(lexed, i)
    iend = i
    while iend < length(lexed) && lexed[iend+1] != StatementEnd()
        iend += 1
    end
    return iend
end

function recombine(lexed, i)
    # Recombine all the text up to the next StatementEnd
    str = ""
    part = lexed[i+=1]
    while !isa(part, StatementEnd)
        if part isa Union{GText,GArray,GNumber}
            str *= string(part)
        elseif part ∈ (Keyword("smax"), Keyword("smin"), Keyword("sum"))
        else
            error("got ", str, "\n  but don't know what to do with ", part)
        end
        part = lexed[i+=1]
    end
    return str, i
end
