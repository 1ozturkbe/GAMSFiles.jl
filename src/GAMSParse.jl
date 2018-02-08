module GAMSParse

using DataStructures
using Compat

export parsegams

const gamsdecls = Set(["acronym", "acronyms",
                       "alias",
                       "equation", "equations",
                       "model", "models",
                       "parameter", "parameters",
                       "scalar", "scalars",
                       "set", "sets",
                       "table",
                       "variable", "variables"])
const gamsactions = Set(["option", "options",
                         "assign",
                         "for",
                         "display",
                         "abort",
                         "while",
                         "execute",
                         "solve",
                         "loop",
                         "repeat"])

# Functions used in GAMS expressions
gamsfuncs = quote
    sqr(x) = x*x
    POWER(x,p) = x^p
    power(x,p) = x^p
    arctan(x) = atan(x)
end

# Translations of GAMS functions to Julia functions
const gamsf2jf = Dict(:sum=>:sum, :smax=>:maximum)

## parsing
function parsegams(file)
    gams = Dict{String,Any}()
    neqs = 0
    lastdecl = ""
    open(file) do io
        while !eof(io)
            c = Base.peekchar(io)
            if c == '*' || c == '\n' || c == '\r'
                # Comment line
                readline(io)
                continue
            end
            block = strip(readuntil(io, ';'))
            isempty(block) && break
            tok, rest = splitws(block; rmsemicolon=true)
            tok = lowercase(tok)
            if tok ∈ gamsdecls
                lastdecl = tok
            elseif tok ∈ gamsactions
                lastdecl = ""
            else
                rest = tok*" "*rest # restore for future parsing
            end
            rest = replace_charints(rest)
            if lastdecl == "set" || lastdecl == "sets"
                gams["Sets"] = sets = Dict{String,String}()
                lines = strip.(split(rest, '\n'; keep=false))
                for line in lines
                    sym, rng = split(line)
                    sets[sym] = rng
                end
            elseif lastdecl == "alias"
                ex = parse(rest)
                @assert(ex.head == :tuple && length(ex.args) == 2)
                symold, symnew = ex.args
                sets[string(symnew)] = sets[string(symold)]
            elseif lastdecl == "variable" || lastdecl == "variables"
                if tok ∈ ("free", "positive", "negative", "binary", "integer")  # not yet handled
                    continue
                end
                m = match(r"=", rest)
                if m == nothing
                    vars = strip.(split(rest, r"[,\n]"))
                    gams["Variables"] = filter(x->!isempty(x), vars)
                else
                    lastdecl = "parameters"  # process in the parameters block
                end
            end
            if lastdecl == "equation" || lastdecl == "equations"
                gams["Equations"] = eqs = Pair{String,String}[]
                eqnames = strip.(split(rest, r"[,\n]"))
                for eq in eqnames  # this is just a counter, because the order isn't guaranteed
                    eqex = replace_charints(replace(strip(readuntil(io, ';')), r"[\t\n\r]", ' '))
                    push!(eqs, stripname(eqex))
                    neqs += 1
                end
                lastdecl = ""
            elseif lastdecl == "parameters" || lastdecl == "parameter"
                if !haskey(gams, "Parameters")
                    gams["Parameters"] = OrderedDict{String,String}()
                end
                m = match(r"=", rest)
                if m === nothing
                    sym, vals = splitws(rest)
                    if isempty(vals)
                        # This is a "size" declaration
                        gams["Parameters"][sym] = ""
                    else
                        if !isempty(vals) && vals[1] == vals[end] == '/'
                            vals = strip(vals[2:end-1])
                        end
                        if !isempty(vals)
                            gams["Parameters"][sym] = vals
                        end
                    end
                else
                    lhs, rhs = strip(rest[1:m.offset-1]), strip(rest[m.offset+1:end])
                    gams["Parameters"][lhs] = rhs
                end
            elseif lastdecl == "table"
                if !haskey(gams, "Table")
                    gams["Table"] = Dict{String,String}()
                end
                sym, vals = splitws(rest)
                gams["Table"][sym] = vals
            elseif tok == "solve"
                words = split(rest)
                @assert words[end-1] == "minimizing" || words[end-1] == "maximizing"
                gams[words[end-1]] = words[end][1:end]
            end
        end
    end
    return gams
end

"""
    modex = parsegams(Module, modname, gams)
    modex = parsegams(Module, filename)

Return an expression defining a module which, when evaluated (`mod = eval(modex)`),
contains a (non-exported) function `f(x)` evaluating the objective function defined in the
corresponding GAMS file.
"""
function parsegams(::Type{Module}, modname::Symbol, gams::Dict{String,Any})
    sets = parsesets(gams)
    vars = allvars(gams)
    consts, constexs = parseconsts(gams, sets, vars)
    # Create the computational part of the function body
    eqex = Expr[]
    for (eqname, eqstr) in gams["Equations"]
        if iscallstr(eqname)
            ex, _ = parse(eqname, 1)
            indexsym = ex.args[2]
            push!(eqex, parseassign(eqstr, vars; loop=(indexsym, sets[indexsym])))
        else
            push!(eqex, parseassign(eqstr, vars))
        end
    end
    # Create the return statement
    retvalstr = gams["minimizing"]
    push!(eqex, :(return $(Symbol(retvalstr))))
    # As needed, destructure the input vector
    varstrings = copy(gams["Variables"])
    i = findfirst(equalto(retvalstr), varstrings)
    if i > 0
        deleteat!(varstrings, i)
    end
    for j = 2:length(varstrings)
        ex, _ = parse(varstrings[j], 1)
        ex isa Symbol && continue
        @assert(ex.head == :call)
        varsym, indexsym = ex.args[1], ex.args[2]
        r = sets[indexsym]
        @assert(first(r)==1)  # could be fixed
        n = length(r)
        unshift!(eqex, :($varsym = Vector{Float64}(uninitialized, $n)))
    end
    if !iscallstr(varstrings[1])
        xin = gensym("x")
        unshift!(eqex, Expr(:(=), Expr(:tuple, Symbol.(varstrings)...), xin))
    else
        xin = varsym(varstrings[1])
    end
    body = Expr(:block, eqex...)
    # Set up the constant expressions
    constexprs = Expr[]
    for (k, v) in consts
        push!(constexprs, Expr(:const, :($k = $v)))
    end
    append!(constexprs, constexs)
    modex = quote
        module $modname
        using Compat
        $gamsfuncs
        $(Expr(:block, constexprs...))
        function f($xin)
            $body
        end
        end  # module
    end
    return Expr(:toplevel, modex.args[2])
end
function parsegams(::Type{Module}, filename::AbstractString, gams::Dict{String,Any})
    bname, _ = splitext(basename(filename))
    return parsegams(Module, Symbol(bname), gams)
end
function parsegams(::Type{Module}, filename::AbstractString)
    gams = parsegams(filename)
    bname, _ = splitext(basename(filename))
    return parsegams(Module, Symbol(bname), gams)
end

function splitws(str; rmsemicolon::Bool=false)
    iend = endof(str)
    if rmsemicolon && str[iend] == ';'
        iend = prevind(str, iend)
    end
    m = match(r"\s", str)
    m == nothing && return str, ""
    return str[1:m.offset-1], strip(str[m.offset:iend])
end

function stripname(eqstr)
    m = match(r"\.\.", eqstr)
    @assert(m != nothing)
    return strip(eqstr[1:m.offset-1]) => strip(eqstr[m.offset+3:end])
end

function varsym(str::AbstractString)
    ex, _ = parse(str, 1)
    ex isa Symbol && return ex
    if ex isa Expr && (ex.head == :call || ex.head == :ref)
        return ex.args[1]
    end
    error(str, " does not appear to refer to a variable")
end

iscallstr(str) = match(r"\(.*\)", str) != nothing

function allvars(gams::Dict)
    vars = Set{Symbol}()
    for v in gams["Variables"]
        push!(vars, GAMSParse.varsym(v))
    end
    if haskey(gams, "Table")
        for (k,v) in gams["Table"]
            push!(vars, GAMSParse.varsym(k))
        end
    end
    if haskey(gams, "Parameters")
        for (k,v) in gams["Parameters"]
            push!(vars, GAMSParse.varsym(k))
        end
    end
    return vars
end

function parsesets(gams::Dict)
    sets = Dict{Symbol,UnitRange{Int}}()
    haskey(gams, "Sets") || return sets
    for (sym, rstr) in gams["Sets"]
        # We require these to be of the form `sym  /1*n/`
        rex, _ = parse(rstr[2:end-1], 1)
        if rex.head == :call && rex.args[1] == :* && rex.args[2] == 1
            sets[Symbol(sym)] = rex.args[2] : rex.args[3]
        else
            error("failed to parse set assignment ", rstr)
        end
    end
    return sets
end

function parseconsts(gams::Dict, sets::Dict{Symbol,UnitRange{Int}}, vars)
    consts, exprs = Dict{Symbol,Any}(), Expr[]
    if haskey(gams, "Parameters")
        for (varstr, val) in gams["Parameters"]
            varex, _ = parse(varstr, 1)
            @assert(varex.head == :call)
            varsym, indexsym = varex.args[1], (varex.args[2:end]...)
            if indexsym isa Tuple{Symbol,Vararg{Symbol}}
                r = map(x->sets[x], indexsym)
                n = length.(r)
                if val == ""
                    # Allocation only
                    consts[varsym] = Array{Float64}(uninitialized, n)
                else
                    lines = split(val, '\n')
                    if length(lines) == prod(n)
                        c = haskey(consts, varsym) ? consts[varsym] : Array{Float64}(uninitialized, n)
                        for line in lines
                            istr, cstr = splitws(strip(line))
                            c[numeval(istr)] = numeval(cstr)
                        end
                        consts[varsym] = c
                    else
                        # The expression must be a formula for varsym
                        push!(exprs, :(const $varsym = Array{Float64}(uninitialized, $n)))
                        lhs, rhs = calls2refs(varstr, vars), calls2refs(val, vars)
                        body = Expr(:(=), lhs, rhs)
                        while !isempty(indexsym)
                            thissym, thisr = indexsym[1], r[1]
                            body = quote
                                for $thissym in $thisr
                                    $body
                                end
                            end
                            indexsym, r = Base.tail(indexsym), Base.tail(r)
                        end
                        push!(exprs, body)
                        # Also add to list of variables
                        push!(vars, varsym)
                    end
                end
            elseif indexsym isa Tuple{Integer,Vararg{Integer}}
                c = consts[varsym]
                if isnumberstring(val)
                    c[indexsym...] = numeval(val)
                else
                    varex = calls2refs!(varex, vars)
                    rhs = calls2refs!(parse(val), vars)
                    if rhs.head == :ref && all(x->isa(x, Number), rhs.args[2:end])
                        rc = consts[rhs.args[1]]
                        c[indexsym...] = rc[rhs.args[2:end]...]
                    else
                        push!(exprs, :($varex = $rhs))
                    end
                end
            end
        end
    end
    if haskey(gams, "Table")
        for (varstr, val) in gams["Table"]
            varex, _ = parse(varstr, 1)
            @assert(varex.head == :call)
            varsym, indexsym1, indexsym2 = varex.args[1], varex.args[2], varex.args[3]
            rm, rn = sets[indexsym1], sets[indexsym2]
            @assert(first(rm)==1 && first(rn)==1)
            m, n = length(rm), length(rn)
            c = Matrix{Float64}(uninitialized, m, n)
            lines = strip.(split(val, '\n'))
            next_is_header = true
            colindex = 0:0
            for line in lines
                if isempty(line)
                    next_is_header = true
                elseif next_is_header || line[1] == '+'
                    if line[1] == '+'
                        line = line[2:end]
                    end
                    inds = split(line)
                    colindex = numeval(inds[1]):numeval(inds[end])
                    next_is_header = false
                else
                    rowvals = strip.(split(line, r"[\s-]", keep=false))
                    c[numeval(rowvals[1]), colindex] = numeval.(rowvals[2:end])
                end
            end
            consts[varsym] = c
        end
    end
    return consts, exprs
end

function numeval(str)
    m = match(r"\.", str)
    return m == nothing ? parse(Int, str) : parse(Float64, str)
end

function isnumberstring(str)
    m = match(r"^[+-]?[0-9]*\.?[0-9]*$", str)
    return m !== nothing
end

function calls2refs!(ex::Expr, vars)
    for i = 1:length(ex.args)
        ex.args[i] = calls2refs!(ex.args[i], vars)
    end
    if ex.head == :call && ex.args[1] ∈ vars
        return Expr(:ref, ex.args...)
    end
    return ex
end
calls2refs!(ex, vars) = ex
function calls2refs(str::AbstractString, vars)
    ex, _ = parse(str, 1)
    return calls2refs!(ex, vars)
end

function parseassign(eqex, vars; loop=nothing)
    if endswith(eqex, ';')
        eqex = eqex[1:end-1]
    end
    m = match(r"=[eE]=", eqex)
    m === nothing && error("cannot parse ", eqex)
    lhs, rhs = strip(eqex[1:m.offset-1]), strip(eqex[m.offset+3:end])
    lhs, rhs = replaceexprs(lhs, vars), replaceexprs(rhs, vars)
    if rhs ∈ vars || isnumberstring(lhs)
        lhs, rhs = rhs, lhs
    end
    if lhs ∈ vars
        lhsex, rhsex = parse(lhs), parse(rhs)
        return :($lhsex = $rhsex)
    elseif isnumberstring(rhs)
        # Assume the variable-to-be-assigned is last
        i = endof(lhs)
        while !isspace(lhs[i]) && i > 0
            i = prevind(lhs, i)
        end
        @assert(i>0)
        vstr = lhs[nextind(lhs, i):end]
        v = varsym(vstr)
        @assert(v ∈ vars)
        # Find the preceding operator
        while i > 0 && ((c = lhs[i]) != '+' && c != '-')
            i = prevind(lhs, i)
        end
        @assert(i>0)
        c = lhs[i] == '+' ? -1 : 1
        rest = lhs[1:i-1]
        restex = parse(rest)
        rhsval = numeval(rhs)
        if loop == nothing
            return :($(Symbol(vstr)) = $c * $restex + $rhsval)
        else
            loopvar, loopr = loop
            return quote
                for $loopvar = $loopr
                    $(Symbol(vstr)) = $c * $restex + $rhsval
                end
            end
        end
    else
        error("neither the lhs $lhs nor rhs $rhs appeared in $vars")
    end
end

function replaceexprs(str, vars)
    ex, _ = parse(str, 1)
    newex = replaceexprs!(ex)
    finalex = calls2refs!(newex, vars)
    string(finalex)
end

function replace_charints(str)
    # Replace 'i' (where i is an integer) with the integer
    return replace(str, r"'(\d*)'", s"\1")
end

function replaceexprs!(ex::Expr)
    if ex.head == :call && ex.args[1] ∈ (:smax, :sum)
        vars, val = ex.args[2], replaceexprs!(ex.args[3])
        index_exprs = Expr[]
        if vars isa Symbol
            sym, slot, success = find_index_usage(val, vars)
            success || error("could not find $vars in $val")
            push!(index_exprs, :($vars in Compat.axes($sym, $slot)))
        else
            for v in vars
                sym, slot, success = find_index_usage(val, v)
                success || error("could not find $v in $val")
                push!(index_exprs, :($v in Compat.axes($sym, $slot)))
            end
        end
        return Expr(:call, gamsf2jf[ex.args[1]], Expr(:comprehension, Expr(:generator, val, index_exprs...)))
    end
    for i = 1:length(ex.args)
        ex.args[i] = replaceexprs!(ex.args[i])
    end
    return ex
end
replaceexprs!(ex) = ex

function find_index_usage(ex::Expr, indexsym)
    if ex.head == :call
        i = findfirst(ex.args, indexsym)
        if i > 0
            return ex.args[1], i-1, true
        end
    end
    for arg in ex.args
        sym, slot, success = find_index_usage(arg, indexsym)
        success && return sym, slot, success
    end
    return :missing, 0, false
end
find_index_usage(ex, indexsym) = :missing, 0, false

# Full list of GAMS keywords, in case it's useful:
const gamskws = ["abort",
                 "acronym",
                 "acronyms",
                 "alias",
                 "all",
                 "and",
                 "assign",
                 "binary",
                 "card",
                 "diag",
                 "display",
                 "else",
                 "eps",
                 "eq",
                 "equation",
                 "equations",
                 "file",
                 "files",
                 "for",
                 "free",
                 "ge",
                 "gt",
                 "if",
                 "inf",
                 "integer",
                 "le",
                 "loop",
                 "lt",
                 "maximizing",
                 "minimizing",
                 "model",
                 "models",
                 "na",
                 "ne",
                 "negative",
                 "no",
                 "not",
                 "option",
                 "options",
                 "or",
                 "ord",
                 "parameter",
                 "parameters",
                 "positive",
                 "prod",
                 "putpage",
                 "puttl",
                 "repeat",
                 "sameas",
                 "scalar",
                 "scalars",
                 "semicont",
                 "semiint",
                 "set",
                 "sets",
                 "smax",
                 "smin",
                 "solve",
                 "sos1",
                 "sos2",
                 "sum",
                 "system",
                 "table",
                 "then",
                 "until",
                 "using",
                 "variable",
                 "variables",
                 "while",
                 "xor",
                 "yes"]


end # module
