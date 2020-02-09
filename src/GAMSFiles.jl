module GAMSFiles

using DataStructures, OffsetArrays

export parsegams, getwithkey, sexpr

include("types.jl")
include("consts.jl")
include("io.jl")
include("lexer.jl")
include("parser.jl")

"""
    modex = parsegams(Module, modname, gams)
    modex = parsegams(Module, filename)

Return an expression defining a module which, when evaluated (`mod = eval(modex)`),
contains a (non-exported) function `f(x)` evaluating the objective function defined in the
corresponding GAMS file.
"""
function parsegams(::Type{Module}, modname::Symbol, gams::Dict{String,Any})
    parseconsts!(gams)
    vars = gams["variables"]
    # Create the computational part of the function body
    preexprs, bodyexprs, solved = parseassignments(gams)
    # "uncheck" the solved flag for initialized variables
    for (v, vinfo) in vars
        if isinitialized(vinfo)
            solved[getname(v)] = false
        end
    end
    # Create the return statement
    retvalstr = gams["minimizing"]
    push!(bodyexprs, :(return $(Symbol(retvalstr))))
    # For the solved variables, allocate as needed. For the unsolved variables,
    # store them in declaration order for destructuring
    uvars = []
    for (v, vinfo) in vars
        if solved[getname(v)]
            if v isa GArray && getname(v) != gams["minimizing"]
                vsym = Symbol(getname(v))
                allocex = allocate_expr(v.indices, gams["sets"])
                unshift!(bodyexprs, :($vsym = $allocex))
            end
        else
            push!(uvars, v)
        end
    end
    # As needed, destructure the input vector
    varnames = Vector{Symbol}(undef, length(uvars))
    if length(uvars) == 1 && (v = uvars[1]) isa GArray
        varnames[1] = getname(v)
        xin = Symbol(getname(v))
        xaxes = getaxes(v.indices, gams["sets"])
    else
        for (i, v) in enumerate(uvars)
            v isa GText || error("destructured inputs require scalar args, got ", v, ".\nPerhaps there is an unused variable in the file?")
            varnames[i] = getname(v)
        end
        xin = gensym("x")
        xaxes = (Base.OneTo(length(varnames)),)
        unshift!(bodyexprs, Expr(:(=), Expr(:tuple, varnames...), xin))
    end
    szcheck = :(@assert(axes($xin) == $xaxes))
    body = Expr(:block, szcheck, bodyexprs...)
    # Set up the constant expressions
    initexprs = Expr[]
    for p in ("parameters", "tables")
        if haskey(gams, p)
            for (k, v) in gams[p]
                v = v isa Ref ? v[] : v
                push!(initexprs, Expr(:const, :($(Symbol(getname(k))) = $v)))
            end
        end
    end
    append!(initexprs, preexprs)
    modex = quote
        module $modname
        using OffsetArrays
        $gamsfuncs
        $(Expr(:block, initexprs...))
        function objective($xin)
            $body
        end
        end  # module
    end
    # Initialization information
    x0 = fillr(NaN, xaxes)
    lo, up = fillr(-Inf, xaxes), fillr(Inf, xaxes)
    for (i, v) in enumerate(uvars)
        vinfo = vars[v]
        for (prop, val) in vinfo.assignments
            inds = nothing
            if isa(prop, GText)
                inds = (i,)
            elseif isa(prop, GArray)
                inds = map(x->x.val, prop.indices)
            end
            if isa(prop, Union{GText,GArray})
                c = val.val
                if getname(prop) ∈ ("l", "fx")
                    x0[inds...] = c
                elseif getname(prop) == "lo"
                    lo[inds...] = c
                elseif getname(prop) == "up"
                    up[inds...] = c
                end
            end
        end
    end
    return Expr(:toplevel, modex.args[2]), (x0, lo, up)
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

function parseconsts!(gams::Dict{String,Any})
    sets = get(gams, "sets", nothing)
    if haskey(gams, "parameters")
        params = gams["parameters"]
        for (key, val) in params
            c = allocate(key, sets)
            if !isempty(val)
                if key isa GText
                    c[] = parse(Float64, val)
                else
                    lines = strip.(split(val, '\n'))
                    for line in lines
                        ln = split(line)
                        @assert(length(ln) == 2)
                        c[parse(Int, ln[1])] = parse(Float64, ln[2])
                    end
                end
            end
            params[key] = c
        end
    end
    if haskey(gams, "tables")
        tables = gams["tables"]
        for (key, val) in tables
            @assert(key isa GArray)
            c = allocate(key, sets)
            lines = split(val, '\n')
            firstrow = true
            cols = Int[]
            for line in lines
                isempty(strip(line)) && continue
                if line[1] == '+'
                    firstrow = true
                end
                if firstrow
                    empty!(cols)
                    # Identify the locations of the column ends (labels are right-justified)
                    j = 2
                    while j <= length(line)
                        i, j = bracket_text(line, j)
                        if i <= length(line)
                            push!(cols, parse(Int, line[i:j-1]))
                        end
                    end
                    firstrow = false
                    continue
                end
                i, j = bracket_text(line, 1)
                row = parse(Int, line[i:j-1])
                for k = 1:length(cols)
                    i, j = bracket_text(line, j)
                    c[row,cols[k]] = parse(Float64, line[i:j-1])
                end
            end
            tables[key] = c
        end
    end
    gams
end

function bracket_text(line, i)
    while i <= length(line) && isspace(line[i])
        i = nextind(line, i)
    end
    j = i
    while j <= length(line) && !isspace(line[j]) && (j == i || line[j] != '-')  # sometimes columns have no gap, use the sign as an indicator
        j = nextind(line, j)
    end
    i, j
end

# Process assignments that can be resolved to a constant
function parseassignments!(exprs, asgn::Vector{<:Pair}, params::Dict, sets)
    for (k, v) in asgn
        x, success = evalconst(v)
        if success
            if k isa GArray && all(x->isa(x, GNumber), k.indices)
                # This can be evaluated ahead of time
                c = params[k]
                inds = map(x->x.val, k.indices)
                c[inds...] = x
            elseif k isa GText
                params[k][] = x
            else
                lhs = convert(Expr, k)
                push!(exprs, assignexpr(lhs, x, sets))
            end
        else
            push!(exprs, assignexpr(convert(Expr, k), convert(Expr, v), sets))
        end
    end
    return exprs
end

function parseassignments(gams)
    vars = getvars(gams["variables"])
    sets = get(gams, "sets", Dict{String,Any}())
    preexprs, bodyexprs = Expr[], Expr[]
    if haskey(gams, "parameters") && haskey(gams, "assignments")
        parseassignments!(preexprs, gams["assignments"], gams["parameters"], sets)
    end
    # Anyone using a solver approach (general constrained optimization) might want to
    # skip the next part (see below).
    if haskey(gams, "equations")
        solved = eqs2assigns!(bodyexprs, gams["equations"], vars, sets, gams["minimizing"])
    else
        solved = nothing
    end
    return preexprs, bodyexprs, solved
end

"""
    solved = eqs2assigns!(bodyexprs, equations, vars, sets, solvevar)

Convert `equations`, containing a list of relations, into Julia expressions
corresponding to assignments, storing then in `bodyexprs`. `solvevar` is the variable you
are trying to solve for (e.g., the objective value), and only those entries in `equations`
needed to solve for it are processed. Any processed entry in `equations` is removed.

Returns `solved`, a dictionary indicating the status of all variables listed in `vars`.
`sets` contains the ranges defined as "sets" in the GAMS file.

This is designed to convert simple constraints into assignment statements,
to allow an objective to be evaluated. For example, if you're minimizing `objval`
(`solvevar == "objval"`) it converts the GAMS equation
    eq1..  x + objval =e= 2
into
    objval = 2 - x
and the GAMS equations
    eq1(i)..  2*x(i) - y =g= 0
    eq0..    5*y - objval =e= 0
into
    y = Inf
    for i = irange
        y = min(y, 2*x[i])
    end
    objval = 5*y
"""
function eqs2assigns!(bodyexprs, equations, vars, sets, solvevar)
    # Determine equation(s) that can be solved for needed variables
    eqs = []   # equations in reverse order of evaluation
    neededvars = Set([solvevar])
    solved = Dict([v=>isinitialized(vinfo) for (v,vinfo) in vars])
    todelete_eqs = []  # it's not nice to modify containers that you're...
    todelete_nvs = []  # ...iterating over, so these store items to be killed...
    toadd_nvs    = []  # ...or added, and are processed after the loop is finished.
    while !isempty(equations) && !isempty(neededvars)
        anysucceeded = false
        empty!(todelete_nvs)
        for var in neededvars
            empty!(todelete_eqs)
            for (key, eq) in equations
                eqsol, succeeded = solvefor(eq, var)
                deparen!(eqsol)
                if succeeded
                    push!(eqs, key=>eqsol)
                    solved[var] = true
                    push!(todelete_nvs, var)
                    add_dependents!(toadd_nvs, eqsol.args[2], solved)
                    push!(todelete_eqs, key=>eq)
                    anysucceeded = true
                    # Don't break because we may have multiple constraint equations
                end
            end
            for pr in todelete_eqs
                deletefrom!(equations, pr)
            end
        end
        for var in todelete_nvs
            delete!(neededvars, var)
        end
        for var in toadd_nvs
            push!(neededvars, var)
        end
        anysucceeded || break  # triggered when the only remaining variables are inputs
    end
    @assert(!isempty(eqs))
    sentinels = Dict()
    for (key, eq) in reverse(eqs)
        @assert(eq isa GCall && length(eq.args)==2)
        lhs, rhs, op = eq.args[1], eq.args[2], eqops[getname(eq)]
        eqj = assignexpr(convert(Expr, lhs), convert(Expr, rhs), key, sets, op)
        eqj = replace_reductions!(eqj, sets)
        push!(bodyexprs, eqj)
        sentinels[lhs] = op
    end
    for (key, op) in sentinels
        op == :(=) && continue
        val = op == :> ? -Inf : Inf
        if key isa GText
            init = Expr(:(=), Symbol(key.text), val)
        elseif key isa GArray
            init = Expr(:call, :fill!, Symbol(key.name), val)
        else
            error("unexpected key ", key)
        end
        unshift!(bodyexprs, init)
    end
    return solved
end

function deletefrom!(a::Vector, pr::Pair)
    j = 0
    for i = 1:length(a)
        if a[i] == pr
            j = i
        end
    end
    if j != 0
        deleteat!(a, j)
    end
    a
end
deletefrom!(a::AbstractDict, pr::Pair) = delete!(a, first(pr))

function solvefor(eq, solvevar)
    @assert((eq isa GCall) & haskey(eqops, getname(eq)))
    lhs, rhs, op = eq.args[1], eq.args[2], getname(eq)
    onleft, onright = hasvar(lhs, solvevar), hasvar(rhs, solvevar)
    # Make sure exactly one has it
    onleft != onright || return eq, false
    if onright
        lhs, rhs, op = rhs, lhs, flipop(op)
    end
    if lhs isa Parens && length(lhs.args) == 1
        lhs = lhs.args[1]
    end
    if lhs isa Union{GText,GArray} && getname(lhs) == solvevar
        return GCall(op, [lhs, rhs]), true
    end
    if (lhs isa GCall && getname(lhs) ∈ ("+", "-"))
        if length(lhs.args) == 1
            # unary -
            @assert(getname(lhs) == "-")
            neweq = GCall(flipop(op), lhs.args, unaryminus(rhs))
        else
            @assert(length(lhs.args)==2)
            arg1, arg2 = lhs.args
            has1, has2 = hasvar(arg1, solvevar), hasvar(arg2, solvevar)
            has1 != has2 || return eq, false
            if has1
                neweq = GCall(op, [arg1, GCall(flipsign(getname(lhs)), [rhs, arg2])])
            else
                if getname(lhs) == "+"
                    neweq = GCall(op, [arg2, GCall(flipsign(getname(lhs)), [rhs, arg1])])
                else
                    neweq = GCall(flipop(op), [arg2, GCall("-", [arg1, rhs])])
                end
            end
        end
        return solvefor(neweq, solvevar)
    end
    return eq, false
end

function hasvar(expr, varname::AbstractString)
    expr isa GText && return expr.text == varname
    expr isa GArray && return expr.name == varname
    expr isa GNumber && return false
    expr isa Union{GCall,Parens} || error(expr, " unexpected")
    hv = false
    for i = 1:length(expr.args)
        hv |= hasvar(expr.args[i], varname)
        hv && break
    end
    return hv
end

function add_dependents!(neededvars, rhs, solved)
    if rhs isa Union{GCall,Parens}
        for a in rhs.args
            if !get(solved, getname(a), true)
                push!(neededvars, getname(a))
            end
            add_dependents!(neededvars, a, solved)
        end
    else
        if !get(solved, getname(rhs), true)
            push!(neededvars, getname(rhs))
        end
    end
    neededvars
end

function isinitialized(vinfo)
    for (prop, val) in vinfo.assignments
        isa(prop, GText) && prop.text ∈ ("l", "fx") && return true
    end
    return false
end
isinitialized(::Nothing) = false

function assignexpr(lhs, rhs, sets::Dict, op = :(=))
    if op == :(=)
        body = Expr(:(=), lhs, rhs)
    elseif op == :> || op == :<
        body = Expr(:(=), lhs, Expr(:call, op == :> ? :max : :min, lhs, rhs))
    else
        error("operator $op not recognized")
    end
    if lhs isa Expr && lhs.head == :ref
        for j = 2:length(lhs.args)
            s = lhs.args[j]
            if s isa Symbol
                rng = sets[string(s)]
                body = quote
                    for $s = $rng
                        $body
                    end
                end
            end
        end
    end
    return body
end

function assignexpr(lhs, rhs, eqname::AbstractLex, sets::Dict, op = :(=))
    if op == :(=)
        body = Expr(:(=), lhs, rhs)
    elseif op == :> || op == :<
        body = Expr(:(=), lhs, Expr(:call, op == :> ? :max : :min, lhs, rhs))
    else
        error("operator $op not recognized")
    end
    if eqname isa GArray
        for s in eqname.indices
            if !isa(s, GNumber)
                rng = sets[getname(s)]
                itersym = Symbol(getname(s))
                body = quote
                    for $itersym = $rng
                        $body
                    end
                end
            end
        end
    end
    return body
end

flipop(op::Symbol) = op == :(=) ? op : (op == :< ? :> : (op == :> ? :< : error(op, " not recognized")))
flipop(op::String) = op == "=e=" ? op : (op == "=l=" ? "=g=" : (op == "=g=" ? "=l=" : error(op, " not recognized")))

flipsign(op::String) = op == "+" ? "-" : (op == "-" ? "+" : error(op, " is not a sign operator"))

function unaryminus(x)
    # A double-negative is a positive
    if isa(x, GCall) && getname(x) == "-" && length(x.args) == 1
        return x.args[1]
    end
    return GCall("-", x)
end

function allvars(gams)
    totvars = Dict{String,Any}()
    if haskey(gams, "variables")
        getvars!(totvars, gams["variables"])
    end
    if haskey(gams, "parameters")
        getvars!(totvars, gams["parameters"])
    end
    if haskey(gams, "tables")
        getvars!(totvars, gams["tables"])
    end
    return totvars
end

function getvars!(vars, pairiter)
    for (key, val) in pairiter
        vars[getname(key)] = val
    end
    return vars
end
getvars(pairiter) = getvars!(Dict{String,Any}(), pairiter)

function evalconst(expr::AbstractLex)
    isa(expr, GNumber) && return expr.val, true
    if isa(expr, GCall)
        # Apply evalconst to the args of the call
        arg_sxs = map(evalconst, expr.args)
        if all(x->x[2], arg_sxs)
            # we were able to evaluate all the args
            args = map(x->x[1], arg_sxs)
            f = getfield(GamsFuncs, Symbol(expr.name))
            return f(args...), true
        end
    elseif isa(expr, Parens)
        if length(expr.args) == 1
            return evalconst(expr.args[1])
        end
    end
    return NaN, false
end

function getaxes(setnames, sets)
    axs = []
    for name in setnames
        s = sets[string(name)]
        push!(axs, s isa UnitRange ? s : Base.OneTo(length(s)))
    end
    (axs...,)
end

allocate(array::GArray, sets) = allocate(array.indices, sets)
allocate(x::GText, sets) = Ref(0.0)   # a ref so that it can be passed in updatable form

function allocate(setnames, sets)
    axs = getaxes(setnames, sets)
    axs isa Tuple{} && return 0.0
    axs isa Tuple{Base.OneTo{Int},Vararg{Base.OneTo{Int}}} &&
        return Array{Float64}(undef, length.(axs))
    return OffsetArray{Float64}(undef, axs)
end

function allocate_expr(setnames, sets)
    axs = getaxes(setnames, sets)
    axs isa Tuple{} && return 0.0
    axs isa Tuple{Base.OneTo{Int},Vararg{Base.OneTo{Int}}} &&
        return :(Array{Float64}(undef, $(length.(axs))))
    return :(OffsetArray{Float64}(undef, $axs))
end


function splitws(str; rmsemicolon::Bool=false)
    iend = lastindex(str)
    if rmsemicolon && str[iend] == ';'
        iend = prevind(str, iend)
    end
    m = match(r"\s", str)
    m == nothing && return str, ""
    return str[1:m.offset-1], strip(str[m.offset:iend])
end

function replace_reductions!(ex::Expr, sets)
    for i = 1:length(ex.args)
        ex.args[i] = replace_reductions!(ex.args[i], sets)
    end
    if ex.head == :call && ex.args[1] ∈ (:sum, :prod, :smin, :smax)
        @assert(length(ex.args) == 3)
        vars, val = ex.args[2], ex.args[3]
        index_exprs = Expr[]
        if vars isa Symbol
            rng = sets[String(vars)]
            push!(index_exprs, :($vars = $rng))
        else
            for v in vars
                rng = sets[String(v)]
                push!(index_exprs, :($v = $rng))
            end
        end
        return Expr(:call, gamsf2jf[ex.args[1]], Expr(:comprehension, Expr(:generator, val, index_exprs...)))
    end
    return ex
end
replace_reductions!(arg, sets) = arg

fillr(val, inds::Tuple{}) = Ref(val)
fillr(val, inds::Tuple{Base.OneTo,Vararg{Base.OneTo}}) = fill(val, length.(inds))
fillr(val, inds) = fill(val, inds)

end # module
