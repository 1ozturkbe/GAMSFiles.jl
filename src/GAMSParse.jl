module GAMSParse

using Compat

export sqr, POWER, parsegams

# Functions used in GAMS expressions
sqr(x) = x*x
POWER(x,p) = x^p

## parsing
function parsegams(::Type{Expr}, file)
    vars = String[]
    assigns = Expr[]
    retval = ""
    neqs = 0
    open(file) do io
        while !eof(io)
            line = strip(readline(io))
            if startswith(line, "Variables")
                if line[end] != ';'
                    line = line*readuntil(io, ';')
                end
                vars = declargs(line, "Variables")
            elseif startswith(line, "Equations")
                eqnames = declargs(line, "Equations")
                for eq in eqnames
                    eqex = replace(strip(readuntil(io, ';')), r"[\t\n\r]", ' ')
                    @assert(startswith(eqex, eq))
                    push!(assigns, parseassign(eqex, vars))
                    neqs += 1
                end
            elseif startswith(line, "Solve")
                words = split(line)
                @assert words[end-1] == "minimizing"
                retval = words[end][1:end-1]
            end
        end
    end
    @assert !isempty(vars) && neqs > 0 && retval != ""
    to_delete = Int[]
    for i = 1:length(vars)
        if vars[i] == retval
            push!(to_delete, i)
        end
    end
    deleteat!(vars, to_delete)
    vars = Symbol.(vars)
    xin = gensym("x")
    destruct = :($(Expr(:tuple, vars...)) = $xin)
    blk = Expr(:block, destruct, assigns...)
    return Expr(:function, Expr(:tuple, xin), blk, :(return $(Symbol(retval))))
end

declargs(line, prefix) = strip.(split(strip(line[length(prefix)+1:end-1]), [',','\n']))

function parseassign(eqex, vars)
    m = match(r"\.\.", eqex)
    m === nothing && error("cannot parse ", eqex)
    eqex = strip(eqex[m.offset+2:end-1])  # strip off the ;
    m = match(r"=[eE]=", eqex)
    m === nothing && error("cannot parse ", eqex)
    lhs, rhs = strip(eqex[1:m.offset-1]), strip(eqex[m.offset+3:end])
    lhs, rhs = replaceexprs(lhs, vars), replaceexprs(rhs, vars)
    if lhs ∈ vars
        return :($(Symbol(lhs)) = $(Symbol(rhs)))
    elseif rhs ∈ vars
        return :($(Symbol(rhs)) = $(Symbol(lhs)))
    elseif rhs == "0"
        for v in reverse(vars)
            idx = search(lhs, v)
            isempty(idx) && continue
            isempty(search(lhs, v, last(idx))) || continue
            # find the previous operator
            i = last(idx)
            while i > 0 && ((c = lhs[i]) != '+' && c != '-')
                i = prevind(lhs, i)
            end
            @assert(i>0)
            c = lhs[i] == '+' ? -1 : 1
            rest = lhs[1:i-1]
            restex = parse(rest)
            return :($(Symbol(v)) = $c * $restex)
        end
    else
        error("neither the lhs $lhs nor rhs $rhs appeared in $vars")
    end
end

function replaceexprs(str, vars)
    ex, _ = parse(str, 1)
    newex = replaceexprs!(ex)
    vsym = varsym.(vars)
    finalex = replacecall!(newex, vsym)
    string(finalex)
end

function replaceexprs!(ex::Expr)
    if ex.head == :call && ex.args[1] == :smax
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
        return Expr(:comprehension, Expr(:generator, val, index_exprs...))
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

function replacecall!(ex::Expr, vars)
    for i = 1:length(ex.args)
        ex.args[i] = replacecall!(ex.args[i], vars)
    end
    if ex.head == :call
        if ex.args[1] ∈ vars
            return Expr(:ref, ex.args...)
        end
    end
    return ex
end
replacecall!(ex, vars) = return ex

function varsym(str)
    ex, _ = parse(str, 1)
    ex isa Symbol && return ex
    if ex isa Expr
        if ex.head == :call
            return ex.args[1]
        end
    end
    error("oops")
end

end # module
