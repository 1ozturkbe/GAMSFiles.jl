module GAMSParse

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

declargs(line, prefix) = split(strip(line[length(prefix)+1:end-1]), ',')

function parseassign(eqex, vars)
    m = match(r"\.\.", eqex)
    m === nothing && error("cannot parse ", eqex)
    eqex = strip(eqex[m.offset+2:end-1])  # strip off the ;
    m = match(r"=[eE]=", eqex)
    m === nothing && error("cannot parse ", eqex)
    lhs, rhs = strip(eqex[1:m.offset-1]), strip(eqex[m.offset+3:end])
    if lhs ∈ vars
        return :($(Symbol(lhs)) = $(Symbol(rhs)))
    elseif rhs ∈ vars
        return :($(Symbol(rhs)) = $(Symbol(lhs)))
    elseif rhs == "0"
        for v in reverse(vars)
            idx = search(eqex, v)
            isempty(idx) && continue
            isempty(search(eqex, v, last(idx))) || continue
            # find the previous operator
            i = last(idx)
            while i > 0 && ((c = eqex[i]) != '+' && c != '-')
                i = prevind(eqex, i)
            end
            @assert(i>0)
            c = eqex[i] == '+' ? -1 : 1
            rest = eqex[1:i-1]
            restex = parse(rest)
            return :($(Symbol(v)) = $c * $restex)
        end
    else
        error("neither the lhs $lhs nor rhs $rhs appeared in $vars")
    end
end

end # module
