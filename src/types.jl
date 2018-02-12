## Lexer types
abstract type AbstractLex end

struct Keyword <: AbstractLex
    text::String
end

struct GText <: AbstractLex
    text::String
end

struct Slashed <: AbstractLex
    text::String
end

struct GNumber{T<:Union{Int,Float64}} <: AbstractLex
    val::T
end

struct GArray{N} <: AbstractLex
    name::String
    indices::NTuple{N,Any}
end
GArray(name::AbstractString, indices::NTuple{N,Any}) where N =
    GArray{N}(name, indices)

struct Table{N} <: AbstractLex
    name::GArray{N}
    body::String
end

struct Dots <: AbstractLex
    text::String
end

# perhaps Operator, Relation?

struct GCall <: AbstractLex
    name::String
    args::Vector{Any}
end

struct Parens <: AbstractLex
    args::Vector{Any}
end

struct StatementEnd <: AbstractLex end

# For comparisons and hashing, we deliberately exclude the indices of GArray,
# since dictionary lookup wants to return the variable independently of the
# specific indices being accessed.
Base.:(==)(::AbstractLex, ::AbstractLex) = false
Base.:(==)(a::T, b::T) where T<:Union{Keyword,GText,Slashed,Dots} = a.text == b.text
Base.:(==)(a::GNumber, b::GNumber) = a.val == b.val
Base.:(==)(a::GArray{N}, b::GArray{N}) where N = a.name == b.name # && a.indices == b.indices
Base.:(==)(a::Table{N}, b::Table{N}) where N = a.name == b.name && a.body == b.body
Base.:(==)(a::GCall, b::GCall) = a.name == b.name && a.args == b.args
Base.:(==)(a::Parens, b::Parens) = a.args == b.args
Base.:(==)(::StatementEnd, ::StatementEnd) = true

const hash_lex = Sys.WORD_SIZE == 64 ? 0xc4e3eb2da1eebf4a : 0x834afa53

function Base.hash(x::AbstractLex, h::UInt)
    h ⊻= hash_lex
    _hash(x, h)
end
_hash(x::Keyword, h::UInt) = hash(x.text, hash(1, h))
_hash(x::GText, h::UInt)   = hash(x.text, hash(2, h))
_hash(x::GArray, h::UInt)  = hash(x.name, hash(3, h)) # hash(x.indices, )
_hash(x::Table, h::UInt)   = hash(x.body, hash(x.name, hash(4, h)))
_hash(x::Slashed, h::UInt) = hash(x.text, hash(5, h))
_hash(x::GNumber, h::UInt) = hash(x.val, hash(6, h))
_hash(x::Dots, h::UInt)    = hash(x.text, hash(7, h))
_hash(x::GCall, h::UInt)   = hash(x.args, hash(x.name, hash(8, h)))
_hash(x::Parens, h::UInt)  = hash(x.args, hash(9, h))
_hash(x::StatementEnd, h::UInt) = hash(10, h)

Base.convert(::Type{String}, l::Union{Keyword,GText,Slashed}) = l.text
Base.string(l::Union{Keyword,GText,Slashed}) = l.text
Base.string(l::GNumber) = string(l.val)
function Base.string(part::GArray)
    str = part.name * '(' * part.indices[1]
    for j = 2:length(part.indices)
        str *= ',' * part.indices[j]
    end
    str *= ')'
    return str
end

Base.convert(::Type{Expr}, l::GNumber) = l.val
Base.convert(::Type{Expr}, l::GText) = Symbol(string(l))
Base.convert(::Type{Expr}, l::GArray) =
    Expr(:ref, Symbol(getname(l)), map(x->convert(Expr, x), l.indices)...)
Base.convert(::Type{Expr}, l::GCall) =
    Expr(:call, Symbol(getname(l)), map(x->convert(Expr, x), l.args)...)
function Base.convert(::Type{Expr}, l::Parens)
    @assert length(l.args) == 1
    ex = convert(Expr, l.args[1])
    return :( ($ex) )
end

getname(a::GArray) = a.name
getname(a::Table) = a.name
getname(a::GCall) = a.name
getname(a::AbstractLex) = a.text

sexpr(io::IO, l::Union{Keyword,Slashed,Table,Dots}) = print(io, l)
sexpr(io::IO, l::GText)   = print(io, l.text)
sexpr(io::IO, l::GNumber) = print(io, l.val)
sexpr(io::IO, l::GArray)  = print_delim(io, l.name, l.indices, '[', ']')
sexpr(io::IO, l::GCall)   = print_delim(io, l.name, l.args, '(', ')')
sexpr(io::IO, l::Parens)  = print_delim(io, "", l.args, '(', ')')
sexpr(io::IO, l::StatementEnd) = print(io, ';')
function print_delim(io, name, args, opener, closer)
    print(io, name, opener)
    sexpr(io, args[1])
    for i = 2:length(args)
        print(io, ',')
        sexpr(io, args[i])
    end
    print(io, closer)
end


## Parser types

# Attributes of a Variable
struct VarInfo{Axs<:Tuple}
    typ::String  # free, positive, negative, integer, binary
    axs::Axs
    assignments::Vector{Pair{Any,Any}}   # assignments that set lo, up, ...

    function VarInfo{Axs}(typ::AbstractString, axs) where Axs
        typ ∈ vartypes || error(typ, " must be one of ", vartypes)
        new{Axs}(typ, axs, Pair{Any,Any}[])
    end
end
VarInfo(typ::AbstractString, axs::Tuple{Vararg{<:AbstractUnitRange}}) =
    VarInfo{typeof(axs)}(typ, axs)

struct ModelInfo
    equations::Vector{String}
    assignments::Dict{String,Any}

    ModelInfo(equations) = new(equations, Dict{String,Any}())
end
