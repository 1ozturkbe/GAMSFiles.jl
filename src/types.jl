## Lexer types
abstract type AbstractLex end

struct Keyword <: AbstractLex
    text::String
end

struct GText <: AbstractLex
    text::String
end

struct GArray{N} <: AbstractLex
    name::String
    indices::NTuple{N,String}
end
GArray(name::AbstractString, indices::NTuple{N,AbstractString}) where N =
    GArray{N}(name, indices)

struct Table{N} <: AbstractLex
    name::GArray{N}
    body::String
end

struct Slashed <: AbstractLex
    text::String
end

struct GNumber{T<:Union{Int,Float64}} <: AbstractLex
    val::T
end

struct Dots <: AbstractLex
    text::String
end

struct StatementEnd <: AbstractLex end

Base.:(==)(::AbstractLex, ::AbstractLex) = false
Base.:(==)(a::T, b::T) where T<:Union{Keyword,GText,Slashed,Dots} = a.text == b.text
Base.:(==)(a::GNumber, b::GNumber) = a.val == b.val
Base.:(==)(a::GArray{N}, b::GArray{N}) where N = a.name == b.name # && a.indices == b.indices
Base.:(==)(a::Table{N}, b::Table{N}) where N = a.name == b.name && a.body == b.body
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
_hash(x::StatementEnd, h::UInt) = hash(8, h)

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

## Parser types

# Attributes of a Variable
struct VarInfo{Axs<:Tuple}
    typ::String  # free, positive, negative, integer, binary
    axs::Axs
    assignments::Vector{Pair{Any,String}}   # assignments that set lo, up, ...

    function VarInfo{Axs}(typ::AbstractString, axs) where Axs
        typ ∈ vartypes || error(typ, " must be one of ", vartypes)
        new{Axs}(typ, axs, Pair{String,String}[])
    end
end
VarInfo(typ::AbstractString, axs::Tuple{Vararg{<:AbstractUnitRange}}) =
    VarInfo{typeof(axs)}(typ, axs)

struct ModelInfo
    equations::Vector{String}
    assignments::Dict{String,String}

    ModelInfo(equations) = new(equations, Dict{String,String}())
end
