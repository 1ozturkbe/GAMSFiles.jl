## Base.peekchar doesn't support IOBuffer, this fixes that
@inline function get_next_char(p::Ptr{UInt8}, i::Int, len::Int)
    b = unsafe_load(p, i)
    if b < 0x80
        return Char(b)
    end
    c, i = Base.slow_utf8_next(p, b, i, len)
    return c
end
function peekchar(io::IOBuffer)
    i, len = io.ptr, io.size
    i > len && Base.throw_boundserror(io, i)
    return get_next_char(pointer(io.data), i, len)
end
peekchar(io) = Base.peekchar(io)

lastuint8(io::IOBuffer) = io.data[io.ptr-1]

## Similar to readuntil, but stops before adding any char appearing in delim
function readupto(io::IO, delim)
    out = IOBuffer()
    c = '\0'
    while !eof(io)
        c = read(io, Char)
        if c ∈ delim
            break
        end
        write(out, c)
    end
    return String(take!(out)), c
end

function eatws(io::IO)
    eof(io) && return io
    c = peekchar(io)
    while isspace(c) && !eof(io)
        read(io, Char)
        c = peekchar(io)
    end
    io
end

# function skiptext(io::IO)
#     c = peekchar(io)
#     if c == '"'
#         # Skip over text
#         read(io, Char)
#         readuntil(io, '"')
#         eatws(io)
#         c = peekchar(io)
#     end
#     return c
# end

## Utility to skip the "description" (text) field
function skiptext(io::IO, term)
    c = peekchar(io)
    while !eof(io)
        c ∈ term && return c
        if c == '"'
            # Skip over text between quotes
            read(io, Char)
            readuntil(io, '"')
            eatws(io)
            return peekchar(io)
        end
        if isspace(c)
            read(io, Char)
            c = peekchar(io)
            continue
        end
        readupto(io, (',', '/'))
        eatws(io)
        return eof(io) ? '\0' : peekchar(io)
    end
    return c
end

