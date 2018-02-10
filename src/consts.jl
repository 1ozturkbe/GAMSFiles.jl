# Full list of GAMS reserved words
const gamskws = Set(["abort",
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
                     "yes"])

const stdkws = Dict("acronym"=>"acronyms",
                    "equation"=>"equations",
                    "file"=>"files",
                    "model"=>"models",
                    "option"=>"options",
                    "parameter"=>"parameters",
                    "scalar"=>"scalars",
                    "set"=>"sets",
                    "variable"=>"variables")

# Declarations
const gamsdecls = Set(["acronym", "acronyms",
                       "alias",
                       "equation", "equations",
                       "model", "models",
                       "parameter", "parameters",
                       "scalar", "scalars",
                       "set", "sets",
                       "table",
                       "variable", "variables"])

# Actions
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

const vartypes = ("free", "positive", "negative", "binary", "integer")

const varattributes = ("lo", "hi", "fx", "l", "m", "scale", "prior")

const modelattributes = Set(["bratio",
                             "domlim",
                             "holdfixed",
                             "iterlim",
                             "limcol",
                             "limrow",
                             "optca",
                             "optcr",
                             "optfile",
                             "reslim",
                             "scaleopt",
                             "solprint",
                             "solveopt",
                             "sysout",
                             "workspace"])

const eqops = Dict("e" => :(=), "E" => :(=), "g" => :>, "G" => :>, "l" => :<, "L" => :<)

# Functions used in GAMS expressions
const gamsfuncs = quote
    sqr(x) = x*x
    POWER(x,p) = x^p
    power(x,p) = x^p
    arctan(x) = atan(x)
end

# Translations of GAMS functions to Julia functions
const gamsf2jf = Dict(:sum=>:sum, :smax=>:maximum, :smin=>:minimum)

## parsing
const tokbreak   = [' ','\n','\r']
const stmtbreak  = [tokbreak; ';']
const entrybreak = [',',';']
const slashbreak = ['/', ',', '\n', '\r']
const itembreak  = [slashbreak; ' ']
# const itembreak  = ['/', ',']