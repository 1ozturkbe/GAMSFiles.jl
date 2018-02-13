# Full list of GAMS reserved words
# sum, prod, smin, and smax have been removed and added to gamsfuncs
# so too and and or
const gamskws = Set(["abort",
                     "acronym",
                     "acronyms",
                     "alias",
                     "all",
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
                     "ord",
                     "parameter",
                     "parameters",
                     "positive",
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
                     "solve",
                     "sos1",
                     "sos2",
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

const vartypes = ("free", "positive", "negative", "binary", "integer")

const varattributes = ("lo", "up", "fx", "l", "m", "scale", "prior")

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

const operatortokens = ("+", "-", "*", "/", "^", "=e=", "=g=", "=l=")

const eqops = Dict("=e=" => :(=), "=g=" => :>, "=l=" => :<)

# From table 6.1 in the manual
const funcnames = Set(["errorf",
                       "exp",
                       "log",
                       "log10",
                       "normal",
                       "uniform",
                       "abs",
                       "ceil",
                       "floor",
                       "mapval",
                       "max",
                       "min",
                       "mod",
                       "power",
                       "round",
                       "sign",
                       "sqr",
                       "sqrt",
                       "trunc",
                       "arctan",
                       "cos",
                       "sin",
                       "prod",
                       "smax",
                       "smin",
                       "sum",
                       "and",
                       "or"])

const funcsubst = Dict(["power"=>"^",
                        "arctan"=>"atan"])

# Functions used in GAMS expressions
const gamsfuncs = quote
    sqr(x) = x*x
    power(x,y) = x^y
    errorf(x) = 0.5 * (1 + erf(x))
    normal(x,y) = y*randn() + x
    uniform(x,y) = (y-x)*rand() + x
    and(x, y) = x & y
    or(x, y)  = x | y
    arctan(x) = atan(x)
end

@eval module GamsFuncs $gamsfuncs end

# Translations of GAMS reduction functions to Julia functions
const gamsf2jf = Dict(:sum=>:sum, :prod=>:prod, :smax=>:maximum, :smin=>:minimum)
