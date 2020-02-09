module GAMSTest

using GAMSFiles
using Test

glex(str) = GAMSFiles.lex(seekstart(IOBuffer(str)))

gparse(str) = GAMSFiles.parseexprs(glex(str))
function gparse1(str)
    parsed = gparse(str)
    @assert(length(parsed) == 1)
    return parsed[1]
end
function sexprstr(expr)
    io = IOBuffer()
    sexpr(io, expr)
    String(take!(io))
end

isline(a) = (isa(a, Expr) && a.head == :line) || isa(a, LineNumberNode)

# Compare expressions, skipping line numbers
# Doesn't elide trivial blocks, etc.
function cmpexprs(ex1::Expr, ex2::Expr)
    ex1.head == ex2.head || return false
    i = j = 1
    while i <= length(ex1.args) && j <= length(ex2.args)
        while i <= length(ex1.args) && isline(ex1.args[i])
            i += 1
        end
        while j <= length(ex2.args) && isline(ex2.args[j])
            j += 1
        end
        i > length(ex1.args) && j > length(ex2.args) && return true
        i > length(ex1.args) || j > length(ex2.args) && return false
        if !cmpexprs(ex1.args[i], ex2.args[j])
            return false
        end
        i += 1
        j += 1
    end
    return i > length(ex1.args) && j > length(ex2.args)
end
cmpexprs(a, b) = a == b

@testset "Lexing" begin
    @test glex("123") == GAMSFiles.AbstractLex[GAMSFiles.GNumber(123)]
    @test glex("123.0") == GAMSFiles.AbstractLex[GAMSFiles.GNumber(123.0)]
    @test glex("123.0e-5") == GAMSFiles.AbstractLex[GAMSFiles.GNumber(123.0e-5)]
    lexed = glex("x.l('1')")
    @test lexed == GAMSFiles.AbstractLex[GAMSFiles.GText("x"), GAMSFiles.Dots("."), GAMSFiles.GArray("l", ("'1'",))]
    # Because == for GArrays ignores indices, check explicitly
    @test lexed[3].indices == (GAMSFiles.GNumber(1),)
    @test glex("x=0") == GAMSFiles.AbstractLex[GAMSFiles.GText("x"), GAMSFiles.GText("="), GAMSFiles.GNumber(0)]
    @test glex("x=E=0") == GAMSFiles.AbstractLex[GAMSFiles.GText("x"), GAMSFiles.GText("=e="), GAMSFiles.GNumber(0)]
end

@testset "parse_slashed!" begin
    function slashtest!(dest, str)
        empty!(dest)
        io = seekstart(IOBuffer(str))
        GAMSFiles.parse_slashed!(dest, GAMSFiles.lex(io), 1)
        return dest
    end

    dest = Dict{String,Vector{String}}()
    slashtest!(dest, "set name /1*2/")
    @test length(dest) == 1 && dest["name"] == ["1*2"]
    slashtest!(dest, "sets name /1*2/;")
    @test length(dest) == 1 && dest["name"] == ["1*2"]
    slashtest!(dest, "sets name \"dollars/ounce\" /1*2/")
    @test length(dest) == 1 && dest["name"] == ["1*2"]
    slashtest!(dest, "set name \"dollars per ounce\" /1*2/;")
    @test length(dest) == 1 && dest["name"] == ["1*2"]
    slashtest!(dest, "set name /a, b, c/")
    @test length(dest) == 1 && dest["name"] == ["a", "b", "c"]
    slashtest!(dest, "set name /a \"apple\", b \"banana\", c \"carrot\"/")
    @test length(dest) == 1 && dest["name"] == ["a", "b", "c"]
    slashtest!(dest, "set name /a apple, b banana, c carrot/")
    @test length(dest) == 1 && dest["name"] == ["a", "b", "c"]
    slashtest!(dest, "sets a /1*2/\n  b   / 1*3 /")
    @test length(dest) == 2 && dest["a"] == ["1*2"] && dest["b"] == ["1*3"]
    slashtest!(dest, "sets a /1*2/\n  b   / 1*3 /;")
    @test length(dest) == 2 && dest["a"] == ["1*2"] && dest["b"] == ["1*3"]
    # Some examples from the manual (chapter on sets)
    slashtest!(dest, "set cq \"nutrients\" / N, P2O5 / ;")
    @test length(dest) == 1 && dest["cq"] == ["N", "P2O5"]
    slashtest!(dest, "set cq \"nutrients\" / N\n   P2O5 / ;")
    @test length(dest) == 1 && dest["cq"] == ["N", "P2O5"]
    slashtest!(dest, """set
f         "final products"
/yncrude         "refined crude (million barrels)"
lpg                "liquified petroleum gas(million barrels)"
ammonia          "ammonia (million tons)"
coke       "coke (million tons)"
sulfur          "sulfur (million tons)"
/;
""")
    @test length(dest) == 1 && dest["f"] == ["yncrude", "lpg", "ammonia", "coke", "sulfur"]
    slashtest!(dest, """sets
s   "Sector"    /  manuf
                   agri
                   services
                   government  /
r   "regions"  /  north
                  eastcoast
                  midwest
                  sunbelt    / ;
""")
    @test length(dest) == 2 && dest["s"] == ["manuf", "agri", "services", "government"] &&
        dest["r"] == ["north", "eastcoast", "midwest", "sunbelt"]
end

@testset "Expression parsing" begin
    parsed = gparse1("-5 + 2")
    @test parsed isa GAMSFiles.GCall && parsed.name == "+"
    @test length(parsed.args) == 2
    @test parsed.args[1] == GAMSFiles.GNumber(-5)
    @test parsed.args[2] == GAMSFiles.GNumber(2)
    for str in ("1 + 3 - 2", "1+3 - 2", "1 + 3-2", "1+3-2")
        parsed = gparse1(str)
        @test parsed isa GAMSFiles.GCall && parsed.name == "-"
        @test length(parsed.args) == 2
        @test parsed.args[1] == GAMSFiles.GCall("+", [GAMSFiles.GNumber(1), GAMSFiles.GNumber(3)])
        @test parsed.args[2] == GAMSFiles.GNumber(2)
        # Also test sexpr since we'll use that for future tests
        @test sexprstr(parsed) == "-(+(1,3),2)"
    end
    for str in ("0.3*x", "0.3 * x", "+0.3*x", "+ 0.3 * x")
        parsed = gparse1(str)
        @test sexprstr(parsed) == "*(0.3,x)"
    end
    @test_throws AssertionError gparse(" * 0.3")  # the space in front is needed to avoid interpretation as comment
    parsed = gparse1("0.2*sin(x+4)")
    @test sexprstr(parsed) == "*(0.2,sin(+(x,4)))"
    @test parsed isa GAMSFiles.GCall && parsed.name == "*"
    @test length(parsed.args) == 2
    @test parsed.args[1] == GAMSFiles.GNumber(0.2)
    @test parsed.args[2] == GAMSFiles.GCall("sin", [GAMSFiles.GCall("+", [GAMSFiles.GText("x"), GAMSFiles.GNumber(4)])])

    parsed = gparse1("power(x+1, y)")
    @test sexprstr(parsed) == "power(+(x,1),y)"
    @test parsed isa GAMSFiles.GCall && parsed.name == "power"
    @test length(parsed.args) == 2
    @test parsed.args[1] == GAMSFiles.GCall("+", [GAMSFiles.GText("x"), GAMSFiles.GNumber(1)])
    @test parsed.args[2] == GAMSFiles.GText("y")

    parsed = gparse1("abs(1 - 2*t(j))")
    @test parsed isa GAMSFiles.GCall && parsed.name == "abs"
    @test length(parsed.args) == 1
    parsed = parsed.args[1]
    @test parsed isa GAMSFiles.GCall && parsed.name == "-"
    @test length(parsed.args) == 2
    @test parsed.args[1] == GAMSFiles.GNumber(1)
    @test parsed.args[2] == GAMSFiles.GCall("*", [GAMSFiles.GNumber(2), GAMSFiles.GArray("t", (GAMSFiles.GText("j"),))])

    parsed = gparse1("abs(2*t(j)-1)")
    @test parsed isa GAMSFiles.GCall && parsed.name == "abs"
    @test length(parsed.args) == 1
    parsed = parsed.args[1]
    @test parsed isa GAMSFiles.GCall && parsed.name == "-"
    @test length(parsed.args) == 2
    @test parsed.args[1] == GAMSFiles.GCall("*", [GAMSFiles.GNumber(2), GAMSFiles.GArray("t", (GAMSFiles.GText("j"),))])
    @test parsed.args[2] == GAMSFiles.GNumber(1)
end

@testset "evalconst" begin
    ex = gparse("5")
    @test GAMSFiles.evalconst(ex[1]) == (5, true)
    ex = gparse("5-2")
    @test GAMSFiles.evalconst(ex[1]) == (3, true)
    pi2 = pi/2
    ex = gparse("sin($pi2)")
    x, success = GAMSFiles.evalconst(ex[1])
    @test x ≈ 1 && success
    z = Float64(pi)
    ex = gparse("sin($z/2)")
    x, success = GAMSFiles.evalconst(ex[1])
    @test x ≈ 1 && success
    ex = gparse("sin(pi/2)")
    x, success = GAMSFiles.evalconst(ex[1])
    @test isnan(x) && !success
    ex = gparse("2**4")
    @test GAMSFiles.evalconst(ex[1]) == (16, true)
    ex = gparse("1+2^3*7-5")
    @test GAMSFiles.evalconst(ex[1]) == (52, true)
    ex = gparse("1+2^(5-2)*7-5")
    @test GAMSFiles.evalconst(ex[1]) == (52, true)
end

@testset "parseconsts" begin
    gams = cd(joinpath(@__DIR__, "gams")) do
        parsegams("problem2.18.gms")
    end
    GAMSFiles.parseconsts!(gams)
    params = gams["parameters"]
    @test getwithkey(params, "pi")[] == 3.14159265358979324
    @test getwithkey(params, "aux") == (a = collect(1:41); a[31] = 21; a)
    @test getwithkey(params, "t") == [0,0.01,0.02,0.03,0.04,0.05,0.07,0.1,0.13,0.16,0.19,0.22,0.25,0.28,0.31,0.34,0.37,0.4,0.43,0.46,0.5,0.54,0.57,0.6,0.63,0.66,0.69,0.72,0.75,0.78,0.81,0.84,0.87,0.9,0.93,0.95,0.96,0.97,0.98,0.99,1]
    gams = cd(joinpath(@__DIR__, "gams")) do
        parsegams("convex2_5_2.gms")
    end
    GAMSFiles.parseconsts!(gams)
    tables = gams["tables"]
    @test getwithkey(tables, "A") == [ 0.1808  1.1179 -34.6750 -0.7402 -0.4768;
                                       1.5166 -0.5267   2.3941  0.1376  1.0633;
                                      -0.7785  0.2988  21.7019 -0.0612  0.2740;
                                       0.9350 -1.8687  33.7391 -0.9762  0.0000]
end

@testset "parseassignments" begin
    sets = Dict("i" => Base.OneTo(5), "j" => 2:4)
    pconst = GAMSFiles.GText("pi")
    pveci = GAMSFiles.GArray("x", (GAMSFiles.GText("i"),))
    pvecj = GAMSFiles.GArray("y", (GAMSFiles.GText("j"),))
    pmat =  GAMSFiles.GArray("z", (GAMSFiles.GText("i"),GAMSFiles.GText("j")))
    params = Dict(pconst => GAMSFiles.allocate(pconst, sets),
                  pveci  => GAMSFiles.allocate(pveci, sets),
                  pvecj  => GAMSFiles.allocate(pvecj, sets),
                  pmat   => GAMSFiles.allocate(pmat, sets))
    @test params[pconst] isa Base.RefValue
    @test axes(params[pveci]) === (sets["i"],)
    @test axes(params[pvecj]) === (sets["j"],)
    @test axes(params[pmat])  === (UnitRange(sets["i"]),sets["j"])
    noeval = gparse("cos(alpha)")[1]  # an expression that can't be reduced to a constant
    exprs = GAMSFiles.parseassignments!(Expr[], [pconst=>GAMSFiles.GNumber(3)], params, sets)
    @test params[pconst][] == 3 && isempty(exprs)
    exprs = GAMSFiles.parseassignments!(Expr[], [pconst=>noeval], params, sets)
    @test params[pconst][] == 3 && exprs == [:(pi = cos(alpha))]
    exprs = GAMSFiles.parseassignments!(Expr[], [gparse("x(3)")[1]=>GAMSFiles.GNumber(1.7)], params, sets)
    @test params[pveci][3] == 1.7 && isempty(exprs)
    global y
    y = params[pvecj]
    fill!(y, -1)
    exprs = GAMSFiles.parseassignments!(Expr[], [gparse("y(j)")[1]=>GAMSFiles.GNumber(-3)], params, sets)
    @test params[pvecj][3] == -1
    eval(exprs[1])
    @test all(y .== -3)
    exprs = GAMSFiles.parseassignments!(Expr[], [gparse("z(3,4)")[1]=>GAMSFiles.GNumber(2.3)], params, sets)
    @test params[pmat][3,4] == 2.3 && isempty(exprs)
    exprs = GAMSFiles.parseassignments!(Expr[], [gparse("z(i,j)")[1]=>noeval], params, sets)
    @test params[pmat][3,4] == 2.3
    global z, alpha
    z, alpha = params[pmat], π/4
    eval(exprs[1])
    @test z ≈ fill(1/sqrt(2), 1:5, 2:4)
end

@testset "solvefor" begin
    eq = gparse1("x =e= 7")
    @test GAMSFiles.solvefor(eq, "x") == (eq, true)
    @test GAMSFiles.solvefor(eq, "y") == (eq, false)
    @test GAMSFiles.solvefor(gparse1("7 =e= x"), "x") == (eq, true)
    @test GAMSFiles.solvefor(gparse1("x + z =e= 0"), "x") == (gparse1("x =e= 0 - z"), true)
    @test GAMSFiles.solvefor(gparse1("z + x =e= 1"), "x") == (gparse1("x =e= 1 - z"), true)
    @test GAMSFiles.solvefor(gparse1("0 =e= x + z"), "x") == (gparse1("x =e= 0 - z"), true)
    @test GAMSFiles.solvefor(gparse1("1 =e= z + x"), "x") == (gparse1("x =e= 1 - z"), true)
    @test GAMSFiles.solvefor(gparse1("x - z =e= 0"), "x") == (gparse1("x =e= 0 + z"), true)
    @test GAMSFiles.solvefor(gparse1("z - x =e= 1"), "x") == (gparse1("x =e= z - 1"), true)
    @test GAMSFiles.solvefor(gparse1("0 =e= x - z"), "x") == (gparse1("x =e= 0 + z"), true)
    @test GAMSFiles.solvefor(gparse1("1 =e= z - x"), "x") == (gparse1("x =e= z - 1"), true)
    @test GAMSFiles.solvefor(gparse1("(x + 2) + z =e= -5"), "x") == (gparse1("x =e= -5 - z - 2"), true)
    @test GAMSFiles.solvefor(gparse1("z + (x + 2) =e= -5"), "x") == (gparse1("x =e= -5 - z - 2"), true)
    @test GAMSFiles.solvefor(gparse1("z - (x + 2) =e= 5"), "x") == (gparse1("x =e= z - 5 - 2"), true)
    @test GAMSFiles.solvefor(gparse1("z - (2 - x) =e= 5"), "x") == (GAMSFiles.deparen!(gparse1("x =e= 2 - (z - 5)")), true)

    @test GAMSFiles.solvefor(gparse1("7 =g= x"), "x") == (gparse1("x =l= 7"), true)
    @test GAMSFiles.solvefor(gparse1("x + z =g= 0"), "x") == (gparse1("x =g= 0 - z"), true)
    @test GAMSFiles.solvefor(gparse1("z + x =l= 1"), "x") == (gparse1("x =l= 1 - z"), true)
    @test GAMSFiles.solvefor(gparse1("0 =g= x + z"), "x") == (gparse1("x =l= 0 - z"), true)
    @test GAMSFiles.solvefor(gparse1("1 =l= z + x"), "x") == (gparse1("x =g= 1 - z"), true)
    @test GAMSFiles.solvefor(gparse1("x - z =g= 0"), "x") == (gparse1("x =g= 0 + z"), true)
    @test GAMSFiles.solvefor(gparse1("z - x =l= 1"), "x") == (gparse1("x =g= z - 1"), true)
    @test GAMSFiles.solvefor(gparse1("0 =g= x - z"), "x") == (gparse1("x =l= 0 + z"), true)
    @test GAMSFiles.solvefor(gparse1("1 =g= z - x"), "x") == (gparse1("x =g= z - 1"), true)
    @test GAMSFiles.solvefor(gparse1("(x + 2) + z =g= -5"), "x") == (gparse1("x =g= -5 - z - 2"), true)
    @test GAMSFiles.solvefor(gparse1("z + (x + 2) =l= -5"), "x") == (gparse1("x =l= -5 - z - 2"), true)
    @test GAMSFiles.solvefor(gparse1("z - (x + 2) =g= 5"), "x") == (gparse1("x =l= z - 5 - 2"), true)
    @test GAMSFiles.solvefor(gparse1("z - (2 - x) =g= 5"), "x") == (GAMSFiles.deparen!(gparse1("x =g= 2 - (z - 5)")), true)
end

@testset "eqs2assigns" begin
    # Do the examples in the docstring
    equations = [gparse1("eq1")=>gparse1("x + objval =e= 2")]
    vars = Dict("x"=>nothing, "objval"=>nothing)
    sets = Dict()
    bodyexprs = Expr[]
    solved = GAMSFiles.eqs2assigns!(bodyexprs, equations, vars, sets, "objval")
    @test solved["objval"]
    @test !solved["x"]
    @test isempty(equations)
    @test bodyexprs == [:(objval = 2 - x)]

    equations = [gparse1("eq1(i)") => gparse1("2*x(i) - y =g=0"),
                 gparse1("eq0") => gparse1("5*y - objval =e= 0")]
    vars = Dict("x"=>nothing, "y"=>nothing, "objval"=>nothing)
    sets = Dict("i" => 1:3)
    bodyexprs = Expr[]
    solved = GAMSFiles.eqs2assigns!(bodyexprs, equations, vars, sets, "objval")
    @test solved["objval"]
    @test solved["y"]
    @test !solved["x"]
    @test isempty(equations)
    @test length(bodyexprs) == 3
    @test bodyexprs[1] == :(y = $Inf)
    ex = bodyexprs[2]
    if ex.head == :block && isline(ex.args[1])
        ex = ex.args[2]
    end
    @test cmpexprs(ex, :(for i = $(1:3) y = min(y, 2*x[i]-0) end))
    @test bodyexprs[3] == :(objval = 5*y-0)

    # sets = Dict("i" => Base.OneTo(5), "j" => 2:4)
    # vars = Dict("x" => nothing, "y" => nothing)
    # # Test that equations get turned into assignment statements
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("x =e= 7")[1]], vars, sets)
    # @test exprs == [:(x = 7)]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("7 =e= x")[1]], vars, sets)
    # @test exprs == [:(x = 7)]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("x =e= y")[1]], vars, sets)
    # @test exprs == [:(x = y)]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("y =e= x")[1]], vars, sets)
    # @test exprs == [:(y = x)]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("x + z =e= 0")[1]], vars, sets)
    # @test exprs == [:(x = 0 - z)]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("z + x =e= 0")[1]], vars, sets)
    # @test exprs == [:(x = 0 - z)]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("0 =e= z + x")[1]], vars, sets)
    # @test exprs == [:(x = 0 - z)]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("0 =e= x + z")[1]], vars, sets)
    # @test exprs == [:(x = 0 - z)]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("x - z =e= 0")[1]], vars, sets)
    # @test exprs == [:(x = 0 + z)]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("z - x =e= 0")[1]], vars, sets)
    # @test exprs == [:(x = z - 0)]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("0 =e= z - x")[1]], vars, sets)
    # @test exprs == [:(x = z - 0)]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("0 =e= x - z")[1]], vars, sets)
    # @test exprs == [:(x = 0 + z)]

    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("x =g= 7")[1]], vars, sets)
    # @test exprs == [:(x = $(-Inf)), :(x = max(x, 7))]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("x =l= 7")[1]], vars, sets)
    # @test exprs == [:(x = $Inf), :(x = min(x, 7))]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("7 =g= x")[1]], vars, sets)
    # @test exprs == [:(x = $Inf), :(x = min(x, 7))]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("7 =l= x")[1]], vars, sets)
    # @test exprs == [:(x = $(-Inf)), :(x = max(x, 7))]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("x =l= y")[1]], vars, sets)
    # @test exprs == [:(x = $Inf), :(x = min(x, y))]
    # exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("y =l= x")[1]], vars, sets)
    # @test exprs == [:(y = $Inf), :(y = min(y, x))]

    # for (rel, f, g, sent) in (("=g=", :max, :min, -Inf), ("=l=", :min, :max, Inf))
    #     exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("x + z $rel 0")[1]], vars, sets)
    #     @test exprs == [:(x = $sent), :(x = $f(x, 0 - z))]
    #     exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("z + x $rel 0")[1]], vars, sets)
    #     @test exprs == [:(x = $sent), :(x = $f(x, 0 - z))]
    #     exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("0 $rel z + x")[1]], vars, sets)
    #     @test exprs == [:(x = $(-sent)), :(x = $g(x, 0 - z))]
    #     exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("0 $rel x + z")[1]], vars, sets)
    #     @test exprs == [:(x = $(-sent)), :(x = $g(x, 0 - z))]
    #     exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("x - z $rel 0")[1]], vars, sets)
    #     @test exprs == [:(x = $sent), :(x = $f(x, 0 + z))]
    #     exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("z - x $rel 0")[1]], vars, sets)
    #     @test exprs == [:(x = $(-sent)), :(x = $g(x, z - 0))]
    #     exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("0 $rel z - x")[1]], vars, sets)
    #     @test exprs == [:(x = $sent), :(x = $f(x, z - 0))]
    #     exprs = GAMSFiles.eqs2assigns!(Expr[], ["e1" => gparse("0 $rel x - z")[1]], vars, sets)
    #     @test exprs == [:(x = $(-sent)), :(x = $g(x, 0 + z))]
    # end
end

# This is only needed if some problems don't take random inputs or we want to check the returned value
inputs = Dict("beale.gms"=>rand(2),
              "convex4_10_1.gms"=>rand(10),
              "problem2.1.gms"=>rand(2),
              "yfit.gms"=>rand(3),
              "steiner_vareps.gms"=>rand(17),
              "tointqor.gms"=>rand(50),
              "convex2_5_2.gms"=>rand(5),
              "problem2.10.gms"=>rand(4),
              "convex3_10_1.gms"=>rand(10),
              "zangwil2.gms"=>rand(2),
              "problem3.21.gms"=>rand(48),
              "problem2.24.gms"=>rand(20),
              "problem3.17.gms"=>rand(10),
              "problem2.18.gms"=>rand(9))

cd(joinpath(@__DIR__, "gams")) do
    for file in readdir()
        println(file)
        modex, axs = parsegams(Module, file)
        mod = eval(modex)
        f = getfield(mod, :objective)
        x = inputs[file]
        @assert(axes(x) == axes(axs[1]))
        @test isreal(Base.invokelatest(f, x))
    end
end

end
