module GAMSTest

using GAMSParse
using Compat
using Base.Test

@testset "Lexing" begin
    glex(str) = GAMSParse.lex(IOBuffer(str))

    @test glex("123") == GAMSParse.AbstractLex[GAMSParse.GNumber(123)]
    @test glex("123.0") == GAMSParse.AbstractLex[GAMSParse.GNumber(123.0)]
    @test glex("123.0e-5") == GAMSParse.AbstractLex[GAMSParse.GNumber(123.0e-5)]
    lexed = glex("x.l('1')")
    @test lexed == GAMSParse.AbstractLex[GAMSParse.GText("x"), GAMSParse.Dots("."), GAMSParse.GArray("l", ("'1'",))]
    # Because == for GArrays ignores indices, check explicitly
    @test lexed[3].indices == (GAMSParse.GText("1"),)
    @test glex("x=0") == GAMSParse.AbstractLex[GAMSParse.GText("x"), GAMSParse.GText("="), GAMSParse.GNumber(0)]
    @test glex("x=E=0") == GAMSParse.AbstractLex[GAMSParse.GText("x"), GAMSParse.GText("=e="), GAMSParse.GNumber(0)]
end

@testset "parse_slashed!" begin
    function slashtest!(dest, str)
        empty!(dest)
        io = seekstart(IOBuffer(str))
        GAMSParse.parse_slashed!(dest, GAMSParse.lex(io), 1)
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

gparse(str) = GAMSParse.parseexprs(GAMSParse.lex(seekstart(IOBuffer(str))))

@testset "Expression parsing" begin
    for str in ("1 + 3 - 2", "1+3 - 2", "1 + 3-2", "1+3-2")
        parsed = gparse(str)
        @test length(parsed) == 1
        parsed = parsed[1]
        @test parsed isa GAMSParse.GCall && parsed.name == "+"
        @test length(parsed.args) == 2
        @test parsed.args[1] == GAMSParse.GNumber(1)
        @test parsed.args[2] == GAMSParse.GCall("-", [GAMSParse.GNumber(3), GAMSParse.GNumber(2)])
    end
    for str in ("0.3*x", "0.3 * x", "+0.3*x", "+ 0.3 * x")
        parsed = gparse(str)
        @test length(parsed) == 1
        parsed = parsed[1]
        @test parsed isa GAMSParse.GCall && parsed.name == "*"
        @test length(parsed.args) == 2
        @test parsed.args[1] == GAMSParse.GNumber(0.3)
        @test parsed.args[2] == GAMSParse.GText("x")
    end
    @test_throws AssertionError gparse(" * 0.3")  # the space in front is needed to avoid interpretation as comment
    parsed = gparse("0.2*sin(x+4)")
    @test length(parsed) == 1
    parsed = parsed[1]
    @test parsed isa GAMSParse.GCall && parsed.name == "*"
    @test length(parsed.args) == 2
    @test parsed.args[1] == GAMSParse.GNumber(0.2)
    @test parsed.args[2] == GAMSParse.GCall("sin", [GAMSParse.GCall("+", [GAMSParse.GText("x"), GAMSParse.GNumber(4)])])

    parsed = gparse("power(x+1, y)")
    @test length(parsed) == 1
    parsed = parsed[1]
    @test parsed isa GAMSParse.GCall && parsed.name == "power"
    @test length(parsed.args) == 2
    @test parsed.args[1] == GAMSParse.GCall("+", [GAMSParse.GText("x"), GAMSParse.GNumber(1)])
    @test parsed.args[2] == GAMSParse.GText("y")

    parsed = gparse("abs(1 - 2*t(j))")
    @test length(parsed) == 1
    parsed = parsed[1]
    @test parsed isa GAMSParse.GCall && parsed.name == "abs"
    @test length(parsed.args) == 1
    parsed = parsed.args[1]
    @test parsed isa GAMSParse.GCall && parsed.name == "-"
    @test length(parsed.args) == 2
    @test parsed.args[1] == GAMSParse.GNumber(1)
    @test parsed.args[2] == GAMSParse.GCall("*", [GAMSParse.GNumber(2), GAMSParse.GArray("t", (GAMSParse.GText("j"),))])

    parsed = gparse("abs(2*t(j)-1)")
    @test length(parsed) == 1
    parsed = parsed[1]
    @test parsed isa GAMSParse.GCall && parsed.name == "abs"
    @test length(parsed.args) == 1
    parsed = parsed.args[1]
    @test parsed isa GAMSParse.GCall && parsed.name == "-"
    @test length(parsed.args) == 2
    @test parsed.args[1] == GAMSParse.GCall("*", [GAMSParse.GNumber(2), GAMSParse.GArray("t", (GAMSParse.GText("j"),))])
    @test parsed.args[2] == GAMSParse.GNumber(1)
end

@testset "evalconst" begin
    ex = gparse("5")
    @test GAMSParse.evalconst(ex[1]) == (5, true)
    ex = gparse("5-2")
    @test GAMSParse.evalconst(ex[1]) == (3, true)
    pi2 = pi/2
    ex = gparse("sin($pi2)")
    x, success = GAMSParse.evalconst(ex[1])
    @test x ≈ 1 && success
    z = Float64(pi)
    ex = gparse("sin($z/2)")
    x, success = GAMSParse.evalconst(ex[1])
    @test x ≈ 1 && success
    ex = gparse("sin(pi/2)")
    x, success = GAMSParse.evalconst(ex[1])
    @test isnan(x) && !success
    ex = gparse("2**4")
    @test GAMSParse.evalconst(ex[1]) == (16, true)
    ex = gparse("1+2^3*7-5")
    @test GAMSParse.evalconst(ex[1]) == (52, true)
    ex = gparse("1+2^(5-2)*7-5")
    @test GAMSParse.evalconst(ex[1]) == (52, true)
end

@testset "parseconsts" begin
    gams = cd(joinpath(@__DIR__, "gams")) do
        parsegams("problem2.18.gms")
    end
    GAMSParse.parseconsts!(gams)
    params = gams["parameters"]
    @test getwithkey(params, "pi")[] == 3.14159265358979324
    @test getwithkey(params, "aux") == (a = collect(1:41); a[31] = 21; a)
    @test getwithkey(params, "t") == [0,0.01,0.02,0.03,0.04,0.05,0.07,0.1,0.13,0.16,0.19,0.22,0.25,0.28,0.31,0.34,0.37,0.4,0.43,0.46,0.5,0.54,0.57,0.6,0.63,0.66,0.69,0.72,0.75,0.78,0.81,0.84,0.87,0.9,0.93,0.95,0.96,0.97,0.98,0.99,1]
    gams = cd(joinpath(@__DIR__, "gams")) do
        parsegams("convex2_5_2.gms")
    end
    GAMSParse.parseconsts!(gams)
    tables = gams["tables"]
    @test getwithkey(tables, "A") == [ 0.1808  1.1179 -34.6750 -0.7402 -0.4768;
                                       1.5166 -0.5267   2.3941  0.1376  1.0633;
                                      -0.7785  0.2988  21.7019 -0.0612  0.2740;
                                       0.9350 -1.8687  33.7391 -0.9762  0.0000]
end

@testset "parseassignments" begin
    sets = Dict("i" => Base.OneTo(5), "j" => 2:4)
    pconst = GAMSParse.GText("pi")
    pveci = GAMSParse.GArray("x", (GAMSParse.GText("i"),))
    pvecj = GAMSParse.GArray("y", (GAMSParse.GText("j"),))
    pmat =  GAMSParse.GArray("z", (GAMSParse.GText("i"),GAMSParse.GText("j")))
    params = Dict(pconst => GAMSParse.allocate(pconst, sets),
                  pveci  => GAMSParse.allocate(pveci, sets),
                  pvecj  => GAMSParse.allocate(pvecj, sets),
                  pmat   => GAMSParse.allocate(pmat, sets))
    @test params[pconst] isa Base.RefValue
    @test Compat.axes(params[pveci]) === (sets["i"],)
    @test Compat.axes(params[pvecj]) === (sets["j"],)
    @test Compat.axes(params[pmat])  === (UnitRange(sets["i"]),sets["j"])
    noeval = gparse("cos(alpha)")[1]  # an expression that can't be reduced to a constant
    exprs = GAMSParse.parseassignments!(Expr[], [pconst=>GAMSParse.GNumber(3)], params, sets)
    @test params[pconst][] == 3 && isempty(exprs)
    exprs = GAMSParse.parseassignments!(Expr[], [pconst=>noeval], params, sets)
    @test params[pconst][] == 3 && exprs == [:(pi = cos(alpha))]
    exprs = GAMSParse.parseassignments!(Expr[], [gparse("x(3)")[1]=>GAMSParse.GNumber(1.7)], params, sets)
    @test params[pveci][3] == 1.7 && isempty(exprs)
    global y
    y = params[pvecj]
    fill!(y, -1)
    exprs = GAMSParse.parseassignments!(Expr[], [gparse("y(j)")[1]=>GAMSParse.GNumber(-3)], params, sets)
    @test params[pvecj][3] == -1
    eval(exprs[1])
    @test all(y .== -3)
    exprs = GAMSParse.parseassignments!(Expr[], [gparse("z(3,4)")[1]=>GAMSParse.GNumber(2.3)], params, sets)
    @test params[pmat][3,4] == 2.3 && isempty(exprs)
    exprs = GAMSParse.parseassignments!(Expr[], [gparse("z(i,j)")[1]=>noeval], params, sets)
    @test params[pmat][3,4] == 2.3
    global z, alpha
    z, alpha = params[pmat], π/4
    eval(exprs[1])
    @test z ≈ fill(1/sqrt(2), 1:5, 2:4)
end

@testset "parseequations" begin
    sets = Dict("i" => Base.OneTo(5), "j" => 2:4)
    vars = Dict("x" => nothing, "y" => nothing)
    # Test that equations get turned into assignment statements
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("x =e= 7")[1]], vars, sets)
    @test exprs == [:(x = 7)]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("7 =e= x")[1]], vars, sets)
    @test exprs == [:(x = 7)]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("x =e= y")[1]], vars, sets)
    @test exprs == [:(x = y)]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("y =e= x")[1]], vars, sets)
    @test exprs == [:(y = x)]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("x + z =e= 0")[1]], vars, sets)
    @test exprs == [:(x = 0 - z)]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("z + x =e= 0")[1]], vars, sets)
    @test exprs == [:(x = 0 - z)]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("0 =e= z + x")[1]], vars, sets)
    @test exprs == [:(x = 0 - z)]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("0 =e= x + z")[1]], vars, sets)
    @test exprs == [:(x = 0 - z)]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("x - z =e= 0")[1]], vars, sets)
    @test exprs == [:(x = 0 + z)]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("z - x =e= 0")[1]], vars, sets)
    @test exprs == [:(x = z - 0)]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("0 =e= z - x")[1]], vars, sets)
    @test exprs == [:(x = z - 0)]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("0 =e= x - z")[1]], vars, sets)
    @test exprs == [:(x = 0 + z)]

    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("x =g= 7")[1]], vars, sets)
    @test exprs == [:(x = $(-Inf)), :(x = max(x, 7))]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("x =l= 7")[1]], vars, sets)
    @test exprs == [:(x = $Inf), :(x = min(x, 7))]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("7 =g= x")[1]], vars, sets)
    @test exprs == [:(x = $Inf), :(x = min(x, 7))]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("7 =l= x")[1]], vars, sets)
    @test exprs == [:(x = $(-Inf)), :(x = max(x, 7))]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("x =l= y")[1]], vars, sets)
    @test exprs == [:(x = $Inf), :(x = min(x, y))]
    exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("y =l= x")[1]], vars, sets)
    @test exprs == [:(y = $Inf), :(y = min(y, x))]

    for (rel, f, g, sent) in (("=g=", :max, :min, -Inf), ("=l=", :min, :max, Inf))
        exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("x + z $rel 0")[1]], vars, sets)
        @test exprs == [:(x = $sent), :(x = $f(x, 0 - z))]
        exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("z + x $rel 0")[1]], vars, sets)
        @test exprs == [:(x = $sent), :(x = $f(x, 0 - z))]
        exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("0 $rel z + x")[1]], vars, sets)
        @test exprs == [:(x = $(-sent)), :(x = $g(x, 0 - z))]
        exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("0 $rel x + z")[1]], vars, sets)
        @test exprs == [:(x = $(-sent)), :(x = $g(x, 0 - z))]
        exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("x - z $rel 0")[1]], vars, sets)
        @test exprs == [:(x = $sent), :(x = $f(x, 0 + z))]
        exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("z - x $rel 0")[1]], vars, sets)
        @test exprs == [:(x = $(-sent)), :(x = $g(x, z - 0))]
        exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("0 $rel z - x")[1]], vars, sets)
        @test exprs == [:(x = $sent), :(x = $f(x, z - 0))]
        exprs = GAMSParse.parseequations!(Expr[], ["e1" => gparse("0 $rel x - z")[1]], vars, sets)
        @test exprs == [:(x = $(-sent)), :(x = $g(x, 0 + z))]
    end
end

error("stop")

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
        modex = parsegams(Module, file)
        mod = eval(modex)
        f = getfield(mod, :f)
        @test isreal(Base.invokelatest(f, inputs[file]))
    end
end

end
