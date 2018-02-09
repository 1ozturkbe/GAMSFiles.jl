module GAMSTest

using GAMSParse
using Base.Test

## Low-level syntax
function slashtest!(dest, str)
    empty!(dest)
    io = seekstart(IOBuffer(str))
    GAMSParse.parse_slashed!(io, dest)
    return dest
end

dest = Dict{String,Vector{String}}()
slashtest!(dest, "name /1*2/")
@test length(dest) == 1 && dest["name"] == ["1*2"]
slashtest!(dest, "name /1*2/;")
@test length(dest) == 1 && dest["name"] == ["1*2"]
slashtest!(dest, "name \"dollars/ounce\" /1*2/")
@test length(dest) == 1 && dest["name"] == ["1*2"]
slashtest!(dest, "name \"dollars per ounce\" /1*2/")
@test length(dest) == 1 && dest["name"] == ["1*2"]
slashtest!(dest, "name /a, b, c/")
@test length(dest) == 1 && dest["name"] == ["a", "b", "c"]
slashtest!(dest, "name /a \"apple\", b \"banana\", c \"carrot\"/")
@test length(dest) == 1 && dest["name"] == ["a", "b", "c"]
slashtest!(dest, "name /a apple, b banana, c carrot/")
@test length(dest) == 1 && dest["name"] == ["a", "b", "c"]
slashtest!(dest, "a /1*2/\n  b   / 1*3 /")
@test length(dest) == 2 && dest["a"] == ["1*2"] && dest["b"] == ["1*3"]
slashtest!(dest, "a /1*2/\n  b   / 1*3 /;")
@test length(dest) == 2 && dest["a"] == ["1*2"] && dest["b"] == ["1*3"]
# Some examples from the manual (chapter on sets)
slashtest!(dest, "cq \"nutrients\" / N, P2O5 / ;")
@test length(dest) == 1 && dest["cq"] == ["N", "P2O5"]
slashtest!(dest, "cq \"nutrients\" / N\n   P2O5 / ;")
@test length(dest) == 1 && dest["cq"] == ["N", "P2O5"]
slashtest!(dest, """
f         "final products"
/yncrude         "refined crude (million barrels)"
lpg                "liquified petroleum gas(million barrels)"
ammonia          "ammonia (million tons)"
coke       "coke (million tons)"
sulfur          "sulfur (million tons)"
/;
""")
@test length(dest) == 1 && dest["f"] == ["yncrude", "lpg", "ammonia", "coke", "sulfur"]
slashtest!(dest, """
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
