module GAMSTest

using GAMSParse
using Base.Test

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
              "problem2.24.gms"=>rand(20))

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
