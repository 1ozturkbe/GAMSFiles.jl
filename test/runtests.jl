module GAMSTest

using GAMSParse
using Base.Test

inputs = Dict("beale.gms"=>rand(2),
              "convex4_10_1.gms"=>rand(10),
              "problem2.1.gms"=>rand(2),
              "yfit.gms"=>rand(3),
              "steiner_vareps.gms"=>rand(17),
              "tointqor.gms"=>rand(50))

cd(joinpath(@__DIR__, "gams")) do
    for file in readdir()
        modex = parsegams(Module, file)
        mod = eval(modex)
        f = getfield(mod, :f)
        @test isfinite(Base.invokelatest(f, inputs[file]))
    end
end

end
