using Pkg
Pkg.activate(@__DIR__)

using BplusTools; @using_bplus_tools

@component Ca {abstract} begin
    i::Int
    @configurable modify_i() = (this.i *= 5)
    @promise stringify_self()::String
    TICK() = (this.i += 1)
end
@component Cb <: Ca begin
    stringify_self() = "CbbbbbBB"
    TICK() = (this.i += 1)
end

function run_world(n_ticks::Int)
    w = World()
    e = add_entity(w)
    c = add_component(e, Cb, 4)

    for i in 1:n_ticks
        tick_world(w, 0.1f0)
    end
end
run_world(100)


using Profile
Profile.start_timer()
@component Da {abstract} begin
    i::Int
    @configurable modify_i() = (this.i *= 5)
    @promise stringify_self()::String
    TICK() = (this.i += 1)
end
@component Db <: Da begin
    stringify_self() = "DbbbbbBB"
    TICK() = (SUPER(); this.i += 1)
end
Profile.stop_timer()
const prof_text_path = joinpath(@__DIR__, "ProfileResult.txt")
open(prof_text_path, "w") do io::IO
    ctx = IOContext(io, :displaysize=>(5000, 999999))

    println(ctx, "========================================\n==    Flat")
    Profile.print(ctx, format=:flat, mincount=100)

    println(ctx, "\n\n\n")

    println(ctx, "========================================\n==    Tree")
    Profile.print(ctx, format=:tree, noisefloor=2.0)
end
if Sys.iswindows()
    run(`cmd /C $(abspath(prof_text_path))`)
else
    run(`$(abspath(prof_text_path))`)
end