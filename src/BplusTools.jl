"Various helpers to make B+ games"
module BplusTools

# Import dependencies.
using Setfield, Dates
using GLFW, DataStructures

# Import B+.
using BplusCore
@using_bplus_core


# Compile sub-modules.
include("Fields/Fields.jl")
export Fields
include("ECS/ECS.jl")
export ECS
include("SceneTree/SceneTree.jl")
export SceneTree

# Compile general helpers that don't deserve their own module.
@make_toggleable_asserts bp_helpers_
include("cam3D.jl")
include("file_cacher.jl")


# Helper macro to import all BplusTools stuff.
const MODULES = tuple(:Fields, :ECS, :SceneTree)
const MODULES_USING_STATEMENTS = [:( using BplusTools.$m ) for m in MODULES]
"Imports all Tools B+ modules"
macro using_bplus_tools()
    return quote $(MODULES_USING_STATEMENTS...) end
end
export @using_bplus_tools


end # module BplusTools
