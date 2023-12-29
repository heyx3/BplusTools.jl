# Make sure the test is always running in the same directory and within the same project.
using Pkg
cd(joinpath(@__DIR__, ".."))
Pkg.activate(".")

using BplusCore; @using_bplus_core
using BplusApp; @using_bplus_app
using BplusTools; @using_bplus_tools

# Enable asserts for B+ Tools.
BplusTools.ECS.bp_ecs_asserts_enabled() = true
BplusTools.SceneTree.bp_scene_tree_asserts_enabled() = true
BplusTools.Fields.bp_fields_asserts_enabled() = true
BplusTools.bp_helpers_asserts_enabled() = true

# Execute the tests.
const TEST_HEADER_EXTRA = quote
    using JSON3, DataStructures

    # Sadly, the macros to auto-import B+ do not work right in here.
    using BplusCore, BplusApp, BplusTools
    for use in (BplusCore.MODULES_USING_STATEMENTS...,
                BplusApp.MODULES_USING_STATEMENTS...,
                BplusTools.MODULES_USING_STATEMENTS...)
        eval(use)
    end
end
include(BplusCore.TEST_RUNNER_PATH)