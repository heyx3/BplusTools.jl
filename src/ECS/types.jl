"Some mutable struct representing a bundle of entity data and logic"
abstract type AbstractComponent end

#TODO: Improve the speed of searching for entities in the world entity list, such as a sparse-vector

"
An organized collection of components.

The `World` type is defined afterwards, so it's hidden through a type parameter.
You should refer to this type using the alias `Entity`.
"
mutable struct _Entity{TWorld}
    world::TWorld
    components::Vector{AbstractComponent}
end

"An ordered collection of entities, including accelerated lookups for groups of components"
mutable struct World
    entities::Vector{_Entity{World}}

    delta_seconds::Float32
    elapsed_seconds::Float32
    time_scale::Float32

    # For each Entity, for each Component type, lists all instances.
    component_lookup::Dict{_Entity{World},
                           Dict{Type{<:AbstractComponent},
                                Set{AbstractComponent}}} #TODO: Vector{AbstractComponent} would probably have better performance, and this is only used internally
    # For each component type, lists all entities with that component.
    entity_lookup::Dict{Type{<:AbstractComponent},
                        Set{_Entity{World}}}
    # All component types that exist in this world.
    component_counts::Dict{Type{<:AbstractComponent}, Int}

    # Collections used within specific algorithms.
    buffer_entity_components::Vector{AbstractComponent}
    buffer_ignore_requirements::Set{Type{<:AbstractComponent}}
end

"An organized collection of components"
const Entity = _Entity{World}


export World, Entity, AbstractComponent


function Base.show(io::IO, e::Entity)
    print(io, "Entity(<world>, [")
    for c in e.components
        print(io, " ")
        show(io, c)
    end
    print(io, " ])")
end