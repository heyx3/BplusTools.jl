# An internal interface is used to reference inherited behavior/data from parent components.
#    Metadata:
component_macro_field_exprs(T::Type{<:AbstractComponent}, # *Without* its type params
                            type_params_exprs...
                           ) = error() # iteration of Pair{name Symbol, type expr}
component_macro_has_custom_constructor(::Type{<:AbstractComponent})::Bool = error()
component_macro_types_to_oldest_exprs(::Type{<:AbstractComponent}) = () # Takes a UnionAll type plus its type param expressions.
                                                                        # Returns the type and its parent types, youngest to oldest,
                                                                        #    not including AbstractComponent.
#    Standard events:
component_macro_init(T::Type{<:AbstractComponent}, ::AbstractComponent,
                     entity::Entity, world::World,
                     args...; kw_args...) = error(
    "Arguments do not match in call: ", T, ".CONSTRUCT(",
        join(("::$(typeof(a))" for a in args), ", "),
        " ; ", join(("$n=::$(typeof(v))" for (n, v) in pairs(kw_args)), ", "),
    ")"
)
component_macro_cleanup(    ::Type{<:AbstractComponent}, ::AbstractComponent, ::Bool) = error()
component_macro_tick(       ::Type{<:AbstractComponent}, ::AbstractComponent        ) = error()
component_macro_finish_tick(::Type{<:AbstractComponent}, ::AbstractComponent        ) = error()
#    Promises:
component_macro_all_promises(::Type{<:AbstractComponent}) = () # Symbols
component_macro_unimplemented_promises(::Type{<:AbstractComponent}) = () # Symbols
component_macro_promised_return_type(::Type{<:AbstractComponent}, # The concrete type of the component
                                                                  #   that first declared this @promise
                                     ::Val, # The name of the @promise
                                     # The args and kw_args are needed
                                     #    in case the return type is in terms of them
                                     args...
                                     ; kw_args...
                                    )::Optional{Type} = error()
component_macro_promise_execute(T::Type{<:AbstractComponent}, ::AbstractComponent, v::Val,
                                args...; kw_args...) = error(
    "No parent implementation exists for @promise ", val_type(v), "(...)"
)
#    Configurables:
component_macro_all_configurables(::Type{<:AbstractComponent}) = () # Symbols
component_macro_configurable_return_type(::Type{<:AbstractComponent}, # The concrete type of the component
                                                                      #   that first declared this @promise
                                         ::Val, # The name of the @configurable
                                         # The args and kw_args are needed
                                         #    in case the return type is in terms of them
                                         args...
                                         ; kw_args...
                                        )::Optional{Type} = error()
component_macro_configurable_execute(::Type{<:AbstractComponent},::AbstractComponent, v::Val,
                                     args...; kw_args...) = error(
    "No parent implementation exists for @configurable ", val_type(v), "(...)"
)
#    Utilities:
component_field_print(component, field_name::Val, field_value                   , io) = print(io, field_value)
component_field_print(component, field_name::Val, field_value::AbstractComponent, io) = print(io, "<", typeof(field_value), ">")
component_field_print(component, field_name::Val, field_value::Entity,            io) = print(io,
    if (field_value == component.entity)
        "<self entity>"
    else
        "<other entity>"
    end
)
component_field_print(component, field_name::Val, field_value::World, io) = print(io,
    if field_value == component.world
        "<self world>"
    else
        "<other world>"
    end
)

"To see the printout of new defined components, set this to some output stream"
PRINT_COMPONENT_CODE::Optional{IO} = nothing

"""
A very covenient way to define a component.

The basic syntax is this:

````
@component Name [<: Parent] [attributes...] begin
    field1::Float32
    field2

    # By default, a component is constructed by providing each of its fields, in order.
    # Inherited fields come after the new fields.
    # However, you can override this like so:
    function CONSTRUCT(f)
        # All functions within a @component can reference "this", "entity", and "world".
        this.field1 = Float32(f)
        this.field2 = length(world.entities)
        # You must call SUPER to invoke the parent constructor.
        SUPER(3, f+4)
    end
    function DESTRUCT() # Optionally take a Bool for whether the owning entity is being destroyed
        println("Delta entities: ", length(world.entities) - this.field2)
    end

    function TICK()
        this.field1 += world.delta_seconds
    end
    # FINISH_TICK() exists, but is only useful for abstract components (see below).
````

Attributes are enclosed in braces. Here are the supported ones:
  * `{abstract}`: this component is an abstract type (see below).
  * `{entitySingleton}`: only one of this component can exist on an entity.
  * `{worldSingleton}`: only one of this component can exist in the entire world.
  * `{require: a, b, c}`: other components must exist on this entity, and will be added if they aren't already

This macro also provides an Object-Oriented architecture, where abstract components can add
    fields, behavior, "Promises" (abstract functions) and "Configurables" (virtual functions)
    to their concrete children.
Here is a detailed example of an abstract component:

````
"Some kind of animated motion using float position data. Only one maneuver can run at a time."
@component Maneuver{F<:AbstractFloat} {abstract} {entitySingleton} {require: Position} begin
    pos_component::Position{F}
    duration::F
    progress_normalized::F

    # Because this type has a custom constructor, all child types must have one too.
    # They also must invoke SUPER(...) exactly once, with these arguments.
    function CONSTRUCT(duration_seconds::F)
        this.progress_normalized = 0
        this.duration = duration_seconds
        this.pos_component = get_component(entity, Position)
    end

    # If you want to provide a default instance of this component,
    #    for when it's required but not already existing on an entity,
    #    you can implement this (pretend a child maneuver exists called 'TurningManeuver'):
    DEFAULT() = TurningManeuver(3.5, has_component(entity, PreferLeft) ? -90 : 90)

    # Non-abstract child components must implement this.
    # Abstract child components may choose to implement this for their children.
    # In the latter case, the concrete children can further override the behavior,
    #    and invoke `SUPER()` to get the original parent implementation.
    # If `SUPER()` is called with no arguments,
    #    then the arguments given to the child implementation are automatically forwarded.
    @promise finish_maneuver(last_time_step::F)

    # Child components may choose to override this.
    # It must return a bool.
    # It indicates whether to cut off the maneuver early.
    # When overriding this, you can invoke SUPER() to get the implementation of your parent.
    # If `SUPER()` is called with no arguments,
    #    then the arguments given to the child implementation are automatically forwarded.
    @conigurable should_stop()::Bool = false

    # This abstract base type handles the timing for its children.
    # Base class TICK() is called before children's TICK().
    function TICK()
        this.progress_normalized += world.delta_seconds / this.duration
    end
    # After all tick logic is done (including children), check whether the animation is finished.
    # Base class FINISH_TICK() runs after children's FINISH_TICK().
    function FINISH_TICK()
        if this.should_stop()
            remove_component(entity, this)
        elseif this.progress_normalized >= 1
            # Handle any overshoot of the target, then destroy self:
            overshoot_seconds = (@f32(1) - this.progress_normalized) * this.duration
            this.finish_maneuver(-overshoot_seconds)
            remove_component(entity, this)
        end
    end
end
````

Finally, here is an example of `StrafingManeuver`, a child of `Maneuver` that only uses Float32:

````
@component StrafingManeuver <: Maneuver{Float32} {require: MovementSpeed} begin
    speed_component::MovementSpeed
    dir::v3f
    function CONSTRUCT(duration::Float32, dir::v3f)
        SUPER(duration)
        this.speed_component = get_component(entity, MovementSpeed)
        this.dir = dir
    end

    # Implement the maneuver's interface.
    finish_maneuver(last_time_step::Float32) =
        strafe(this, last_time_step) # Uses a helper function defined below
    should_stop() =
        do_collision_checking(entity) # Some function that checks for this entity colliding with geometry

    TICK() = strafe(this) # Uses a helper function defined below
end

# Helper function for applying the maneuver.
function strafe(s::StrafingManeuver, time_step)
    # In a normal Julia function, things are a bit less convenient.
    entity = s.entity
    world = s.world

    if isnothing(time_step)
        time_step = world.delta_seconds
    end
    s.pos_component.value += s.speed_component.value * time_step * s.dir
end
````
"""
macro component(title, elements...)
    # Parse the component's name and parent type.
    component_type_decl = SplitType(title)
    if isnothing(component_type_decl)
        error("Invalid component name. Must be a type declaration such as 'A{T} <: B'. Got '", title, "'")
    end

    # We need to know about the supertype at compile-time.
    supertype_params = [ ]
    supertype_t = if exists(component_type_decl.parent)
        unionall_type = if @capture(component_type_decl.parent, N_{Ts__})
            append!(supertype_params, Ts)
            N
        else
            component_type_decl.parent
        end
        try
            __module__.eval(unionall_type)
        catch e
            error("Unknown parent type: '", unionall_type, "'")
        end
    else
        nothing
    end

    # Extract the other parts of the declaration.
    attributes = elements[1:(end-1)]
    if length(elements) < 1
        error("No body found for the component '", component_type_decl.name, "'")
    end
    statements = (elements[end]::Expr).args

    return macro_impl_component(component_type_decl, supertype_t, supertype_params,
                                attributes, statements)
end

function macro_impl_component(component_type_decl::SplitType, supertype_t::Optional{Type},
                              supertype_params, attributes, statements)
    if exists(supertype_t) && !(supertype_t <: AbstractComponent)
        error("Component '", component_type_decl.name, "'s parent type '", supertype_t, "' is not a component itself!")
    end

    # Generate some helpers for dealing with the component's type parameters.
    type_param_names::Vector{Symbol} = map(component_type_decl.type_params) do tp
        if tp isa Symbol
            tp
        elseif isexpr(tp, :<:) && (length(tp.args) == 2)
            tp.args[1]
        else
            error("Unexpected type param syntax: '", tp, "'")
        end
    end
    esc_type_param_names = esc.(type_param_names)
    (component_without_type_params, component_with_type_params) =
        if isempty(type_param_names)
            (component_type_decl.name, component_type_decl.name)
        else
            (component_type_decl.name,
             :( $(component_type_decl.name){$(type_param_names...)} ))
        end
    component_broad_type = if isempty(type_param_names)
        component_without_type_params
    else
        :( <:$component_without_type_params )
    end

    # Parse the attributes.
    is_abstract::Bool = false
    is_entity_singleton::Bool = false
    is_world_singleton::Bool = false
    requirements::Vector = [ ]
    for attribute in attributes
        if @capture(attribute, {abstract})
            if is_abstract
                @warn "{abstract} attribute given more than once, which is redundant"
            end
            is_abstract = true
        elseif @capture(attribute, {entitySingleton})
            if is_entity_singleton
                @warn "{entitySingleton} attribute given more than once, which is redundant"
            end
            if is_world_singleton
                @warn "{entitySingleton} is redundant when {worldSingleton} is already given"
            else
                is_entity_singleton = true
            end
        elseif @capture(attribute, {worldSingleton})
            if is_entity_singleton
                @warn "{entitySingleton} is redundant when {worldSingleton} is already given"
                is_entity_singleton = false
            end
            if is_world_singleton
                @warn "{worldSingleton} given more than once, which is redundant"
            else
                is_world_singleton = true
            end
        elseif @capture(attribute, {require:a_, b__})
            requirements = Any[a, b...]
        else
            error("Unexpected attribute: ", attribute)
        end
    end

    # Process the supertype (e.x inheriting attributes).
    supertype_concrete_expr = if isnothing(supertype_t)
        AbstractComponent
    elseif isempty(supertype_params)
        supertype_t
    else
        :( $supertype_t{$(esc.(supertype_params)...)} )
    end
    if exists(supertype_t)
        if is_entitysingleton_component(supertype_t)
            if is_entity_singleton
                @warn "Component $component_without_type_params already inherits {entitySingleton}, so its own attribute is redundant"
            else
                is_entity_singleton = true
            end
        end

        if is_worldsingleton_component(supertype_t)
            if is_world_singleton
                @warn "Component $component_without_type_params already inheritis {worldSingleton}, so its own attribute is redundant"
            else
                is_world_singleton = true
            end
            if is_entity_singleton
                @warn "Component $component_without_type_params has both {entitySingleton} and {worldSingleton} (at least one through inheritance), which is redundant"
            end
        end

        # Append the parent requirements after the child's
        #    (because the child's requirements are probably more specific, to satisfy the parent).
        push!(requirements, :( $(@__MODULE__).require_components($supertype_concrete_expr)... ))
    else
        supertype_t = AbstractComponent
        component_type_decl.parent = AbstractComponent
    end

    # Get the full chain of component inheritance.
    if !isabstracttype(supertype_t)
        error("You may only inherit from abstract components due to Julia's type system! ",
                  supertype_t, " is concrete")
    end
    all_supertypes_youngest_first_exprs = [
        component_with_type_params,
        component_macro_types_to_oldest_exprs(supertype_t, supertype_params...)...
    ]
    all_supertypes_oldest_first_exprs = reverse(all_supertypes_youngest_first_exprs)

    field_data = Vector{Pair{Symbol, Any}}()

    # Parse the declarations.
    constructor::Optional{SplitDef} = nothing
    destructor::Optional{@NamedTuple{b_name::Symbol, body}} = nothing
    defaultor::Optional{Tuple{Any, Any, Any}} = nothing # Concrete component type and its constructor args/kw-args
    tick = nothing # Just the body
    finish_tick = nothing # Just the body
    new_promises = Vector{Tuple{LineNumberNode, SplitDef}}() # Expected to have no body
    new_configurables = Vector{Tuple{LineNumberNode, SplitDef}}() # Expected to have a body
    implemented_promises = Vector{SplitDef}()
    implemented_configurables = Vector{SplitDef}()
    for statement in statements
        # Strip out the doc-string so we can identify the underlying statement.
        # We'll put it back in later.
        doc_string::Optional{AbstractString} = nothing
        if isexpr(statement, :macrocall) && (statement.args[1] == GlobalRef(Core, Symbol("@doc")))
            doc_string = statement.args[3]
            statement = statement.args[4]
        end

        # Figure out what this statement is and respond accordingly.
        if is_function_decl(statement)
            func_data = SplitDef(statement)
            func_data.doc_string = doc_string

            if func_data.name == :CONSTRUCT
                if exists(constructor)
                    error("More than one CONSTRUCT() provided!")
                elseif exists(func_data.return_type) || func_data.generated
                    error("CONSTRUCT() should be a simple function, and with no return type")
                elseif any(arg -> arg.is_splat, @view func_data.args[1:(end-1)])
                        error("CONSTRUCT() has invalid arguments; splat ('a...') must come at the end",
                              " of the ordered parameters")
                end
                constructor = func_data
            elseif func_data.name == :DESTRUCT
                if exists(destructor)
                    error("More than one DESTRUCT() provided!")
                elseif length(func_data.args) > 1
                    error("DESTRUCT() should have at most one argument ('entity_is_dying::Bool').",
                          " Got: '", combinecall(func_data), "'")
                elseif (length(func_data.args) == 1) && let a = func_data.args[1]
                                                              !isa(a.name, Symbol) ||
                                                              a.is_splat ||
                                                              exists(a.default_value)
                                                         end
                    error("The single parameter to DESTRUCT should be something like ",
                          "'is_entity_dying::Bool'. Got: '", func_data.args[1], "'")
                elseif exists(func_data.return_type) || func_data.generated || !isempty(func_data.where_params)
                    error("DESTRUCT() should be a simple function, and with no return type")
                end
                destructor = (
                    b_name=isempty(func_data.args) ?
                             Symbol("IGNORED:entity_is_dying") :
                             func_data.args[1].name,
                    body=func_data.body
                )
            elseif func_data.name == :DEFAULT
                if exists(defaultor)
                    error("DEFAULT() provided more than once")
                elseif !isempty(func_data.args) || !isempty(func_data.kw_args)
                    error("DEFAULT() should have no arguments")
                elseif !isempty(func_data.where_params) || func_data.generated
                    error("DEFAULT() should be a simple function, no type params or @generated")
                end
                if @capture(func_data.body, (concreteName_(args__; kw_args__)) | (concreteName_(args__)))
                    defaultor = (concreteName, args, isnothing(kw_args) ? [ ] : kw_args)
                else
                    error("DEFAULT() clause should be of the form 'ChildComponent(args...; kw_args...)'")
                end
            elseif func_data.name == :TICK
                if exists(tick)
                    error("TICK() provided more than once")
                elseif !isempty(func_data.args) || !isempty(func_data.kw_args)
                    error("TICK() should have no arguments")
                elseif !isempty(func_data.where_params) || func_data.generated
                    error("TICK() should be a simple function, no type params or @generated")
                end
                tick = func_data.body
            elseif func_data.name == :FINISH_TICK
                if exists(finish_tick)
                    error("FINISH_TICK() provided more than once")
                elseif !isempty(func_data.args) || !isempty(func_data.kw_args)
                    error("FINISH_TICK() should have no arguments")
                elseif !isempty(func_data.where_params) || func_data.generated
                    error("FINISH_TICK() should be a simple function, no type params or @generated")
                end
                finish_tick = func_data.body
            elseif func_data.name in component_macro_unimplemented_promises(supertype_t)
                @bp_check(!func_data.generated, "@generated @promise is currently unsupported")
                push!(implemented_promises, func_data)
            elseif func_data.name in component_macro_all_configurables(supertype_t)
                @bp_check(!func_data.generated, "@generated @configurable is currently unsupported")
                push!(implemented_configurables, func_data)
            else
                error("Unexpected function in component: '", func_data.name, "'")
            end
        elseif @capture(statement, fieldName_Symbol) || @capture(statement, fieldName_::fieldType_)
            if fieldName in (:entity, :world)
                error("Can't name a component field 'entity', or 'world'")
            end
            push!(field_data, fieldName => (isnothing(fieldType) ? Any : fieldType))
        elseif is_macro_invocation(statement)
            macro_data = SplitMacro(statement)
            if macro_data.name == Symbol("@promise")
                if !is_abstract
                    error("Only {abstract} components can @promise things")
                elseif length(macro_data.args) != 1
                    error("A @promise should include a single function signature. ",
                            "Got ", length(macro_data.args), " expressions instead")
                elseif !is_function_call(macro_data.args[1])
                    error("A @promise should be a function signature (NOT a definition). Got: ",
                              macro_data.args[1])
                end

                func_data = SplitDef(:( $(macro_data.args[1]) = nothing ))
                func_data.doc_string = doc_string
                if func_data.name in (component_macro_unimplemented_promises(supertype_t)...,
                                      component_macro_all_configurables(supertype_t)...)
                    error("@promise hides inherited @promise or @configurable: '", func_data.name, "'")
                end

                push!(new_promises, (
                    macro_data.source,
                    func_data
                ))
            elseif macro_data.name == Symbol("@configurable")
                if length(macro_data.args) != 1
                    error("A @configurable should include a single function definition. ",
                            "Got ", length(macro_data.args), " expressions instead")
                elseif !is_function_decl(macro_data.args[1])
                    error("A @configurable should be a function declaration. Got: ",
                            macro_data.args[1])
                end

                func_data = SplitDef(macro_data.args[1])
                func_data.doc_string = doc_string
                if func_data.name in (component_macro_unimplemented_promises(supertype_t)...,
                                      component_macro_all_configurables(supertype_t)...)
                    error("@configurable hides inherited @promise or @configurable: '", func_data.name, "'")
                end

                push!(new_configurables, (
                    macro_data.source,
                    func_data
                ))
                push!(implemented_configurables, func_data)
            else
                error("Unexpected macro in component '$component_without_type_params': $statement")
            end
        elseif statement isa LineNumberNode
            # Ignore it.
        else
            error("Unexpected syntax in body of component '$component_without_type_params': \"$statement\"")
        end
    end

    # Take fields from the parent, after the new ones.
    if supertype_t != AbstractComponent
        append!(field_data, component_macro_field_exprs(supertype_t, supertype_params...))
    end

    # Post-process the statement data.
    if isnothing(destructor)
        destructor = (
            b_name=Symbol("UNUSED: entity is dying?"),
            body=nothing
        )
    end

    # Process promise data for code generation.
    all_promise_names::Vector{Symbol} = [
        component_macro_all_promises(supertype_t)...,
        (tup[2].name for tup in new_promises)...
    ]
    inherited_unimplemented_promise_names = filter(
        p_name -> none(p -> p.name == p_name, implemented_promises),
        component_macro_unimplemented_promises(supertype_t)
    )
    if !isempty(inherited_unimplemented_promise_names) && !is_abstract
        error("Component '$component_without_type_params' doesn't follow through on some promises: [ ",
                  join(inherited_unimplemented_promise_names, ", "), "]")
    end

    # Process configurable data for code generation.
    all_configurable_names::Vector{Symbol} = [
        component_macro_all_configurables(supertype_t)...,
        (tup[2].name for tup in new_configurables)...
    ]

    # Look for duplicate property names: fields, promises, and configurables.
    property_name_counts = Dict{Symbol, Int}()
    for name in Iterators.flatten(((name for (name, _) in field_data),
                                   all_configurable_names,
                                   all_promise_names))
        if !haskey(property_name_counts, name)
            property_name_counts[name] = 0
        end
        property_name_counts[name] += 1
    end
    duplicate_property_names = [name for (name, count) in property_name_counts if (count > 2)]
    if !isempty(duplicate_property_names)
        error("Name collisions between fields, promises, and configurables: ",
                join(duplicate_property_names, ", "))
    end

    # Generate the type definition.
    type_decl = if is_abstract
        :( Core.@__doc__(abstract type $(esc(combinetype(component_type_decl))) end) )
    else
        :( Core.@__doc__(mutable struct $(esc(combinetype(component_type_decl)))
                entity::Entity
                world::World

                $(( esc(:( $f::$t )) for (f, t) in field_data )...)

                # Constructor needs to include type params only if applicable.
                $(begin
                    name = if isempty(type_param_names)
                        esc(component_without_type_params)
                    else
                        :( $(esc(component_without_type_params)){$(type_param_names...)} )
                    end

                    signature = if isempty(type_param_names)
                        :( $name(entity, world) )
                    else
                        :( $name(entity, world) where {$(type_param_names...)} )
                    end

                    new_call = if isempty(type_param_names)
                        :( new() )
                    else
                        :( new{$(type_param_names...)}() )
                    end

                    :( $signature = begin
                        this = $new_call
                        this.entity = entity
                        this.world = world
                        return this
                    end )
                end)
        end) )
    end

    global_decls = [ ]

    # Implement the internal macro interface.
    # Note that any user code may access the component's type parameters,
    #    so almost all methods must explicitly name them.
    function emit_macro_interface_impl(func_name::Symbol,
                                       body
                                       ;
                                       extra_args = (),
                                       extra_kw_args = (),
                                       return_type = nothing,
                                       extra_where_params = [], # Enumeration of SplitType and/or expressions
                                       # First param will be the component's Type.
                                       # If you wish, the component's type params can be included,
                                       #    either as normal type params or as expr arguments.
                                       typing_mode::@ano_enum(
                                           no_type_params,
                                           compiled_type_params, 
                                           runtime_type_param_exprs,
                                       ) = @ano_value(compiled_type_params),
                                       # The component Type param can be specific or broad (using `<:`).
                                       broaden_component_type::Bool = false,
                                       force_inline::Bool = false)
        # Generate the expression for the component Type.
        component_t = if typing_mode isa @ano_enum(no_type_params, runtime_type_param_exprs)
            esc(component_without_type_params)
        elseif typing_mode isa @ano_enum(compiled_type_params)
            esc(component_with_type_params)
        else
            error("Unhandled: ", typing_mode)
        end
        component_t = if broaden_component_type
            :( Type{<:$component_t} )
        else
            :( Type{$component_t} )
        end

        # If using runtime type params, insert them at the beginning of the argument list.
        args = extra_args
        if typing_mode isa @ano_enum(runtime_type_param_exprs)
            args = tuple(
                esc.(type_param_names)...,
                args...
            )
        end

        # Generate compile-time type params for the function.
        extra_where_params = map(extra_where_params) do t
            if t isa SplitType
                t
            else
                SplitType(t)
            end
        end
        compile_time_type_params = if typing_mode isa @ano_enum(compiled_type_params)
            esc.(tuple(
                type_param_names...,
                extra_where_params...
            ))
        else
            esc.(extra_where_params)
        end

        # Assemble the function AST.
        func_def = SplitDef(:(
            $(@__MODULE__).$func_name(::$component_t,
                                      $(combinearg.(SplitArg.(args))...)
                                      ;
                                      $(combinearg.(SplitArg.(extra_kw_args))...)
                                     ) where {$(compile_time_type_params...)} = $body
        ))

        # Inject data as requested.
        if exists(return_type)
            func_def.return_type = esc(return_type)
        end
        if force_inline
            func_def.inline = true
        end

        push!(global_decls, combinedef(func_def))
    end
    emit_macro_interface_impl(:component_macro_field_exprs,
        :( tuple(
            $((:( $(QuoteNode(a)) => $(esc(b)) )
                for (a,b) in field_data)...)
        ) ),
        typing_mode = @ano_value(runtime_type_param_exprs)
    )
    emit_macro_interface_impl(:component_macro_has_custom_constructor,
        exists(constructor),
        typing_mode = @ano_value(no_type_params),
        broaden_component_type = true
    )
    emit_macro_interface_impl(:component_macro_types_to_oldest_exprs,
        :( tuple(
            $(esc.(all_supertypes_youngest_first_exprs)...)
        ) ),
        typing_mode = @ano_value(runtime_type_param_exprs)
    )
    emit_macro_interface_impl(:component_macro_init,
        # If using an auto-generated constructor,
        #    the body should set each field to its corresponding argument.
        if isnothing(constructor)
            quote
                $(map(field_data) do (name, type)
                    return esc(:( this.$name = convert($type, $name) ))
                end...)

                # Note that all fields, including inherited ones, are handled by the above loop.
                # No need to call into a parent constructor.
                return nothing
            end
        # If using an explicit constructor, the body should set up and then invoke the user's code.
        else
            quote
                # If there is a parent component type, offer SUPER()
                #    and and count how many times it's invoked.
                if $(supertype_t != AbstractComponent)
                    n_super_calls::Int = 0
                    @inline $(esc(:SUPER))(args...; kw_args...) = begin
                        n_super_calls += 1
                        $(@__MODULE__).component_macro_init($supertype_t,
                                                            $(esc(:this)), $(esc(:entity)), $(esc(:world)),
                                                            args...; kw_args...)
                    end
                end

                # Nest the body in a lambda in case it has an explicit return statement.
                (() -> $(esc(constructor.body)))()

                # Make sure SUPER was called exactly once, if it was available.
                if $(supertype_t != AbstractComponent) && (n_super_calls != 1)
                    error("Your custom constructor for '", $(string(component_without_type_params)),
                            "' should call SUPER exactly once, but it called it ",
                            n_super_calls, " times")
                end

                return nothing
            end
        end,
        extra_args = (
            :( $(esc(:this))::$(esc(component_with_type_params)) ),

            # 'entity' and 'world' are needed so that the user's parameters can reference them
            #    as part of their default values.
            :( $(esc(:entity))::$Entity ),
            :( $(esc(:world))::World ),

            # If using an auto-generated constructor, add one arg for each field.
            (if isnothing(constructor)
                (esc(name) for (name, type) in field_data)
            # If using an explicit constructor, take parameters from the constructor definition.
            # Escape each argument's entire declaration.
            else
                map(constructor.args) do ca
                    ca2 = SplitArg(ca)
                    ca2.is_escaped = true
                    combinearg(ca2)
                end
            end)...
        ),
        extra_kw_args = 
            # If using an explicit constructor, take named parameters from the constructor definition.
            # Escape each argument's entire declaration.
            if exists(constructor)
                map(constructor.kw_args) do ca
                    ca2 = SplitArg(ca)
                    ca2.is_escaped = true
                    combinearg(ca2)
                end
            else
                ()
            end,
        extra_where_params = if exists(constructor)
            constructor.where_params
        else
            []
        end
    )
    emit_macro_interface_impl(:component_macro_cleanup,
        quote
            $(esc(:entity))::Entity = $(esc(:this)).entity
            $(esc(:world))::World = $(esc(:this)).world

            # Nest the body in a lambda in case it has an explicit return statement.
            (() -> $(esc(destructor.body)))()

            return nothing
        end,
        extra_args = (
            :( $(esc(:this))::$(esc(component_with_type_params)) ),
            :( $(esc(destructor.b_name))::Bool )
        )
    )
    emit_macro_interface_impl(:component_macro_tick,
        quote
            $(esc(:entity))::Entity = $(esc(:this)).entity
            $(esc(:world))::World = $(esc(:this)).world

            # Nest the body in a lambda in case it has an explicit return statement.
            (() -> $(esc(tick)))()

            return nothing
        end,
        extra_args = tuple(
            :( $(esc(:this))::$(esc(component_with_type_params)) )
        )
    )
    emit_macro_interface_impl(:component_macro_finish_tick,
        quote
            $(esc(:entity))::Entity = $(esc(:this)).entity
            $(esc(:world))::World = $(esc(:this)).world

            # Nest the body in a lambda in case it has an explicit return statement.
            (() -> $(esc(finish_tick)))()

            return nothing
        end,
        extra_args = tuple(
            :( $(esc(:this))::$(esc(component_with_type_params)) )
        )
    )
    # Promises:
    emit_macro_interface_impl(:component_macro_all_promises, Tuple(all_promise_names),
                              typing_mode = @ano_value(no_type_params),
                              broaden_component_type = true)
    emit_macro_interface_impl(:component_macro_unimplemented_promises,
        tuple(
            inherited_unimplemented_promise_names...,
            (p[2].name for p in new_promises)...
        ),
        typing_mode = @ano_value(no_type_params),
        broaden_component_type = true
    )
    for (source, def) in new_promises; emit_macro_interface_impl(:component_macro_promised_return_type,
        esc(def.return_type),
        extra_args = tuple(
            :( ::Val{$(QuoteNode(def.name))} ),
            esc.(combine_expr.(def.args))...
        ),
        extra_kw_args = tuple(
            esc.(combine_expr.(def.kw_args))...
        ),
        extra_where_params = def.where_params,
        broaden_component_type = true,
        force_inline = true # Really want the return value to be a compile-time constant when possible
    ) end
    for def in implemented_promises # :component_macro_promise_execute
        def = SplitDef(def)

        # Cache user definitions.
        promise_name = def.name
        name_quote = QuoteNode(promise_name)
        name_str = string(promise_name)
        promise_args::Vector{SplitArg} = SplitArg.(def.args)
        promise_kw_args::Vector{SplitArg} = SplitArg.(def.kw_args)

        # Wrap the user definitions in our internal macro interface.
        def.name = :( $(@__MODULE__).component_macro_promise_execute )
        if exists(def.return_type)
            def.return_type = esc(def.return_type)
        end
        for a in def.args
            a.is_escaped = true
        end
        for kw in def.kw_args
            kw.is_escaped = true
        end
        for t in def.where_params
            t.is_escaped = true
        end
        insert!(def.args, 1, SplitArg(:( T::Type{<:$(esc(component_with_type_params))} )))
        insert!(def.args, 2, SplitArg(:( $(esc(:this))::$(esc(component_with_type_params)) )))
        insert!(def.args, 3, SplitArg(:( ::Val{$name_quote} )))
        def.where_params = [
            SplitType.(esc.(type_param_names))...
            def.where_params...
        ]
        def.body = esc(def.body)

        # Wrap the body in a lambda to stop 'return' statements
        #    from skipping over our internal logic.
        def.body = :( (() -> $(def.body))() )

        # Provide access to 'entity' and 'world'.
        def.body = quote
            $(esc(:entity)) = $(esc(:this)).entity
            $(esc(:world)) = $(esc(:this)).world
            $(def.body)
        end

        # Provide access to SUPER(), which invokes the supertype's implementation.
        def.body = quote
            # The 0-argument SUPER() executes the parent's implementation with the same parameters.
            $(combine_expr(SplitDef(
                esc(:SUPER), [ ], [ ],
                :( $(@__MODULE__).component_macro_promise_execute(
                    $supertype_concrete_expr, $(esc(:this)), Val($name_quote),
                    $((esc(a.name) for a in promise_args)...)
                    ; $((if a.is_splat
                             :( $(esc(a.name))... )
                         else
                             :( $(esc(a.name)) = $(esc(a.name)) )
                         end for a in promise_kw_args)...)
                ) ),
                nothing, [ ],
                nothing, false, false, false
            )))
            # The user may also pass different arguments when invoking the parent's implementation.
            $(if length(promise_args) + length(promise_kw_args) > 0
                combine_expr(SplitDef(
                    esc(:SUPER),
                    # Un-escaped versions of the argument names
                    #    won't conflict with the original outer ones,
                    #    which are escaped.
                    [ SplitArg(:( args... )) ],
                    [ SplitArg(:( kw_args... )) ],
                    :( $(@__MODULE__).component_macro_promise_execute(
                        $supertype_concrete_expr, $(esc(:this)), Val($name_quote),
                        args...; kw_args...
                    ) ),
                    nothing, [ ],
                    nothing, false, false, false
                ))
            else
                :( )
            end)
            # Now execute the child implementation with access to SUPER() defined above.
            $(def.body)
        end

        # If the original promise specified a return type, explicitly check for that.
        # The return type promised by this child implementation will get checked by Julia on return.
        return_type_call = SplitDef(:(
            $(@__MODULE__).component_macro_promised_return_type($(esc(component_with_type_params)),
                                                                Val($name_quote))
        ))
        append!(return_type_call.args,
                SplitArg(esc(a.name)) for a in promise_args)
        append!(return_type_call.kw_args,
                SplitArg(if a.is_splat
                             :( $(esc(a.name))... )
                         else
                             :( $(esc(a.name)) = $(esc(a.name)) )
                        end) for a in promise_kw_args)
        return_type_call_expr = combine_expr(return_type_call)
        def.body = quote
            result = $(def.body)
            let T = $return_type_call_expr
                if isnothing(T) || (result isa T)
                    return result
                elseif T == Nothing
                    return nothing
                else
                    error("@promise ", $(name_str), " should return ",
                            T, ", but returned ", typeof(result))
                end
            end
        end

        push!(global_decls, combine_expr(def))
    end
    # Configurables:
    emit_macro_interface_impl(:component_macro_all_configurables, Tuple(all_configurable_names),
                              typing_mode = @ano_value(no_type_params),
                              broaden_component_type = true)
    for (source, def) in new_configurables; emit_macro_interface_impl(:component_macro_configurable_return_type,
        esc(def.return_type),
        extra_args = tuple(
            :( ::Val{$(QuoteNode(def.name))} ),
            esc.(combine_expr.(def.args))...
        ),
        extra_kw_args = tuple(
            esc.(combine_expr.(def.kw_args))...
        ),
        extra_where_params = def.where_params,
        broaden_component_type = true,
        force_inline = true # Really want the return value to be a compile-time constant when possible
    ) end
    for def in implemented_configurables # :component_macro_configurable_execute
        def = SplitDef(def)

        # Cache user definitions.
        configurable_name = def.name
        name_quote = QuoteNode(configurable_name)
        name_str = string(configurable_name)
        configurable_args::Vector{SplitArg} = SplitArg.(def.args)
        configurable_kw_args::Vector{SplitArg} = SplitArg.(def.kw_args)

        # Wrap the user definitions in our internal macro interface.
        def.name = :( $(@__MODULE__).component_macro_configurable_execute )
        if exists(def.return_type)
            def.return_type = esc(def.return_type)
        end
        for a in def.args
            a.is_escaped = true
        end
        for kw in def.kw_args
            kw.is_escaped = true
        end
        for t in def.where_params
            t.is_escaped = true
        end
        insert!(def.args, 1, SplitArg(:( T::Type{<:$(esc(component_with_type_params))} )))
        insert!(def.args, 2, SplitArg(:( $(esc(:this))::$(esc(component_with_type_params)) )))
        insert!(def.args, 3, SplitArg(:( ::Val{$name_quote} )))
        def.where_params = [
            SplitType.(esc.(type_param_names))...
            def.where_params...
        ]
        def.body = esc(def.body)

        # Wrap the body in a lambda to stop 'return' statements
        #    from skipping over our internal logic.
        def.body = :( (() -> $(def.body))() )

        # Provide access to 'entity' and 'world'.
        def.body = quote
            $(esc(:entity)) = $(esc(:this)).entity
            $(esc(:world)) = $(esc(:this)).world
            $(def.body)
        end

        # Provide access to SUPER(), which invokes the supertype's implementation.
        def.body = quote
            # The 0-argument SUPER() executes the parent's implementation with the same parameters.
            $(combine_expr(SplitDef(
                esc(:SUPER), [ ], [ ],
                :( $(@__MODULE__).component_macro_configurable_execute(
                    $supertype_concrete_expr, $(esc(:this)), Val($name_quote),
                    $((esc(a.name) for a in configurable_args)...)
                    ; $((if a.is_splat
                             :( $(esc(a.name))... )
                         else
                             :( $(esc(a.name)) = $(esc(a.name)) )
                         end for a in configurable_kw_args)...)
                ) ),
                nothing, [ ],
                nothing, false, false, false
            )))
            # The user may also pass different arguments when invoking the parent's implementation.
            $(if length(configurable_args) + length(configurable_kw_args) > 0
                combine_expr(SplitDef(
                    esc(:SUPER),
                    # Un-escaped versions of the argument names
                    #    won't conflict with the original outer ones,
                    #    which are escaped.
                    [ SplitArg(:( args... )) ],
                    [ SplitArg(:( kw_args... )) ],
                    :( $(@__MODULE__).component_macro_configurable_execute(
                        $supertype_concrete_expr, $(esc(:this)), Val($name_quote),
                        args...; kw_args...
                    ) ),
                    nothing, [ ],
                    nothing, false, false, false
                ))
            else
                :( )
            end)
            # Now execute the child implementation with access to SUPER() defined above.
            $(def.body)
        end

        # If the original configurable specified a return type, explicitly check for that.
        # The return type given by this child implementation will get checked by Julia on return.
        return_type_call = SplitDef(:(
            $(@__MODULE__).component_macro_configurable_return_type($(esc(component_with_type_params)),
                                                                    Val($name_quote))
        ))
        append!(return_type_call.args,
                SplitArg(esc(a.name)) for a in configurable_args)
        append!(return_type_call.kw_args,
                SplitArg(if a.is_splat
                             :( $(esc(a.name))... )
                         else
                             :( $(esc(a.name)) = $(esc(a.name)) )
                        end) for a in configurable_kw_args)
        return_type_call_expr = combine_expr(return_type_call)
        def.body = quote
            result = $(def.body)
            let T = $return_type_call_expr
                if isnothing(T) || (result isa T)
                    return result
                elseif T === Nothing
                    return nothing
                else
                    error("@configurable ", $(name_str), " should return ",
                            T, ", but returned ", typeof(result))
                end
            end
        end

        push!(global_decls, combine_expr(def))
    end

    # Implement promises and configurables as properties.
    push!(global_decls, quote
        @inline $Base.propertynames(::$(esc(component_without_type_params))) = tuple(
            :world, :entity,
            $((QuoteNode(n) for (n, T) in field_data)...),
            $((QuoteNode(p) for p in all_promise_names)...),
            $((QuoteNode(c) for c in all_configurable_names)...)
        )

        @inline $Base.getproperty(c::$(esc(component_without_type_params)), name::Symbol) = getproperty(c, Val(name))
        @inline $Base.getproperty(c::$(esc(component_without_type_params)), ::Val{:entity}) = getfield(c, :entity)
        @inline $Base.getproperty(c::$(esc(component_without_type_params)), ::Val{:world}) = getfield(c, :world)

        $(map(field_data) do (name, type); :(
            @inline $Base.getproperty(c::$(esc(component_without_type_params)),
                                      ::Val{$(QuoteNode(name))}
                                     ) =
                   getfield(c, $(QuoteNode(name)))
        ) end...)

        $(map(implemented_promises) do promise
            quoted = QuoteNode(promise.name)
            return :(
                @inline $Base.getproperty(c::$(esc(component_without_type_params)), ::Val{$quoted}) =
                    @inline (args...; kw_args...) -> begin
                        $(@__MODULE__).component_macro_promise_execute(
                            typeof(c), c, Val($quoted),
                            args...; kw_args...
                        )
                    end
            )
        end...)

        $(map(implemented_configurables) do def
            quoted = QuoteNode(def.name)
            return :(
                @inline $Base.getproperty(c::$(esc(component_without_type_params)), ::Val{$quoted}) =
                    @inline (args...; kw_args...) -> begin
                        $(@__MODULE__).component_macro_configurable_execute(
                            typeof(c), c, Val($quoted),
                            args...; kw_args...
                        )
                    end
            )
        end...)
    end)

    # Implement the external interface.
    push!(global_decls, quote
        $(@__MODULE__).is_entitysingleton_component(::Type{$(esc(component_broad_type))})::Bool = $is_entity_singleton
        $(@__MODULE__).is_worldsingleton_component(::Type{$(esc(component_broad_type))})::Bool = $is_world_singleton
        # For methods that might want type params,
        #    conditionally add them (adding an empty {} statement confuses the parser).
        $(let inner_def = :( $(@__MODULE__).require_components(::Type{$(esc(component_with_type_params))}
                                                              ) =
                                 tuple($(esc.(requirements)...)))
            sd = SplitDef(inner_def)
            sd.where_params = SplitType.(esc_type_param_names)
            combinedef(sd)
        end)
        $(let inner_def = :( $(@__MODULE__).create_component(::Type{$(esc(component_with_type_params))},
                                                             entity::$Entity,
                                                             args...; kw_args...
                                                            ) )
            sd = SplitDef(inner_def)
            sd.where_params = SplitType.(esc_type_param_names)
            sd.body = quote
                if $is_abstract
                    $(if isnothing(defaultor)
                          :( error("Can't create a '", $(string(component_with_type_params)),
                                     "'; it's abstract and no DEFAULT() was provided") )
                      else
                          :( return $(@__MODULE__).create_component(
                            $(esc(defaultor[1])), entity,
                            $(esc.(defaultor[2])...)
                            ; $(esc.(defaultor[3])...)
                          ) )
                      end)
                else
                    this = $(esc(component_with_type_params))(entity, entity.world)
                    $(@__MODULE__).component_macro_init(typeof(this),
                                                        this, entity, entity.world,
                                                        args...; kw_args...)
                    return this
                end
            end
            combinedef(sd)
        end )
        $(let inner_def = :( $(@__MODULE__).destroy_component(c::$(esc(component_with_type_params)),
                                                              e::$Entity,
                                                              is_entity_dying::Bool)
                           )
            sd = SplitDef(inner_def)
            sd.where_params = SplitType.(esc_type_param_names)
            sd.body = quote
                @bp_ecs_assert(c.entity == e, "Given the wrong entity")
                $(map(all_supertypes_youngest_first_exprs) do T
                    :( $(@__MODULE__).component_macro_cleanup($(esc(T)), c, is_entity_dying) )
                end...)
                return nothing
            end
            combinedef(sd)
        end )
        $(let inner_def = :( $(@__MODULE__).tick_component(c::$(esc(component_with_type_params)),
                                                           e::$Entity)
                           )
            sd = SplitDef(inner_def)
            sd.where_params = SplitType.(esc_type_param_names)
            sd.body = quote
                @bp_ecs_assert(c.entity == e, "Given the wrong entity")
                $(map(all_supertypes_oldest_first_exprs) do T
                    :( $(@__MODULE__).component_macro_tick($(esc(T)), c) )
                end...)
                $(map(all_supertypes_youngest_first_exprs) do T
                    :( $(@__MODULE__).component_macro_finish_tick($(esc(T)), c) )
                end...)
                return nothing
            end
            combinedef(sd)
        end)
    end)

    # Add some other useful stuff.
    if !is_abstract; push!(global_decls, :(
        function $Base.show(io::IO, c::$(esc(component_without_type_params)))
            print(io, typeof(c), "(<entity>, <world>")
            $(map(field_data) do (name, declared_type); quote
                print(io, ", ")
                $(@__MODULE__).component_field_print(
                    c,
                    Val($(QuoteNode(name))), getfield(c, $(QuoteNode(name))),
                    io
                )
            end end...)
            print(io, ")")
        end
    )) end

    final_expr = quote
        $type_decl
        $(global_decls...)
    end
    if exists(PRINT_COMPONENT_CODE)
        print(PRINT_COMPONENT_CODE,
              "\nCOMPONENT ", combinetype(component_type_decl),
              "\n", MacroTools.prettify(final_expr),
              "\n\n")
    end
    return final_expr
end
