# Functions to work with a field's tree of expressions, generically.

"Counts the number of inner fields this field has"
field_input_count(f::AbstractField)::Int = error("Unimplemented: ", typeof(f))
"Gets one of the inner fields (as counted by `field_input_count()`)"
field_input_get(f::AbstractField, i::Integer)::AbstractField = error("Unimplemented: ", typeof(f), ", ", typeof(i))
"Returns a copy of this field, with the given input field replaced"
field_input_set(f::AbstractField, i::Integer, value::AbstractField)::AbstractField = error("Unimplemented: ", typeof(f), ", ", typeof(i), ", ", typeof(value))
export field_input_count, field_input_get, field_input_set

"
Retrieves a field within the given tree of field expressions,
    using an enumeration of child indices through the field expression tree.

Note that this is type-unstable!
"
function field_input_get(f::AbstractField, indices)::AbstractField
    @nospecialize f
    for i in indices
        f = field_input_get(f, i)
    end
    return f
end
"
Returns a copy of this field, with one of its inner fields replaced,
    using an enumeration of child indices to pinpoint the field to replace within the expression tree.

Note that this is type-unstable!
"
function field_input_set(f::AbstractField, indices, v::AbstractField)::AbstractField
    @nospecialize f v
    if isempty(indices)
        return v
    else
        old_child = field_input_get(f, first(indices))
        new_child = field_input_set(old_child, @view(indices[2:end]), v)
        return field_input_set(f, first(indices), new_child)
    end
end


"
Visits each abstract field starting at some outer field, depth-first.
Invokes your visitor function, providing:

  * The field itself (usually a copy, as fields are supposed to be immutable types)
  * A *temporary* list of the path to this field, as a sequence of indices traversing the field tree.

For optimized memory use, you can provide a pre-allocated buffer.

Note that this function is type-unstable!
It's recommended to make your lambda' first parameter `@nospecialize` to reduce JIT overhead.
"
function field_visit_depth_first(visitor, root::AbstractField, buffer::Vector{Int} = [ ])
    @nospecialize root

    function recursion(f::AbstractField)
        @nospecialize f
        visitor(f)
        for i in 1:field_input_count(f)::Int
            push!(buffer, i)
            recursion(field_input_get(f, i)::AbstractField)
            pop!(buffer)
        end
    end

    empty!(buffer)
    recursion(root)
end

#TODO: postwalk/pre-walk equivalents
#TODO: Breadth-first version