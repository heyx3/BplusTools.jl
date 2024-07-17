"Fills an array by sampling the given field."
function sample_field!( array::AbstractArray{Vec{NOut, F}, NIn},
                        field::TField
                        ;
                        use_threading::Bool = true,
                        array_bounds::Box{NIn, UInt} = Box(
                            min = one(Vec{NIn, UInt}),
                            size = convert(Vec{NIn, UInt}, vsize(array))
                        ),
                        sample_space::Box{NIn, F} = Box(
                            min = zero(Vec{NIn, F}),
                            max = one(Vec{NIn, F})
                        ),
                        threaded_1D_slice_size::Int = 1024
                      ) where {NIn, NOut, F, TField<:AbstractField{NIn, NOut, F}}
    prep_data = prepare_field(field)

    # Calculate field positions.
    HALF = F(1) / F(2)
    @inline grid_pos_to_sample_pos(pos_component::Integer, axis::Int) = lerp(
        min_inclusive(sample_space)[axis],
        max_exclusive(sample_space)[axis],
        inv_lerp(min_inclusive(array_bounds)[axis],
                 max_inclusive(array_bounds)[axis],
                 pos_component + HALF)
    )

    # Split the array into conceptual slices so different threads can handle different slices.
    b_min = min_inclusive(array_bounds)
    b_max = max_inclusive(array_bounds)
    b_size = size(array_bounds)
    b_length = prod(b_size.data)
    local n_slices::Int
    local get_slice_array_range # UInt -> (enumerable of Vec{NIn, Int})
    if NIn > 1
        # Multi-dimensional outputs will slice along their outermost axis.
        n_slices = size(array_bounds)[NIn]
        b_min_slice = Vec(i -> b_min[i], Val(NIn - 1))
        b_max_slice = Vec(i -> b_max[i], Val(NIn - 1))
        get_slice_array_range = slice::UInt -> Iterators.map(b_min_slice:b_max_slice) do within_slice
            return vappend(within_slice, slice)::Vec{NIn, UInt}
        end
    else
        # Single-dimensional outputs will slice along that single axis.
        n_slices = (b_length + threaded_1D_slice_size - 1) ÷ threaded_1D_slice_size
        get_slice_array_range = slice::UInt -> begin
            idx_min = ((slice - 1) * threaded_1D_slice_size) + 1
            idx_max = idx_min + threaded_1D_slice_size - 1
            idx_min += b_min.x
            idx_max += b_min.x
            idx_max = min(idx_max, b_max.x)
            return Iterators.map(Vec, UInt(idx_min):UInt(idx_max))
        end
    end

    # Proces the slices, either single-threaded or multi-threaded.
    function process_slice(i::UInt)
        #TODO: Use broadcast operations; requires defining Vec indexing of arrays more properly
        for posI in get_slice_array_range(i)
            posF = Vec(c -> grid_pos_to_sample_pos(posI[c], c), Val(NIn))
            array[posI] = get_field(field, posF, prep_data)
        end
        return nothing
    end
    if use_threading
        Threads.@threads for i::UInt in 1:n_slices
            process_slice(i)
        end
    else
        for i::UInt in 1:n_slices
            process_slice(i)
        end
    end

    return nothing
end
"
Creates and fills a grid using the given field.
Optional arguments are the same as `sample_field!()`.
"
function sample_field( grid_size::Vec{NIn, <:Integer},
                       field::AbstractField{NIn, NOut, F}
                       ;
                       kw...
                     )::Array{Vec{NOut, F}, NIn} where {NIn, NOut, F}
    output = Array{Vec{NOut, F}, NIn}(undef, grid_size.data)
    sample_field!(output, field; kw...)
    return output
end

export sample_field!, sample_field