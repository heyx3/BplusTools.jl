BplusTools.ECS.PRINT_COMPONENT_CODE = open("test.txt", "w")

world = World()
en = add_entity(world)

# Define "A" components to test configurables.
@component A {abstract} begin
    i::Int
    @configurable modify_i() = (this.i *= 5)
    @configurable modify_i_2(j) = (this.i -= j)
    @configurable set_from_type(::Type{J}) where {J<:Integer} = begin
        this.i = typemin(J)
    end
end
@component Aa <: A begin
    set_from_type(J) = (this.i = typemax(J))
end
@component Ab{j} <: A begin
    floats::NTuple{j, Float32}
    modify_i() = (this.i += j)
end
@component Ac{I<:Integer} <: A begin
    modify_i()::I = begin # Promises a return value that the original configurable didn't
        this.i = typemax(I)
        return convert(I, this.i)
    end
    set_from_type(::Type{J}) where {J} = (this.i = typemax(J) ÷ J(2))
end
@component Ad{b} <: A begin
    modify_i() = (b && SUPER())
    modify_i_2(j) = if b
        # Call with same arguments
        SUPER()
    else
        # Call with custom arguments
        SUPER(10)
    end
end
c_Aa = add_component(en, Aa, 3)
c_Ab = add_component(en, Ab{2}, (-6, -66), 6)
c_Ac = add_component(en, Ac{UInt8}, 9)
c_Ad_yes = add_component(en, Ad{true}, 12)
c_Ad_no = add_component(en, Ad{false}, 15)
@bp_check(c_Aa.i == 3, c_Aa)
@bp_check(c_Ab.i == 6, c_Ab)
@bp_check(c_Ac.i == 9, c_Ac)
@bp_check(c_Ad_yes.i == 12, c_Ad_yes)
@bp_check(c_Ad_no.i == 15, c_Ad_no)
c_Aa.modify_i()
c_Ab.modify_i()
let result = c_Ac.modify_i()
    @bp_check(result === 0xff, c_Ac, " led to ", result)
end
c_Ad_yes.modify_i()
c_Ad_no.modify_i()
@bp_check(c_Aa.i == 3*5, c_Aa)
@bp_check(c_Ab.i == 6+2, c_Ab)
@bp_check(c_Ac.i == typemax(UInt8), c_Ac)
@bp_check(c_Ad_yes.i == 12*5, c_Ad_yes)
@bp_check(c_Ad_no.i == 15, c_Ad_no)
c_Aa.modify_i_2(3)
c_Ab.modify_i_2(4)
c_Ac.modify_i_2(5)
c_Ad_yes.modify_i_2(6)
c_Ad_no.modify_i_2(7)
@bp_check(c_Aa.i == (3*5)-3, c_Aa)
@bp_check(c_Ab.i == (6+2)-4, c_Ab)
@bp_check(c_Ac.i == (typemax(UInt8)-5), c_Ac)
@bp_check(c_Ad_yes.i == (12*5)-6, c_Ad_yes)
@bp_check(c_Ad_no.i == (15)-10, c_Ad_no)
c_Aa.set_from_type(UInt16)
c_Ab.set_from_type(UInt16)
c_Ac.set_from_type(UInt16)
c_Ad_yes.set_from_type(UInt16)
c_Ad_no.set_from_type(UInt16)
@bp_check(c_Aa.i == typemax(UInt16), c_Aa)
@bp_check(c_Ab.i == typemin(UInt16), c_Ab)
@bp_check(c_Ac.i == typemax(UInt16) ÷ 2, c_Ac)
@bp_check(c_Ad_yes.i == typemin(UInt16), c_Ad_yes)
@bp_check(c_Ad_no.i == typemin(UInt16), c_Ad_no)

# Define "B" components to test promises, and builtin functions.
@component B {abstract} begin
    str::String

    DEFAULT() = Ba{Float64}(5.4, "abcd")

    "Sets the `str` field to a description of this component"
    @promise stringify_self()::Nothing

    TICK() = (this.str *= ".")
    DESTRUCT() = (this.str = "parent")
end
@component Ba{T} <: B begin
    t::T
    stringify_self() = (this.str = string(this.t))
    TICK() = (this.str *= string(T))
end
@component Bb{T} <: B begin
    t::T
    CONSTRUCT(t, i) = begin
        SUPER(i)
        (this.t = convert(T, t))
    end
    stringify_self() = (this.str = "b$T")
    DESTRUCT() = begin
        this.str = string(T) # Should get oerridden by parent shutdown
        this.t = zero(T)
    end
end
c_B = add_component(en, B)
c_Ba = add_component(en, Ba{UInt8}, 5, "uint8")
c_Bb = add_component(en, Bb{v3f}, v3f(3, 4, 5), "hmm")
@bp_check(c_B isa Ba{Float64}, c_B)
@bp_check(c_B.t == 5.4, c_B)
@bp_check(c_B.str == "abcd", c_B)
@bp_check(c_Ba.t == 5, c_Ba)
@bp_check(c_Ba.str == "uint8", c_Ba)
@bp_check(c_Bb.t == v3f(3, 4, 5), c_Bb)
@bp_check(c_Bb.str == "hmm", c_Bb)
c_B.stringify_self()
c_Ba.stringify_self()
c_Bb.stringify_self()
@bp_check(c_B.t == 5.4, c_B)
@bp_check(c_B.str == string(5.4), c_B)
@bp_check(c_Ba.t == 5, c_Ba)
@bp_check(c_Ba.str == string(5), c_Ba)
@bp_check(c_Bb.t == v3f(3, 4, 5), c_Bb)
@bp_check(c_Bb.str == "b$v3f", c_Bb)

# Test TICK().
tick_world(world, 0.1f0)
@bp_check(c_B.str == "$(5.4).$Float64", c_B)
@bp_check(c_Ba.str == "$(5).$UInt8", c_Ba)
@bp_check(c_Bb.str == "b$v3f.", c_Bb)

# Test DESTRUCT().
remove_entity(world, en)
@bp_check(c_B.str == "parent", c_B)
@bp_check(c_Ba.str == "parent", c_Ba)
@bp_check(c_Bb.str == "parent", c_Bb)
@bp_check(c_Bb.t == zero(v3f), c_Bb)
# Add the entity back for future tests.
en = add_entity(world)


# Define "C" components to test a generic parent type.
println("#TODO: Test a @promise that has its own type params")
println("#TODO: Test a child type happening to use the same type param name as its parent type, for a different purpose")
@component C{I<:Integer} {abstract} begin
    i::I
    DEFAULT() = Ca() #TODO: Try adding parameters to DEFAULT
    @configurable con(j)::Nothing = (this.i += convert(I, j))
    @promise get()::I
end
@component Ca <: C{Int16} begin
    CONSTRUCT(i = -16) = SUPER(Int16(i))
    get() = -this.i
end
@component Cb{J} <: C{J} begin
    get() = this.i
    con(j) = (this.i -= j)
end
@bp_check_throws(add_component(en, C{Int64}), "Default `C` is a C{Int16}")
c_C = add_component(en, C{Int16})
c_Ca = add_component(en, Ca, 256)
c_Cb = add_component(en, Cb{UInt32}, 0x3456)
@bp_check(c_C isa Ca, c_C)
@bp_check(c_C.i === Int16(-16), c_C)
@bp_check(c_Ca.i === Int16(256), c_Ca)
@bp_check(c_Cb.i === UInt32(0x3456))
@bp_check(c_C.get() === Int16(16), c_C.get())
@bp_check(c_Ca.get() === Int16(-256), c_Ca.get())
@bp_check(c_Cb.get() === UInt32(0x3456), c_Cb.get())
c_C.con(3)
c_Ca.con(4)
c_Cb.con(5)
@bp_check(c_C.i === Int16(-13), c_C)
@bp_check(c_Ca.i === Int16(260), c_Ca)
@bp_check(c_Cb.i === UInt32(0x3456) - UInt32(5), c_Cb)

#TODO: Test generic requirements, like {require: C{I}}