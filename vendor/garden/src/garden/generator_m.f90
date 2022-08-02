module garden_generator_m
    use garden_generated_m, only: generated_t
    use garden_input_m, only: input_t
    use garden_shrink_result_m, only: shrink_result_t

    implicit none
    private
    public :: generator_t

    type, abstract :: generator_t
    contains
        private
        procedure(generate_i), public, deferred :: generate
        procedure(shrink_i), nopass, public, deferred :: shrink
    end type

    abstract interface
        function generate_i(self) result(generated_value)
            import :: generated_t, generator_t

            implicit none

            class(generator_t), intent(in) :: self
            type(generated_t) :: generated_value
        end function

        function shrink_i(input) result(shrunk)
            import :: input_t, shrink_result_t

            implicit none

            class(input_t), intent(in) :: input
            type(shrink_result_t) :: shrunk
        end function
    end interface
end module
